use std::cmp::{max, min};

use crate::fen::{FenError, STARTPOS_FEN};

/// Notation functions specific to the position0x88 setup.
use super::{
    square_index, Colour, Piece, PieceType, Position, SquareIndex, BISHOP, BLACK, COLOUR_BIT_MASK,
    EMPTY, KING, KNIGHT, PAWN, QUEEN, ROOK, WHITE, CastlingRights, BoardSide, MoveUndo, piece_colour, get_piece, piece_type, rank, opposite_colour,
};

use regex::Regex;

pub const H_ROOK_HOME_SQUARES: [SquareIndex; 2] = [0x07, 0x77];
pub const A_ROOK_HOME_SQUARES: [SquareIndex; 2] = [0x00, 0x70];
pub const KING_HOME_SQUARES: [SquareIndex; 2] = [0x04, 0x74];

pub fn set_from_fen(position: &mut Position, fen: &str) -> Result<(), FenError> {
    // Check count first as it uses up the iterator.
    let cnt = fen.split_ascii_whitespace().count();
    if cnt != 6 {
        return Err(FenError::IncorrectNumberOfParts);
    }

    let mut parts = fen.split_ascii_whitespace();
    let piece_placements = parts.next().unwrap();

    if let Err(e) = set_piece_placements(position, piece_placements) {
        return Err(e);
    }

    let side_to_move = parts.next().unwrap();
    if side_to_move.len() != 1 {
        return Err(FenError::InvalidColourToMove);
    }
    match char_to_colour(side_to_move.chars().next().unwrap()) {
        None => return Err(FenError::InvalidColourToMove),
        Some(side) => position.side_to_move = side
    }

    let re = Regex::new("^([KQkq]+|-)$").unwrap();
    let castling_chars = parts.next().unwrap();
    if !re.is_match(castling_chars) {
        return Err(FenError::InvalidCastlingInfo);
    }

    position.castling_rights = CastlingRights::new();

    if castling_chars != "-" {
        for c in castling_chars.chars() {
            let colour = match c.to_ascii_lowercase() == c {
                true => BLACK,
                false => WHITE,
            };
            let side = match c.to_ascii_lowercase() {
                'k' => BoardSide::Kingside,
                'q' => BoardSide::Queenside,
                _ => return Err(FenError::InvalidCastlingInfo)
            };
            position.castling_rights.allow(colour, Some(side));
            
        }
    }
    

    let ep_square_str = parts.next().unwrap();
    let ep_square = match ep_square_str {
        "-" => 0,
        x => {
            let ep = str_to_square_index(x);
            match ep {
                Some(sq) => sq,
                None => return Err(FenError::InvalidSquare)
            }
        }
    };

    position.ep_square = ep_square;

    let halfmove_str = parts.next().unwrap();

    match halfmove_str.parse::<u32>() {
        Err(_e) => return Err(FenError::InvalidDigit),
        Ok(halfmove) => position.halfmove_clock = halfmove
    }

    let fullmove_str = parts.next().unwrap();
    match fullmove_str.parse::<u32>() {
        Err(_e) => return Err(FenError::InvalidDigit),
        Ok(fullmove) => position.fullmove_number = fullmove
    }

    Ok(())
}

pub fn set_piece_placements(
    position: &mut Position,
    piece_placements: &str,
) -> Result<(), FenError> {
    if piece_placements.split('/').count() != 8 {
        return Err(FenError::IncorrectNumberOfParts);
    }

    let piece_placement_lines = piece_placements.split('/');

    let mut line_start_index = 0x80;

    for line in piece_placement_lines {
        line_start_index -= 0x10;
        let mut current_index = line_start_index;
        for chr in line.chars() {
            if chr.is_numeric() {
                let skip_num = chr.to_digit(10);
                match skip_num {
                    Some(empty_count) => {
                        if empty_count > 8 {
                            return Err(FenError::InvalidDigit);
                        }
                        for _ in 0..empty_count {
                            position.set_square_to_piece(current_index, EMPTY);
                            current_index += 1;
                        }
                    }
                    None => return Err(FenError::InvalidDigit),
                }
            } else {
                let piece_result = char_to_piece(chr);
                match piece_result {
                    Some(piece) => {
                        position.set_square_to_piece(current_index, piece);
                        current_index += 1;
                    }
                    None => return Err(FenError::InvalidPiece),
                }
            }
        }
        if current_index & 0xF != 8 {
            return Err(FenError::InvalidLine);
        }
    }

    Ok(())
}

pub fn set_startpos(position: &mut Position) {
    set_from_fen(position, STARTPOS_FEN).unwrap();
}

pub fn make_moves(position: &mut Position, moves: &Vec<LongAlgebraicNotationMove>) {
    for mv in moves {
        let from = mv.from_square();
        let to = mv.to_square();
        let queening_piece = mv.queening_piece();
        make_move(position, from, to, queening_piece)
    }
}

pub fn make_move(position: &mut Position, from_index: SquareIndex, to_index: SquareIndex, queening_piece: Option<PieceType>) {
    let captured_piece = position.squares[to_index];
    let undo = MoveUndo {
        from_index,
        to_index,
        captured_piece,
        castling_rights: position.castling_rights,
        ep_square: position.ep_square,
        halfmove_clock: position.halfmove_clock,
    };
    position.undo_stack.push(undo);

    let ep_square = position.ep_square;
    position.ep_square = 0;

    let moved_piece_colour = piece_colour(position.squares[from_index]).unwrap();

    match queening_piece {
        None => position.squares[to_index] = position.squares[from_index],
        Some(piece_type) => {
            position.squares[to_index] = get_piece(piece_type, moved_piece_colour);
        }
    }

    let moved_piece_type = piece_type(position.squares[from_index]);

    let is_pawn_move = moved_piece_type == PAWN;

    if is_pawn_move {
        
        let diff = from_index.abs_diff(to_index);
        if diff == 32 {
            let from_rank = rank(from_index);
            if from_rank == 1 {
                position.ep_square = square_index(from_rank, 2);
            } else if from_rank == 6 {
                position.ep_square =square_index(from_rank, 5);
            } else {
                panic!("Weird e.p. square!");
            }
        }

        if to_index == ep_square {
            
            let captured_pawn_square = if moved_piece_colour == WHITE {
                from_index - 16
            } else {
                from_index + 16
            };
            let captured_pawn = position.squares[captured_pawn_square];
            match piece_colour(captured_pawn) {
                None => panic!("Not a piece"),
                Some(colour) => {
                    if colour != opposite_colour(moved_piece_colour)
                        || piece_type(captured_pawn) != PAWN
                    {
                        panic!("Invalid e.p. capture piece");
                    }
                    position.set_square_to_piece(captured_pawn_square, EMPTY);
                }
            }
        }
    } else if moved_piece_type == KING {

        if from_index == KING_HOME_SQUARES[moved_piece_colour as SquareIndex] {
            position.castling_rights.remove(moved_piece_colour, None);
        }

        // Castling.
        let diff = max(from_index, to_index) - min(from_index, to_index);
        if diff == 2 {
            let rook_square = if from_index < to_index {
                H_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex]
            } else {
                A_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex]
            };
            let rook = position.squares[rook_square];
            if piece_colour(rook).unwrap() != moved_piece_colour || piece_type(rook) != ROOK {
                panic!("Not a rook");
            }
            let rook_to = (from_index + to_index) / 2; // Avoid negative numbers.
            position.squares[rook_to] = rook;
            position.squares[rook_square] = EMPTY;
        }

        position.king_squares[moved_piece_colour as SquareIndex] = to_index;
        
    } else if moved_piece_type == ROOK {
        if from_index == A_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex] {
            position.castling_rights
                .remove(moved_piece_colour, Some(BoardSide::Queenside));
        } else if from_index == H_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex] {
            position.castling_rights
                .remove(moved_piece_colour, Some(BoardSide::Kingside));
        }
    }

    // Reset the half move clock for pawn moves or captures.
    if captured_piece != EMPTY || is_pawn_move {
        position.halfmove_clock = 0;
    }

    position.squares[from_index] = EMPTY;

    position.side_to_move = opposite_colour(position.side_to_move);
}

#[derive(Debug)]
pub struct LongAlgebraicNotationMove {
    pub text: String,
}

#[derive(Debug)]
pub enum LongAlgebraicNotationError {
    InvalidMove
}

impl LongAlgebraicNotationMove {
    pub fn from_text(text: String) -> Result<LongAlgebraicNotationMove, LongAlgebraicNotationError> {
        if validate_long_algebraic_move(&text) {
            Ok(LongAlgebraicNotationMove { text })
        } else {
            Err(LongAlgebraicNotationError::InvalidMove)
        }
    }

    pub fn from_square(&self) -> SquareIndex {
        str_to_square_index(&self.text[0..=1]).unwrap()
    }

    pub fn to_square(&self) -> SquareIndex {
        str_to_square_index(&self.text[2..=3]).unwrap()
    }

    pub fn queening_piece(&self) -> Option<PieceType> {
        let chars: Vec<char> = self.text.chars().collect();
        let q = chars.get(4);
        match q {
            None => None,
            Some(i) => char_to_piece_type(*i),
        }
    }
}

pub fn char_to_piece_type(c: char) -> Option<PieceType> {
    match c.to_ascii_lowercase() {
        'p' => Some(PAWN),
        'r' => Some(ROOK),
        'n' => Some(KNIGHT),
        'b' => Some(BISHOP),
        'q' => Some(QUEEN),
        'k' => Some(KING),
        _ => None,
    }
}

pub fn piece_type_to_char(p: PieceType) -> Option<char> {
    match p {
        EMPTY => Some(' '),
        PAWN => Some('p'),
        ROOK => Some('r'),
        KNIGHT => Some('n'),
        BISHOP => Some('b'),
        QUEEN => Some('q'),
        KING => Some('k'),
        _ => None,
    }
}

pub fn char_to_colour(c: char) -> Option<Colour> {
    match c.to_ascii_lowercase() {
        'b' => Some(BLACK),
        'w' => Some(WHITE),
        _ => None,
    }
}

pub fn str_to_square_index(s: &str) -> Option<SquareIndex> {
    let lower = s.to_ascii_lowercase();
    let bytes = lower.as_bytes();
    let file = bytes[0];
    let file_no = file - 97;
    let rank = bytes[1];
    let rank_no = rank - 0x31;
    if file_no > 7 || rank_no > 7 {
        None
    } else {
        Some(square_index(rank_no, file_no))
    }
}

pub fn square_index_to_str(index: SquareIndex) -> String {
    let mut result = String::new();
    result.push((super::file(index) + 97) as char);
    result.push((super::rank(index) + 0x31) as char);
    result
}

/// Converts a character to a piece with colour.
///
/// e.g. char_to_piece('n') = black knight
///      char_to_piece('N') = white knight
///
fn char_to_piece(c: char) -> Option<Piece> {
    let piece_type = char_to_piece_type(c);

    match piece_type {
        None => None,
        Some(pt) => {
            let is_black = c == c.to_ascii_lowercase();

            if is_black {
                Some(pt | COLOUR_BIT_MASK)
            } else {
                Some(pt)
            }
        }
    }
}

pub fn piece_to_char(p: Piece) -> Option<char> {
    let piece_type = p & 0xF;
    let result = piece_type_to_char(piece_type);
    match result {
        None => None,
        Some(chr) => {
            if p & COLOUR_BIT_MASK == 0 {
                Some(chr.to_ascii_uppercase())
            } else {
                Some(chr.to_ascii_lowercase())
            }
        }
    }
}

pub fn validate_long_algebraic_move(text: &str) -> bool {
    let re = Regex::new("^([a-h][1-8]){2}[rnbq]?$").unwrap();
    re.is_match(text)
}

#[cfg(test)]
mod test {

    use crate::fen::STARTPOS_FEN;

    use super::*;

    #[test]
    fn test_char_to_piece_type() {
        assert_eq!(PAWN, char_to_piece_type('p').unwrap());
        assert_eq!(PAWN, char_to_piece_type('P').unwrap());
        assert_eq!(None, char_to_piece_type('x'));
    }

    #[test]
    fn test_piece_type_to_char() {
        assert_eq!('p', piece_type_to_char(PAWN).unwrap());
        assert_eq!(None, piece_type_to_char(56));
    }

    #[test]
    fn test_char_to_colour() {
        assert_eq!(BLACK, char_to_colour('b').unwrap());
        assert_eq!(WHITE, char_to_colour('w').unwrap());
        assert_eq!(None, char_to_colour('F'));
    }

    #[test]
    fn test_str_to_square_index() {
        assert_eq!(0x44, str_to_square_index("e5").unwrap());
        assert_eq!(0x44, str_to_square_index("E5").unwrap());
        assert_eq!(None, str_to_square_index("X20"));
    }

    #[test]
    fn test_char_to_piece() {
        assert_eq!(ROOK, char_to_piece('R').unwrap());
        assert_eq!(ROOK | COLOUR_BIT_MASK, char_to_piece('r').unwrap());
        assert_eq!(None, char_to_piece('Z'));
    }

    #[test]
    fn test_piece_to_char() {
        assert_eq!('R', piece_to_char(ROOK).unwrap());
        assert_eq!('n', piece_to_char(KNIGHT | COLOUR_BIT_MASK).unwrap());
        assert_eq!(None, piece_to_char(234));
    }

    #[test]
    fn test_validate_long_algebraic_move() {
        assert!(validate_long_algebraic_move("e2e4"));
        assert!(validate_long_algebraic_move("e7e8q"));
        assert!(!validate_long_algebraic_move("text"));
    }

    #[test]
    fn test_set_piece_placements() {
        let mut position = Position::default();
        set_piece_placements(&mut position, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
            .expect("Failed");
        assert_eq!(ROOK, position.squares[0x00]);
        assert_eq!(0x74, position.king_squares[BLACK as usize]);
        assert_eq!(0x04, position.king_squares[WHITE as usize]);
    }

    #[test]
    fn test_set_from_fen() {
        let mut position = Position::default();
        set_from_fen(&mut position, STARTPOS_FEN).expect("Failed");
        assert_eq!(ROOK, position.squares[0x00]);
        assert_eq!(0x74, position.king_squares[BLACK as usize]);
        assert_eq!(0x04, position.king_squares[WHITE as usize]);
        assert!(position.castling_rights.allowed(WHITE, BoardSide::Kingside));
        assert!(position.castling_rights.allowed(WHITE, BoardSide::Queenside));
        assert!(position.castling_rights.allowed(BLACK, BoardSide::Kingside));
        assert!(position.castling_rights.allowed(BLACK, BoardSide::Queenside));
        assert_eq!(0, position.ep_square);
        assert_eq!(WHITE, position.side_to_move);
        assert_eq!(0, position.halfmove_clock);
        assert_eq!(1, position.fullmove_number);
    }

    #[test]
    fn test_long_algebraic_notation_move() {
        let mv1 = LongAlgebraicNotationMove::from_text("e2e4".to_string()).unwrap();
        assert_eq!(0x14, mv1.from_square());
        assert_eq!(0x34, mv1.to_square());
    }
}
