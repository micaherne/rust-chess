use crate::{
    fen::{ConsumeFen, Fen, FenError, STARTPOS_FEN},
    position::{SetPosition, SquareIndex},
};

/// Notation functions specific to the position0x88 setup.
use super::{
    movegen_simple::is_valid_square, square_index, BoardSide, CastlingRights, Colour,
    PieceStandard, PieceType, Position0x88, SquareIndex0x88, BISHOP, BLACK, COLOUR_BIT_MASK, EMPTY,
    KING, KNIGHT, PAWN, QUEEN, ROOK, WHITE,
};

use regex::Regex;

pub const H_ROOK_HOME_SQUARES: [SquareIndex0x88; 2] = [0x07, 0x77];
pub const A_ROOK_HOME_SQUARES: [SquareIndex0x88; 2] = [0x00, 0x70];
pub const KING_HOME_SQUARES: [SquareIndex0x88; 2] = [0x04, 0x74];

impl From<&str> for Position0x88 {
    fn from(value: &str) -> Self {
        let mut pos = Position0x88::default();
        set_from_fen(&mut pos, value).unwrap();
        pos
    }
}

impl ConsumeFen for Position0x88 {
    fn set_from_fen(&mut self, fen: Fen) -> Result<(), FenError> {
        if let Err(e) = set_piece_placements(self, &fen.piece_placements) {
            return Err(e);
        }

        match char_to_colour(fen.side_to_move) {
            None => return Err(FenError::InvalidColourToMove(fen.side_to_move.to_string())),
            Some(side) => self.side_to_move = side,
        }

        let re = Regex::new("^([KQkq]+|-)$").unwrap();
        let castling_chars = &fen.castling;
        if !re.is_match(castling_chars) {
            return Err(FenError::InvalidCastlingInfo(castling_chars.to_string()));
        }

        self.castling_rights = CastlingRights::new();

        if castling_chars != "-" {
            for c in castling_chars.chars() {
                let colour = match c.to_ascii_lowercase() == c {
                    true => BLACK,
                    false => WHITE,
                };
                let side = match c.to_ascii_lowercase() {
                    'k' => BoardSide::Kingside,
                    'q' => BoardSide::Queenside,
                    _ => return Err(FenError::InvalidCastlingInfo(castling_chars.to_string())),
                };
                self.castling_allow(colour, Some(side));
            }
        }

        let ep_square_str = fen.ep_square.as_str();
        let ep_square = match ep_square_str {
            "-" => 0,
            x => {
                let ep = str_to_square_index(x);
                match ep {
                    Some(sq) => sq,
                    None => return Err(FenError::InvalidSquare(x.to_string())),
                }
            }
        };

        self.set_ep_square(ep_square);

        match fen.halfmove.parse::<u32>() {
            Err(_e) => return Err(FenError::InvalidDigit(fen.halfmove)),
            Ok(halfmove) => self.halfmove_clock = halfmove,
        }

        match fen.fullmove.parse::<u32>() {
            Err(_e) => return Err(FenError::InvalidDigit(fen.fullmove)),
            Ok(fullmove) => self.fullmove_number = fullmove,
        }

        Ok(())
    }
}

impl Into<Fen> for Position0x88 {
    fn into(self) -> Fen {
        let string = to_fen(&self);
        string.as_str().try_into().unwrap()
    }
}

pub fn to_fen(position: &Position0x88) -> String {
    let mut result = String::new();
    for line_start in (0..8).rev() {
        let mut empty_square_count = 0;
        for i in 0..8 {
            let square_index = (line_start << 4) + i;

            debug_assert!(is_valid_square(square_index as i16));

            let piece = position.squares0x88[square_index];
            if piece == EMPTY {
                empty_square_count += 1;
            } else {
                if empty_square_count > 0 {
                    result.push_str(&empty_square_count.to_string());
                    empty_square_count = 0;
                }
                result.push(piece_to_char(piece).unwrap());
            }
        }
        if empty_square_count > 0 {
            result.push_str(&empty_square_count.to_string());
        }
        if line_start > 0 {
            result.push('/');
        }
    }

    result.push(' ');
    result.push(colour_to_char(position.side_to_move).unwrap_or_else(|| '-'));
    result.push(' ');
    result.push_str(castling_rights_to_string(&position.castling_rights).as_str());
    let mut ep_string = position.ep_square.sq_to_algebraic_notation();
    if ep_string == "a1" {
        ep_string = "-".to_string();
    }
    result.push(' ');
    result.push_str(&ep_string);
    result.push(' ');
    result.push_str(position.halfmove_clock.to_string().as_str());
    result.push(' ');
    result.push_str(position.fullmove_number.to_string().as_str());

    result
}

pub fn set_from_fen(position: &mut Position0x88, fen: &str) -> Result<(), FenError> {
    position.set_from_fen(fen.try_into()?)
}

pub fn set_piece_placements(
    position: &mut Position0x88,
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
                            return Err(FenError::InvalidDigit(chr.to_string()));
                        }
                        for _ in 0..empty_count {
                            position.set_square_to_piece(current_index, EMPTY);
                            current_index += 1;
                        }
                    }
                    None => return Err(FenError::InvalidDigit(chr.to_string())),
                }
            } else {
                let piece_result = char_to_piece(chr);
                match piece_result {
                    Some(piece) => {
                        position.set_square_to_piece(current_index, piece);
                        current_index += 1;
                    }
                    None => return Err(FenError::InvalidPiece(chr)),
                }
            }
        }
        if current_index & 0xF != 8 {
            return Err(FenError::InvalidLine);
        }
    }

    Ok(())
}

pub fn set_startpos(position: &mut Position0x88) {
    set_from_fen(position, STARTPOS_FEN).unwrap();
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

pub fn colour_to_char(colour: Colour) -> Option<char> {
    match colour {
        BLACK => Some('b'),
        WHITE => Some('w'),
        _ => None,
    }
}

pub fn str_to_square_index(s: &str) -> Option<SquareIndex0x88> {
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

#[deprecated(since = "0.1.0", note = "Please use to_algebraic_notation() instead")]
pub fn square_index_to_str(index: SquareIndex0x88) -> String {
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
fn char_to_piece(c: char) -> Option<PieceStandard> {
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

pub fn piece_to_char(p: PieceStandard) -> Option<char> {
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

pub fn castling_rights_to_string(castling: &CastlingRights) -> String {
    let mut result = String::new();
    for (i, c) in "KQkq".chars().enumerate() {
        if castling.flags & (1 << (3 - i)) != 0 {
            result.push(c);
        }
    }
    if result.len() == 0 {
        result.push('-');
    }
    result
}

#[cfg(test)]
mod test {

    use super::*;

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
    fn test_set_piece_placements() {
        let mut position = Position0x88::default();
        set_piece_placements(&mut position, "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
            .expect("Failed");
        assert_eq!(ROOK, position.squares0x88[0x00]);
        assert_eq!(0x74, position.king_squares[BLACK as usize]);
        assert_eq!(0x04, position.king_squares[WHITE as usize]);
    }

    #[test]
    fn test_set_from_fen() {
        let mut position = Position0x88::default();
        set_from_fen(&mut position, STARTPOS_FEN).expect("Failed");
        assert_eq!(ROOK, position.squares0x88[0x00]);
        assert_eq!(0x74, position.king_squares[BLACK as usize]);
        assert_eq!(0x04, position.king_squares[WHITE as usize]);
        assert!(position.castling_rights.allowed(WHITE, BoardSide::Kingside));
        assert!(position
            .castling_rights
            .allowed(WHITE, BoardSide::Queenside));
        assert!(position.castling_rights.allowed(BLACK, BoardSide::Kingside));
        assert!(position
            .castling_rights
            .allowed(BLACK, BoardSide::Queenside));
        assert_eq!(0, position.ep_square);
        assert_eq!(WHITE, position.side_to_move);
        assert_eq!(0, position.halfmove_clock);
        assert_eq!(1, position.fullmove_number);
    }
}
