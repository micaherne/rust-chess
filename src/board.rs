use std::{
    cmp::{max, min},
    fmt::Debug,
};

use regex::Regex;

use crate::fen::{parse_fen, Fen};

pub struct Board {
    pub squares: [u32; 128],
    pub king_squares: [u32; 2],
    pub side_to_move: Colour,
    pub castling: CastlingRights,
    pub ep_square: Square,
    pub halfmove_clock: u32,
    pub fullmove_number: u32,
    undo_stack: Vec<MoveUndo>,
}

#[derive(Debug, Clone, Copy)]
pub struct CastlingRights {
    pub flags: u32, // KQkq
}

impl CastlingRights {
    pub fn new() -> CastlingRights {
        CastlingRights { flags: 0 }
    }

    pub fn remove(&mut self, colour: Colour, side: Option<BoardSide>) {
        let mask = Self::mask(colour, side);
        self.flags = self.flags & !(mask);
    }

    pub fn allowed(&self, colour: Colour, side: BoardSide) -> bool {
        let mask = Self::mask(colour, Some(side));
        self.flags & mask != 0
    }

    pub fn allow(&mut self, colour: Colour, side: BoardSide) {
        let mask = Self::mask(colour, Some(side));
        self.flags |= mask;
    }

    #[inline]
    fn mask(colour: Colour, side: Option<BoardSide>) -> u32 {
        let colour_shift = 2 * (1 - colour);
        let mask = match side {
            None => 0b11,
            Some(castling_side) => 1 << castling_side as u32,
        };
        mask << colour_shift
    }
}

/// The type of a piece, without colour, e.g. knight, bishop etc.
pub type PieceType = u32;

/// The colour of a piece.
pub type Colour = u32;

/// A piece with colour, e.g. a black knight.
pub type Piece = u32;

pub type SquareIndex = usize;

pub const OFFSETS_DIAGONAL: [i8; 4] = [17, -15, -17, 15];
pub const OFFSETS_LINEAR: [i8; 4] = [16, 1, -16, -1];


#[derive(PartialEq)]
pub enum File {
    A,
    B,
    C,
    D,
    E,
    F,
    H,
}

#[derive(PartialEq)]
pub enum Rank {
    One,
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
}

pub enum BoardSide {
    Queenside,
    Kingside,
}

// Colours.
pub const WHITE: Colour = 0;
pub const BLACK: Colour = 1;

pub const EMPTY: PieceType = 0;
pub const PAWN: PieceType = 1;
pub const ROOK: PieceType = 2;
pub const KNIGHT: PieceType = 3;
pub const BISHOP: PieceType = 4;
pub const QUEEN: PieceType = 5;
pub const KING: PieceType = 6;

// Bit to be set for black pieces;
const BLACK_BIT: u32 = 1 << 5;

const H_ROOK_HOME_SQUARES: [SquareIndex; 2] = [0x07, 0x77];
const A_ROOK_HOME_SQUARES: [SquareIndex; 2] = [0x00, 0x70];
const KING_HOME_SQUARES: [SquareIndex; 2] = [0x04, 0x74];

const STARTPOS_FEN: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

#[inline]
pub fn get_piece(piece_type: PieceType, colour: Colour) -> Piece {
    piece_type | (colour << 5)
}

#[inline]
pub fn piece_type(piece: Piece) -> PieceType {
    piece & 0xF
}

pub fn piece_colour(piece: Piece) -> Option<Colour> {
    if piece == EMPTY {
        None
    } else if (piece & BLACK_BIT) != 0 {
        Some(BLACK)
    } else {
        Some(WHITE)
    }
}

#[inline]
pub fn opposite_colour(colour: Colour) -> Colour {
    1 - colour
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



/// Converts a character to a piece with colour.
///
/// e.g. char_to_piece('n') = black knight
///      char_to_piece('N') = white knight
///
fn char_to_piece(c: char) -> Option<u32> {
    let piece_type = char_to_piece_type(c);

    match piece_type {
        None => None,
        Some(pt) => {
            let is_black = c == c.to_ascii_lowercase();

            if is_black {
                Some(pt | BLACK_BIT)
            } else {
                Some(pt)
            }
        }
    }
}

fn piece_to_char(p: Piece) -> Option<char> {
    let piece_type = p & 0xF;
    let result = piece_type_to_char(piece_type);
    match result {
        None => None,
        Some(chr) => {
            if p & BLACK_BIT == 0 {
                Some(chr.to_ascii_uppercase())
            } else {
                Some(chr.to_ascii_lowercase())
            }
        }
    }
}

#[inline]
pub fn square_index(square: Square) -> SquareIndex {
    (square.0 + 16 * square.1).into()
}

pub fn validate_long_algebraic_move(text: &str) -> bool {
    let re = Regex::new("([a-h][1-8]){2}[rnbq]").unwrap();
    re.is_match(text)
}

impl Board {
    pub fn set_from_fen(&mut self, fen: Fen) {
        self.squares = [EMPTY; 128];

        self.set_pieces_from_fen(&fen.piece_placement).unwrap();
        self.castling = fen.castling;
        self.ep_square = Square(fen.ep_square.0, fen.ep_square.1);
        self.halfmove_clock = fen.halfmove_clock;
        self.fullmove_number = fen.fullmove_number;
    }

    pub fn set_startpos(&mut self) {
        self.set_from_fen(parse_fen(STARTPOS_FEN).unwrap());
    }

    pub fn make_moves(&mut self, moves: &Vec<LongAlgebraicNotationMove>) {
        for mv in moves {
            let from = mv.from_square();
            let to = mv.to_square();
            let queening_piece = mv.queening_piece();
            self.make_move(from, to, queening_piece)
        }
    }

    pub fn make_move(&mut self, from: Square, to: Square, queening_piece: Option<PieceType>) {
        let from_index = square_index(from);
        let to_index = square_index(to);
        let captured_piece = self.squares[to_index];
        let undo = MoveUndo {
            from,
            to,
            captured_piece,
            castling: self.castling,
            ep_square: self.ep_square,
            halfmove_clock: self.halfmove_clock,
        };
        self.undo_stack.push(undo);

        let ep_square = self.ep_square;
        self.ep_square = Square(0, 0);

        let moved_piece_colour = piece_colour(self.squares[from_index]).unwrap();

        match queening_piece {
            None => self.squares[to_index] = self.squares[from_index],
            Some(piece_type) => {
                self.squares[to_index] = get_piece(piece_type, moved_piece_colour);
            }
        }

        let moved_piece_type = piece_type(self.squares[from_index]);

        let is_pawn_move = moved_piece_type == PAWN;

        if is_pawn_move {
            let diff = max(from_index, to_index) - min(from_index, to_index);
            if diff == 32 {
                if from.1 == 1 {
                    self.ep_square = Square(from.0, 2);
                } else if from.1 == 6 {
                    self.ep_square = Square(from.0, 5);
                } else {
                    panic!("Weird e.p. square!");
                }
            }

            if to == ep_square {
                let captured_pawn_square = if moved_piece_colour == WHITE {
                    Square(ep_square.0, ep_square.1 - 1)
                } else {
                    Square(ep_square.0, ep_square.1 + 1)
                };
                let captured_pawn_square_index = square_index(captured_pawn_square);
                let captured_pawn = self.squares[captured_pawn_square_index];
                match piece_colour(captured_pawn) {
                    None => panic!("Not a piece"),
                    Some(colour) => {
                        if colour != opposite_colour(moved_piece_colour)
                            || piece_type(captured_pawn) != PAWN
                        {
                            panic!("Invalid e.p. capture piece");
                        }
                        self.squares[captured_pawn_square_index] = EMPTY;
                    }
                }
            }
        } else if moved_piece_type == KING {
            println!("From: {}", from_index);

            if from_index == KING_HOME_SQUARES[moved_piece_colour as SquareIndex] {
                self.castling.remove(moved_piece_colour, None);
            }

            // Castling.
            let diff = max(from_index, to_index) - min(from_index, to_index);
            if diff == 2 {
                let rook_square = if from_index < to_index {
                    H_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex]
                } else {
                    A_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex]
                };
                let rook = self.squares[rook_square];
                if piece_colour(rook).unwrap() != moved_piece_colour || piece_type(rook) != ROOK {
                    panic!("Not a rook");
                }
                let rook_to = (from_index + to_index) / 2; // Avoid negative numbers.
                self.squares[rook_to] = rook;
                self.squares[rook_square] = EMPTY;
            }
        } else if moved_piece_type == ROOK {
            if from_index == A_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex] {
                self.castling
                    .remove(moved_piece_colour, Some(BoardSide::Queenside));
            } else if from_index == H_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex] {
                self.castling
                    .remove(moved_piece_colour, Some(BoardSide::Kingside));
            }
        }

        // Reset the half move clock for pawn moves or captures.
        if captured_piece != EMPTY || is_pawn_move {
            self.halfmove_clock = 0;
        }

        self.squares[from_index] = EMPTY;

        self.side_to_move = opposite_colour(self.side_to_move);
    }

    fn set_pieces_from_fen(&mut self, fen_part: &str) -> Result<(), &str> {
        let mut i = FenIndexProvider::new();
        for c in fen_part.chars() {
            if c == '/' {
                // Assert that we're at the end of a row.
                continue;
            } else if c.is_numeric() {
                let x = c.to_digit(10).unwrap() as SquareIndex;
                i.skip(x);
                continue;
            }

            let piece = char_to_piece(c);

            match piece {
                Some(val) => {
                    self.squares[i.next()] = val;
                }
                None => return Err("Invalid piece type"),
            }
        }
        Ok(())
    }

}

impl Debug for Board {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        let sep = "+-".repeat(8) + "+\n";
        s.push_str(&sep);
        for i in (0x00..0x80).step_by(0x10).rev() {
            s.push_str("|");
            for sq in self.squares[i..i + 8].iter().map(|s| piece_to_char(*s)) {
                s.push(sq.unwrap());
                s.push_str("|");
            }
            s.push_str("\n");
            s.push_str(&sep);
        }
        writeln!(f, "{}", s)
    }
}

impl Default for Board {
    fn default() -> Self {
        Board {
            squares: [EMPTY; 128],
            king_squares: [0; 2],
            side_to_move: WHITE,
            castling: CastlingRights { flags: 0 },
            ep_square: Square(0, 0),
            halfmove_clock: 0,
            fullmove_number: 0,
            undo_stack: Vec::new(),
        }
    }
}

struct MoveUndo {
    from: Square,
    to: Square,
    captured_piece: Piece,
    ep_square: Square,
    halfmove_clock: u32,
    castling: CastlingRights,
}

struct FenIndexProvider {
    current: SquareIndex,
    valid: bool,
}

// TODO: Make this a proper iterator.
impl FenIndexProvider {
    fn new() -> Self {
        return FenIndexProvider {
            current: 0x70,
            valid: true,
        };
    }

    fn reset(&mut self) {
        self.current = 0x70;
        self.valid = true;
    }

    fn next(&mut self) -> SquareIndex {
        if !self.valid {
            panic!("All squares given")
        }
        let result = self.current;

        self.current += 1;

        if self.current & 0x88 != 0 {
            if self.current < 0x18 {
                self.valid = false;
            } else {
                self.current -= 0x18;
            }
        }
        result
    }

    fn skip(&mut self, count: SquareIndex) {
        if count > 8 {
            panic!("Can't skip more than 8 spaces");
        }

        self.current += count;

        if self.current & 0x88 != 0 {
            self.current -= 0x18;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Square(pub u8, pub u8);

///
/// A move.
///
/// This uses the square abstraction so is only intended for use when parsing notation.
/// Actual generated moves use the Move structure in the movegen crate for speed.
///
pub trait Move {
    fn from_square(&self) -> Square;
    fn to_square(&self) -> Square;
    fn queening_piece(&self) -> Option<Piece>;
}

#[derive(Debug)]
pub struct LongAlgebraicNotationMove {
    pub text: String,
}

impl Move for LongAlgebraicNotationMove {
    fn from_square(&self) -> Square {
        let lower = self.text.to_ascii_lowercase();
        let bytes = lower.as_bytes();
        let file = bytes[0];
        let file_no = file - 97;
        let rank = bytes[1];
        let rank_no = rank - 0x31;
        Square(file_no, rank_no)
    }

    fn to_square(&self) -> Square {
        let lower = self.text.to_ascii_lowercase();
        let bytes = lower.as_bytes();
        let file = bytes[2];
        let file_no = file - 97;
        let rank = bytes[3];
        let rank_no = rank - 0x31;
        Square(file_no, rank_no)
    }
    fn queening_piece(&self) -> Option<PieceType> {
        let chars: Vec<char> = self.text.chars().collect();
        let q = chars.get(4);
        match q {
            None => None,
            Some(i) => char_to_piece_type(*i),
        }
    }
}

impl LongAlgebraicNotationMove {
    pub fn from_text(text: String) -> LongAlgebraicNotationMove {
        return LongAlgebraicNotationMove { text };
    }
}

// enum Piece {
//     Empty = 0,
//     Pawn = 1,
//     Rook = 2,
//     Knight = 3,
//     Bishop = 4,
//     Queen = 5,
//     King = 6
// }

// enum Colour {
//     Black = 0,
//     White = 16
// }

// enum ColouredPiece {
//     Empty = Piece::Empty,
//     BlackPawn = Piece::Pawn | Colour::Black,
//     BlackRook = Piece::Rook | Colour::Black,
//     BlackKnight = Piece::Knight | Colour::Black,
//     BlackBishop = Piece::Bishop | Colour::Black,
//     BlackQueen = Piece::Queen | Colour::Black,
//     BlackKing = Piece::King | Colour::Black,
//     WhitePawn = Piece::Pawn | Colour::White,
//     WhiteRook = Piece::Rook | Colour::White,
//     WhiteKnight = Piece::Knight | Colour::White,
//     WhiteBishop = Piece::Bishop | Colour::White,
//     WhiteQueen = Piece::Queen | Colour::White,
//     WhiteKing = Piece::King | Colour::White,
// }

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_set_pieces_from_fen() {
        let mut b = Board::default();
        b.set_pieces_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR")
            .unwrap();
        assert!(b.squares[0x77] == BLACK_BIT | ROOK);
        assert!(b.squares[0x07] == ROOK);
        assert!(b.squares[0x70] == BLACK_BIT | ROOK);
        assert!(b.squares[0x00] == ROOK);
    }

    #[test]
    fn test_piece_to_char() {
        assert!('P' == piece_to_char(PAWN).unwrap());
        assert!('p' == piece_to_char(PAWN | BLACK_BIT).unwrap());
    }

    #[test]
    fn test_rank() {
        assert!(File::A as u32 == 0);
        assert!(Rank::One as u32 == 0);
    }
}
