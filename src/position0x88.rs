use std::fmt::Debug;

use self::notation::piece_to_char;

pub mod notation;

/// The type of a piece, without colour, e.g. knight, bishop etc.
pub type PieceType = u8;

/// The colour of a piece.
pub type Colour = u8;

/// A piece with colour, e.g. a black knight.
pub type Piece = u8;

// Colours.
pub const WHITE: Colour = 0;
pub const BLACK: Colour = 1;

pub const COLOUR_BIT: u8 = 5;
pub const COLOUR_BIT_MASK: u8 = 1 << COLOUR_BIT;

pub const EMPTY: PieceType = 0;
pub const PAWN: PieceType = 1;
pub const ROOK: PieceType = 2;
pub const KNIGHT: PieceType = 3;
pub const BISHOP: PieceType = 4;
pub const QUEEN: PieceType = 5;
pub const KING: PieceType = 6;

pub type SquareIndex = usize;
pub type RankOrFileIndex = u8;

pub struct Position {
    squares: [PieceType; 128],
    king_squares: [SquareIndex; 2],
    side_to_move: Colour,
    castling_rights: CastlingRights,
    ep_square: SquareIndex,
    halfmove_clock: u32,
    fullmove_number: u32,
    undo_stack: Vec<MoveUndo>
}

impl Position {
    pub fn set_square_to_piece(&mut self, square: SquareIndex, piece: Piece) {
        self.squares[square] = piece;
        if piece_type(piece) == KING {
            self.king_squares[piece_colour(piece).unwrap() as usize] = square;
        }
    }

    
}

impl Debug for Position {
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

impl Default for Position {
    fn default() -> Self {
        Position {
            squares: [0; 128],
            king_squares: [0; 2],
            side_to_move: WHITE,
            castling_rights: CastlingRights::new(),
            ep_square: 0,
            halfmove_clock: 0,
            fullmove_number: 0,
            undo_stack: vec![]
        }
    }
}

struct MoveUndo {
    from_index: SquareIndex,
    to_index: SquareIndex,
    captured_piece: Piece,
    ep_square: SquareIndex,
    halfmove_clock: u32,
    castling_rights: CastlingRights,
}

pub enum BoardSide {
    Queenside,
    Kingside,
}

#[derive(Debug, Clone, Copy)]
pub struct CastlingRights {
    pub flags: u8, // KQkq
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

    pub fn allow(&mut self, colour: Colour, side: Option<BoardSide>) {
        let mask = Self::mask(colour, side);
        self.flags |= mask;
    }

    #[inline]
    fn mask(colour: Colour, side: Option<BoardSide>) -> u8 {
        let colour_shift = 2 * (1 - colour);
        let mask = match side {
            None => 0b11,
            Some(castling_side) => 1 << castling_side as u32,
        };
        mask << colour_shift
    }
}

#[inline]
fn file(square: SquareIndex) -> RankOrFileIndex {
    square as u8 & 7
}

#[inline]
fn rank(square: SquareIndex) -> RankOrFileIndex {
    square as u8 >> 4
}

#[inline]
fn square_index(rank: RankOrFileIndex, file: RankOrFileIndex) -> SquareIndex {
    debug_assert!(rank < 8);
    debug_assert!(file < 8);
    ((rank as usize) << 4) | file as usize
}

#[inline]
pub fn get_piece(piece_type: PieceType, colour: Colour) -> Piece {
    piece_type | (colour << COLOUR_BIT)
}

#[inline]
pub fn piece_type(piece: Piece) -> PieceType {
    piece & 0xF
}

pub fn piece_colour(piece: Piece) -> Option<Colour> {
    if piece == EMPTY {
        None
    } else if (piece & COLOUR_BIT_MASK) != 0 {
        Some(BLACK)
    } else {
        Some(WHITE)
    }
}

#[inline]
pub fn opposite_colour(colour: Colour) -> Colour {
    1 - colour
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_file() {
        assert_eq!(0, file(0x00));
        assert_eq!(4, file(0x64));
    }

    #[test]
    fn test_rank() {
        assert_eq!(0, rank(0x00));
        assert_eq!(4, rank(0x42));
    }

    #[test]
    fn test_square_index() {
        assert_eq!(0x42, square_index(4, 2));
    }

    #[test]
    fn test_get_piece() {
        assert_eq!(ROOK, get_piece(ROOK, WHITE));
        assert_eq!(COLOUR_BIT_MASK | ROOK, get_piece(ROOK, BLACK));
    }

    #[test]
    fn test_piece_type() {
        assert_eq!(PAWN, piece_type(COLOUR_BIT_MASK | PAWN));
    }

    #[test]
    fn test_piece_colour() {
        assert_eq!(WHITE, piece_colour(PAWN).unwrap());
        assert_eq!(BLACK, piece_colour(PAWN | COLOUR_BIT_MASK).unwrap());
        assert_eq!(None, piece_colour(EMPTY));
    }

    #[test]
    fn test_opposite_colour() {
        assert_eq!(WHITE, opposite_colour(BLACK));
        assert_eq!(BLACK, opposite_colour(WHITE));
    }

    #[test]
    fn test_castling_rights() {
        let mut castling = CastlingRights::new();
        assert!(!castling.allowed(WHITE, BoardSide::Kingside));
        assert!(!castling.allowed(BLACK, BoardSide::Queenside));
        castling.allow(WHITE, None);
        assert!(castling.allowed(WHITE, BoardSide::Kingside));
        assert!(castling.allowed(WHITE, BoardSide::Queenside));
        assert!(!castling.allowed(BLACK, BoardSide::Queenside));
        castling.remove(WHITE, Some(BoardSide::Kingside));
        assert!(!castling.allowed(WHITE, BoardSide::Kingside));
        assert!(castling.allowed(WHITE, BoardSide::Queenside));
        assert!(!castling.allowed(BLACK, BoardSide::Queenside));
        castling.allow(BLACK, Some(BoardSide::Kingside));
        assert!(!castling.allowed(WHITE, BoardSide::Kingside));
        assert!(castling.allowed(WHITE, BoardSide::Queenside));
        assert!(!castling.allowed(BLACK, BoardSide::Queenside));
        assert!(castling.allowed(BLACK, BoardSide::Kingside));
    }

}