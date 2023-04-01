use std::fmt::Debug;

use crate::{
    bitboards::{self, square_mask0x88, Bitboard, SquareIndex64},
    fen::FenError,
    position::{
        BoardSide, CastlingRights, MoveUndo, Piece, Position, RankOrFileIndex, SetPosition,
        SquareIndex,
    },
    position0x88::movegen_simple::is_valid_square,
    transposition::{Hashable, ZobristNumber, ZobristNumbers},
};

use self::{
    movegen::{GenerateMoves, Move0x88},
    movegen_simple::{generate_moves, PIECE_TYPES_COUNT},
    notation::{char_to_piece_type, piece_to_char, piece_type_to_char},
};

pub mod evaluate;
pub mod iters;
pub mod make_moves;
pub mod movegen;
pub mod movegen_simple;
pub mod notation;

/// The type of a piece, without colour, e.g. knight, bishop etc.
pub type PieceType = u8;

/// The colour of a piece.
pub type Colour = u8;

/// A piece with colour, e.g. a black knight.
pub type PieceStandard = u8;

pub type MoveUndo0x88 = MoveUndo<SquareIndex0x88, PieceType>;

// Colours.
pub const WHITE: Colour = 0;
pub const BLACK: Colour = 1;
// To simplify bitboards - this is a pseudo-colour for empty squares.
pub const NONE: Colour = 2;

pub const COLOUR_BIT: u8 = 5;
pub const COLOUR_BIT_MASK: u8 = 1 << COLOUR_BIT;

pub const EMPTY: PieceType = 0;
pub const PAWN: PieceType = 1;
pub const ROOK: PieceType = 2;
pub const KNIGHT: PieceType = 3;
pub const BISHOP: PieceType = 4;
pub const QUEEN: PieceType = 5;
pub const KING: PieceType = 6;

pub type SquareIndex0x88 = usize;

impl SquareIndex for SquareIndex0x88 {
    fn sq_to_algebraic_notation(&self) -> String {
        let mut result = String::new();
        result.push((file(*self) + 97) as char);
        result.push((rank(*self) + 0x31) as char);
        result
    }

    fn from_rank_and_file(rank: RankOrFileIndex, file: RankOrFileIndex) -> Self {
        ((rank << 4) | file) as Self
    }

    fn sq_from_algebraic_notation(algebraic_notation: &str) -> Result<Self, FenError>
    where
        Self: Sized,
    {
        if algebraic_notation.len() != 2 {
            return Err(FenError::InvalidSquare(algebraic_notation.into()));
        }

        let file = algebraic_notation.chars().nth(0).unwrap() as u8 - 97;
        let rank = algebraic_notation.chars().nth(1).unwrap() as u8 - 0x31;

        if file > 7 || rank > 7 {
            return Err(FenError::InvalidSquare(algebraic_notation.into()));
        }

        Ok(Self::from_rank_and_file(
            rank as RankOrFileIndex,
            file as RankOrFileIndex,
        ))
    }
}

impl Piece for PieceStandard {
    fn to_algebraic_notation(&self) -> String {
        let piece_type = *self & 0xF;
        let result = piece_type_to_char(piece_type);
        match result {
            None => " ".into(),
            Some(chr) => {
                if *self & COLOUR_BIT_MASK == 0 {
                    chr.to_ascii_uppercase().into()
                } else {
                    chr.to_ascii_lowercase().into()
                }
            }
        }
    }

    fn from_algebraic_notation(algebraic_notation: char) -> Result<Self, FenError>
    where
        Self: Sized,
    {
        let piece_type = char_to_piece_type(algebraic_notation);
        if piece_type.is_none() {
            return Ok(EMPTY);
        }
        let colour = if algebraic_notation.is_uppercase() {
            WHITE
        } else {
            BLACK
        };
        Ok(piece_type.unwrap() | (colour << COLOUR_BIT))
    }
}

#[derive(Clone, Copy)]
pub struct Position0x88 {
    squares0x88: [PieceStandard; 128],
    squares: [PieceStandard; 64],
    king_squares: [SquareIndex0x88; 2],
    side_to_move: Colour,
    castling_rights: CastlingRights,
    ep_square: SquareIndex0x88,
    halfmove_clock: u32,
    fullmove_number: u32,
    zobrist_numbers: ZobristNumbers,
    hash_key: ZobristNumber,
    // bitboards
    pub bb_pieces: [Bitboard; PIECE_TYPES_COUNT],
    pub bb_colours: [Bitboard; 3],
}

impl Position<SquareIndex0x88, PieceStandard> for Position0x88 {}

impl SetPosition<SquareIndex0x88, PieceStandard> for Position0x88 {
    fn set_square_to_piece(&mut self, square: SquareIndex0x88, piece: PieceStandard) {
        let new_piece_type = piece_type(piece);
        let new_piece_colour = piece_colour(piece).unwrap_or(NONE) as usize;
        let current_piece_type = piece_type(self.square_piece(square));
        let current_piece_colour = piece_colour(self.square_piece(square)).unwrap_or(NONE) as usize;

        let square_index = index0x88to64(square) as usize;

        self.hash_key ^= self.zobrist_numbers.piece_square[current_piece_colour]
            [current_piece_type as usize][square_index];
        self.hash_key ^= self.zobrist_numbers.piece_square[new_piece_colour]
            [new_piece_type as usize][square_index];

        self.squares0x88[square] = piece;
        self.squares[square_index] = piece;

        if new_piece_type == KING {
            self.king_squares[new_piece_colour] = square;
        }

        let mask = square_mask0x88(square);
        self.bb_pieces[current_piece_type as usize] ^= mask;
        self.bb_colours[current_piece_colour] ^= mask;
        self.bb_pieces[new_piece_type as usize] ^= mask;
        self.bb_colours[new_piece_colour] ^= mask;
    }

    fn remove_from_square(&mut self, square: SquareIndex0x88) {
        self.set_square_to_piece(square, EMPTY);
    }

    #[inline]
    fn square_piece(&self, square: SquareIndex0x88) -> PieceStandard {
        debug_assert!(is_valid_square(square as i16));
        self.squares0x88[square]
    }
}

impl Position0x88 {
    pub fn set_ep_square(&mut self, square: SquareIndex0x88) {
        if self.ep_square != 0 {
            self.hash_key ^= self.zobrist_numbers.ep_file[file(self.ep_square) as usize];
        }
        if square != 0 {
            self.hash_key ^= self.zobrist_numbers.ep_file[file(square) as usize];
        }
        self.ep_square = square;
    }

    pub fn set_castling_rights(&mut self, castling_rights: CastlingRights) {
        for i in 0..4 {
            let mask = 1 << i;
            if (self.castling_rights.flags & mask) != castling_rights.flags & mask {
                self.hash_key ^= self.zobrist_numbers.castling_rights[i];
            }
        }
        self.castling_rights = castling_rights;
    }

    pub fn set_side_to_move(&mut self, colour: Colour) {
        if colour == self.side_to_move {
            return;
        }
        self.side_to_move = colour;
    }

    pub fn get_side_to_move(&self) -> Colour {
        self.side_to_move
    }

    // This is mainly due to bad design - we need to update the hash.
    pub fn castling_allow(&mut self, colour: Colour, side: Option<BoardSide>) {
        let pre = self.castling_rights.flags;
        self.castling_rights.allow(colour, side);
        let post = self.castling_rights.flags;
        let mut changed = pre & !post & 0xF;
        while changed != 0 {
            let bit = changed.trailing_zeros();
            self.hash_key ^= self.zobrist_numbers.castling_rights[bit as usize];
            changed ^= 1 << bit;
        }
    }

    pub fn castling_remove(&mut self, colour: Colour, side: Option<BoardSide>) {
        let pre = self.castling_rights.flags;
        self.castling_rights.remove(colour, side);
        let post = self.castling_rights.flags;
        let mut changed = pre & !post & 0xF;
        while changed != 0 {
            let bit = changed.trailing_zeros();
            self.hash_key ^= self.zobrist_numbers.castling_rights[bit as usize];
            changed ^= 1 << bit;
        }
    }
}

impl Hashable for Position0x88 {
    fn hash_key(&self) -> ZobristNumber {
        self.hash_key
    }
}

impl Debug for Position0x88 {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut s = String::new();
        let sep = "+-".repeat(8) + "+\n";
        s.push_str(&sep);
        for i in (0x00..0x80).step_by(0x10).rev() {
            s.push_str("|");
            for sq in self.squares0x88[i..i + 8].iter().map(|s| piece_to_char(*s)) {
                s.push(sq.unwrap());
                s.push_str("|");
            }
            s.push_str("\n");
            s.push_str(&sep);
        }
        writeln!(f, "{}", s)
    }
}

impl Default for Position0x88 {
    fn default() -> Self {
        Position0x88 {
            squares0x88: [0; 128],
            squares: [0; 64],
            king_squares: [0; 2],
            side_to_move: WHITE,
            castling_rights: CastlingRights::new(),
            ep_square: 0,
            halfmove_clock: 0,
            fullmove_number: 0,
            zobrist_numbers: ZobristNumbers::init(),
            hash_key: 0,
            bb_pieces: [0; PIECE_TYPES_COUNT],
            bb_colours: [0; 3],
        }
    }
}

impl GenerateMoves<SquareIndex0x88, PieceStandard> for Position0x88 {
    fn generate_moves(&self) -> Vec<Move0x88> {
        generate_moves(self)
    }
}

#[inline]
pub const fn file(square: SquareIndex0x88) -> RankOrFileIndex {
    square as u8 & 7
}

#[inline]
pub const fn rank(square: SquareIndex0x88) -> RankOrFileIndex {
    square as u8 >> 4
}

#[inline]
fn diagonal(square: SquareIndex0x88) -> RankOrFileIndex {
    7 + rank(square) - file(square)
}

#[inline]
fn antidiagonal(square: SquareIndex0x88) -> RankOrFileIndex {
    rank(square) + file(square)
}

#[inline]
fn square_index(rank: RankOrFileIndex, file: RankOrFileIndex) -> SquareIndex0x88 {
    debug_assert!(rank < 8);
    debug_assert!(file < 8);
    ((rank as usize) << 4) | file as usize
}

#[inline]
pub fn get_piece(piece_type: PieceType, colour: Colour) -> PieceStandard {
    piece_type | (colour << COLOUR_BIT)
}

#[inline]
pub fn piece_type(piece: PieceStandard) -> PieceType {
    piece & 0xF
}

pub fn piece_colour(piece: PieceStandard) -> Option<Colour> {
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
    colour ^ 1
}

#[inline]
pub const fn index0x88to64(square: SquareIndex0x88) -> SquareIndex64 {
    debug_assert!(is_valid_square(square as i16));
    rank(square) * 8 + file(square)
}

#[inline]
pub const fn index64to0x88(square: SquareIndex64) -> SquareIndex0x88 {
    (bitboards::rank(square) * 16 + bitboards::file(square)) as SquareIndex0x88
}

#[derive(Default)]
pub struct SquareIterator {
    current: SquareIndex0x88,
}

impl Iterator for SquareIterator {
    type Item = SquareIndex0x88;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current & 0x88 != 0 {
            if self.current > 0x77 {
                return None;
            }
            self.current += 8;
        }
        let result = Some(self.current);
        self.current += 1;
        result
    }
}

pub fn square_iter() -> impl Iterator<Item = SquareIndex0x88> {
    SquareIterator::default()
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
    fn test_index0x88to64() {
        assert_eq!(8, index0x88to64(0x10));
        assert_eq!(63, index0x88to64(0x77));
        assert_eq!(43, index0x88to64(0x53));
        assert_eq!(0, index0x88to64(0x00));
    }

    #[test]
    fn test_square_iterator() {
        let mut iterator1 = SquareIterator { current: 0x08 };
        assert_eq!(0x10, iterator1.next().unwrap());
        let mut i = 0;
        for _ in square_iter() {
            i += 1;
        }
        assert_eq!(64, i);
    }
}
