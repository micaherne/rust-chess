use chess_uci::messages::LongAlgebraicNotationMove;

use crate::{
    fen::{ConsumeFen, Fen, FenError},
    transposition::Hashable,
};

pub const PIECE_COUNT: usize = 7;
pub const SQUARE_COUNT: usize = 64;

/// A number representing a rank or a file.
pub type RankOrFileIndex = u8;

pub trait SquareIndex {
    // These have sq to distinguish them from the methods on the Piece trait (when applied to PieceStandard
    // which is a u8 the same as SquareIndex64). This sucks and could do with being fixed.
    fn sq_to_algebraic_notation(&self) -> String;
    fn sq_from_algebraic_notation(algebraic_notation: &str) -> Result<Self, FenError>
    where
        Self: Sized;
    fn from_rank_and_file(rank: RankOrFileIndex, file: RankOrFileIndex) -> Self;
}

pub trait Piece {
    fn to_algebraic_notation(&self) -> String;

    /// Returns the piece type given an algebraic notation character.
    /// This should also return the empty type for a space.
    fn from_algebraic_notation(algebraic_notation: char) -> Result<Self, FenError>
    where
        Self: Sized;
}

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub enum Castling {
    WhiteKingSide,
    WhiteQueenSide,
    BlackKingSide,
    BlackQueenSide,
}

impl Castling {
    pub fn from_index(index: usize) -> Castling {
        match index {
            0 => Castling::WhiteKingSide,
            1 => Castling::WhiteQueenSide,
            2 => Castling::BlackKingSide,
            3 => Castling::BlackQueenSide,
            _ => panic!("Invalid castling index"),
        }
    }
}

/// Trait for checking and setting castling rights on a position.
pub trait HasCastlingRights {
    fn can_castle(&self, castling: Castling) -> bool;
    fn set_castling(&mut self, castling: Castling, can_castle: bool);
}

/// A marker trait for a castling rights type in a position.
pub trait CastlingRights {}

pub trait MakeMoves<S: SquareIndex, P: Piece, C: CastlingRights> {
    fn make_moves(&mut self, moves: &Vec<LongAlgebraicNotationMove>) -> Vec<MoveUndo<S, P, C>>;
    fn make_move(
        &mut self,
        from_index: S,
        to_index: S,
        queening_piece: Option<P>,
    ) -> MoveUndo<S, P, C>;
    fn undo_move(&mut self, undo: MoveUndo<S, P, C>);
}

/// Marker trait for positions.
pub trait Position<S: SquareIndex, P: Piece, C: CastlingRights>:
    MakeMoves<S, P, C> + Hashable + Into<Fen> + ConsumeFen
{
}

/// Enables pieces to be set and retrieved in the position.
///
pub trait SetPosition<S: SquareIndex, P: Piece> {
    fn set_square_to_piece(&mut self, square: S, piece: P);
    fn remove_from_square(&mut self, square: S);
    fn square_piece(&self, square: S) -> P;
}

pub trait Evaluate<ScoreType, S: SquareIndex, P: Piece, C: CastlingRights>
where
    Self: Position<S, P, C>,
{
    fn evaluate(&self) -> ScoreType;
}

#[derive(Clone, Copy)]
pub struct MoveUndo<S: SquareIndex, P: Piece, C: CastlingRights> {
    pub from_index: S,
    pub to_index: S,
    pub moved_piece: P,
    pub captured_piece: P,
    pub ep_square: S,
    pub halfmove_clock: u32,
    pub castling_rights: C,
}

#[derive(Clone, Copy)]
pub enum BoardSide {
    Queenside,
    Kingside,
}

#[derive(Clone, Copy)]
pub enum Square {
    A1,
    B1,
    C1,
    D1,
    E1,
    F1,
    G1,
    H1,
    A2,
    B2,
    C2,
    D2,
    E2,
    F2,
    G2,
    H2,
    A3,
    B3,
    C3,
    D3,
    E3,
    F3,
    G3,
    H3,
    A4,
    B4,
    C4,
    D4,
    E4,
    F4,
    G4,
    H4,
    A5,
    B5,
    C5,
    D5,
    E5,
    F5,
    G5,
    H5,
    A6,
    B6,
    C6,
    D6,
    E6,
    F6,
    G6,
    H6,
    A7,
    B7,
    C7,
    D7,
    E7,
    F7,
    G7,
    H7,
    A8,
    B8,
    C8,
    D8,
    E8,
    F8,
    G8,
    H8,
}
