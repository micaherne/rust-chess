use chess_uci::messages::LongAlgebraicNotationMove;

use crate::{
    fen::{ConsumeFen, Fen, FenError},
    transposition::Hashable,
};

pub const PIECE_COUNT: usize = 7;

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

pub enum Castling {
    WhiteKingSide,
    WhiteQueenSide,
    BlackKingSide,
    BlackQueenSide,
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
    A2,
    A3,
    A4,
    A5,
    A6,
    A7,
    A8,
    B1,
    B2,
    B3,
    B4,
    B5,
    B6,
    B7,
    B8,
    C1,
    C2,
    C3,
    C4,
    C5,
    C6,
    C7,
    C8,
    D1,
    D2,
    D3,
    D4,
    D5,
    D6,
    D7,
    D8,
    E1,
    E2,
    E3,
    E4,
    E5,
    E6,
    E7,
    E8,
    F1,
    F2,
    F3,
    F4,
    F5,
    F6,
    F7,
    F8,
    G1,
    G2,
    G3,
    G4,
    G5,
    G6,
    G7,
    G8,
    H1,
    H2,
    H3,
    H4,
    H5,
    H6,
    H7,
    H8,
}
