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
