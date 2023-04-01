use chess_uci::messages::LongAlgebraicNotationMove;

use crate::{
    fen::{ConsumeFen, Fen, FenError},
    position0x88::Colour,
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

pub trait MakeMoves<S: SquareIndex, P: Piece> {
    fn make_moves(&mut self, moves: &Vec<LongAlgebraicNotationMove>) -> Vec<MoveUndo<S, P>>;
    fn make_move(
        &mut self,
        from_index: S,
        to_index: S,
        queening_piece: Option<P>,
    ) -> MoveUndo<S, P>;
    fn undo_move(&mut self, undo: MoveUndo<S, P>);
}

/// Marker trait for positions.
pub trait Position<S: SquareIndex, P: Piece>:
    MakeMoves<S, P> + Hashable + Into<Fen> + ConsumeFen
{
}

/// Enables pieces to be set and retrieved in the position.
///
pub trait SetPosition<S: SquareIndex, P: Piece> {
    fn set_square_to_piece(&mut self, square: S, piece: P);
    fn remove_from_square(&mut self, square: S);
    fn square_piece(&self, square: S) -> P;
}

pub trait Evaluate<ScoreType, S: SquareIndex, P: Piece>
where
    Self: Position<S, P>,
{
    fn evaluate(&self) -> ScoreType;
}

#[derive(Clone, Copy)]
pub struct MoveUndo<S, P> {
    pub from_index: S,
    pub to_index: S,
    pub moved_piece: P,
    pub captured_piece: P,
    pub ep_square: S,
    pub halfmove_clock: u32,
    pub castling_rights: CastlingRights,
}

#[derive(Clone, Copy)]
pub enum BoardSide {
    Queenside,
    Kingside,
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub struct CastlingRights {
    pub flags: u8, // KQkq
}

impl CastlingRights {
    pub fn new() -> CastlingRights {
        CastlingRights { flags: 0 }
    }

    pub fn allow(&mut self, colour: Colour, side: Option<BoardSide>) {
        let mask = Self::mask(colour, side);
        self.flags |= mask;
    }

    pub fn remove(&mut self, colour: Colour, side: Option<BoardSide>) {
        let mask = Self::mask(colour, side);
        self.flags = self.flags & !(mask);
    }

    pub fn allowed(&self, colour: Colour, side: BoardSide) -> bool {
        let mask = Self::mask(colour, Some(side));
        self.flags & mask != 0
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

#[cfg(test)]
mod test {

    use crate::position0x88::{BLACK, WHITE};

    use super::*;

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

        let castling2 = CastlingRights { flags: 0b1011 };
        assert!(!castling2.allowed(WHITE, BoardSide::Queenside));
    }
}
