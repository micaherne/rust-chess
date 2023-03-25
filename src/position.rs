use crate::position0x88::Colour;

/// Marker trait for positions.
pub trait Position {}

/// Enables pieces to be set and retrieved in the position.
///
pub trait SetPosition<S, P>
where
    Self: Position,
{
    fn set_square_to_piece(&mut self, square: S, piece: P);
    fn remove_from_square(&mut self, square: S);
    fn square_piece(&self, square: S) -> P;
}

pub trait Evaluate<ScoreType>
where
    Self: Position,
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

#[derive(Debug, Clone, Copy)]
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
