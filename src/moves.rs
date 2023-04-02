use crate::position::{Piece, SquareIndex};

#[derive(Debug, Default, Clone, Copy)]
pub struct Move<S: SquareIndex, P: Piece> {
    pub from_index: S,
    pub to_index: S,
    pub queening_piece: Option<P>,
}

pub trait GenerateMoves<S: SquareIndex, P: Piece> {
    fn generate_moves(&self) -> Vec<Move<S, P>>;
}
