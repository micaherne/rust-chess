use std::fmt::Display;

use chess_uci::messages::LongAlgebraicNotationMove;

use crate::{
    bitboards::{rank, Bitboard, BitboardOps, SquareIndex64},
    fen::FenError,
    position::{Castling, HasCastlingRights, SetPosition, Square, SquareIndex},
    position64::{Colour, PieceType},
    zobrist::ZOBRIST_NUMBERS,
};

use super::{AlgebraicNotation, CastlingRightsBool, PieceWithColour, Position64};

#[derive(Debug, Default, Clone, Copy)]
pub struct Move {
    pub from_index: SquareIndex64,
    pub to_index: SquareIndex64,
    pub queening_piece: Option<PieceType>,
}

impl Move {
    pub fn new(
        from_index: SquareIndex64,
        to_index: SquareIndex64,
        queening_piece: Option<PieceType>,
    ) -> Self {
        Self {
            from_index,
            to_index,
            queening_piece,
        }
    }
}

impl Display for Move {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{}{}",
            self.from_index.sq_to_algebraic_notation(),
            self.to_index.sq_to_algebraic_notation(),
            match self.queening_piece {
                Some(x) => x.to_algebraic_notation(),
                _ => "".to_string(),
            }
        )
    }
}

pub struct MoveUndo {
    pub from_index: SquareIndex64,
    pub to_index: SquareIndex64,
    pub moved_piece: PieceWithColour,
    pub captured_piece: PieceWithColour,
    pub ep_square: Bitboard,
    pub halfmove_clock: u32,
    pub castling_rights: CastlingRightsBool,
    #[cfg(debug_assertions)]
    pub fen: String,
}

pub trait HasToAndFromIndex {
    fn from_index(&self) -> SquareIndex64;
    fn to_index(&self) -> SquareIndex64;
}

impl HasToAndFromIndex for Move {
    fn from_index(&self) -> SquareIndex64 {
        self.from_index
    }

    fn to_index(&self) -> SquareIndex64 {
        self.to_index
    }
}

impl HasToAndFromIndex for MoveUndo {
    fn from_index(&self) -> SquareIndex64 {
        self.from_index
    }

    fn to_index(&self) -> SquareIndex64 {
        self.to_index
    }
}

pub trait GenerateMoves {
    fn generate_moves(&self) -> Vec<Move>;
}

pub trait MakeMove {
    fn make_move(&mut self, mv: Move) -> MoveUndo;
    fn undo_move(&mut self, mv: MoveUndo);
    fn make_moves(&mut self, mvs: &[Move]) -> Vec<MoveUndo> {
        let mut undo_moves = Vec::new();
        for mv in mvs {
            undo_moves.push(self.make_move(*mv));
        }
        undo_moves
    }
}

pub trait CheckMoveOrUndo<T: HasToAndFromIndex> {
    fn is_standard_capture(&self, mv: &T) -> bool;
    fn is_en_passant(&self, mv: &T) -> bool;
    fn is_en_passant_capture(&self, mv: &T) -> bool;
    fn is_castling(&self, mv: &T) -> bool;
    fn is_queening(&self, mv: &T) -> bool;
    fn ep_square(&self, mv: &T) -> Option<Bitboard>;
    fn capture_square(&self, mv: &T) -> SquareIndex64;
}

impl CheckMoveOrUndo<Move> for Position64 {
    /// A capture that is not en passant.
    fn is_standard_capture(&self, mv: &Move) -> bool {
        self.squares[mv.to_index as usize].piece_type != PieceType::Empty
    }

    /// A move that results in an en passant square.
    fn is_en_passant(&self, mv: &Move) -> bool {
        mv.from_index.abs_diff(mv.to_index) == 16
            && self.squares[mv.from_index as usize].piece_type == PieceType::Pawn
    }

    fn is_en_passant_capture(&self, mv: &Move) -> bool {
        self.ep_square != 0
            && self.square_piece(mv.from_index).piece_type == PieceType::Pawn
            && self.ep_square == Bitboard::from_single_square(mv.to_index)
    }

    fn is_castling(&self, mv: &Move) -> bool {
        mv.from_index.abs_diff(mv.to_index) == 2
            && self.squares[mv.from_index as usize].piece_type == PieceType::King
    }

    fn is_queening(&self, mv: &Move) -> bool {
        [0, 7].contains(&rank(mv.to_index))
            && self.squares[mv.from_index as usize].piece_type == PieceType::Pawn
    }

    fn ep_square(&self, mv: &Move) -> Option<Bitboard> {
        if self.is_en_passant(mv) {
            Some(Bitboard::from_single_square(
                (mv.to_index + mv.from_index) / 2,
            ))
        } else {
            None
        }
    }

    fn capture_square(&self, mv: &Move) -> SquareIndex64 {
        if self.ep_square != 0
            && self.square_piece(mv.from_index).piece_type == PieceType::Pawn
            && Bitboard::from_single_square(mv.to_index) == self.ep_square
        {
            match self.side_to_move {
                Colour::White => mv.to_index - 8,
                Colour::Black => mv.to_index + 8,
                Colour::None => unreachable!("Colour::None is not a valid colour"),
            }
        } else {
            mv.to_index
        }
    }
}

impl CheckMoveOrUndo<MoveUndo> for Position64 {
    fn is_standard_capture(&self, mv: &MoveUndo) -> bool {
        mv.captured_piece.piece_type != PieceType::Empty
    }

    fn is_en_passant(&self, mv: &MoveUndo) -> bool {
        mv.from_index.abs_diff(mv.to_index) == 16 && mv.moved_piece.piece_type == PieceType::Pawn
    }

    fn is_en_passant_capture(&self, mv: &MoveUndo) -> bool {
        mv.ep_square != 0
            && mv.moved_piece.piece_type == PieceType::Pawn
            && mv.ep_square == Bitboard::from_single_square(mv.to_index)
    }

    fn is_castling(&self, mv: &MoveUndo) -> bool {
        mv.from_index.abs_diff(mv.to_index) == 2 && mv.moved_piece.piece_type == PieceType::King
    }

    fn is_queening(&self, mv: &MoveUndo) -> bool {
        [0, 7].contains(&rank(mv.to_index)) && mv.moved_piece.piece_type == PieceType::Pawn
    }

    fn ep_square(&self, mv: &MoveUndo) -> Option<Bitboard> {
        if self.is_en_passant(mv) {
            Some(Bitboard::from_single_square(
                mv.to_index - (mv.to_index - mv.from_index) / 2,
            ))
        } else {
            None
        }
    }

    fn capture_square(&self, mv: &MoveUndo) -> SquareIndex64 {
        if mv.ep_square != 0
            && mv.moved_piece.piece_type == PieceType::Pawn
            && Bitboard::from_single_square(mv.to_index) == mv.ep_square
        {
            match self.side_to_move {
                Colour::White => mv.to_index + 8,
                Colour::Black => mv.to_index - 8,
                Colour::None => unreachable!("Colour::None is not a valid colour"),
            }
        } else {
            mv.to_index
        }
    }
}

impl MakeMove for Position64 {
    fn make_move(&mut self, mv: Move) -> MoveUndo {
        let moved_piece = self.squares[mv.from_index as usize];

        debug_assert!(moved_piece.piece_type != PieceType::Empty);

        #[cfg(debug_assertions)]
        let fen = self.to_string();

        let captured_piece = self.squares[self.capture_square(&mv) as usize];

        let undo = MoveUndo {
            from_index: mv.from_index,
            to_index: mv.to_index,
            moved_piece,
            captured_piece,
            castling_rights: self.castling_rights,
            ep_square: self.ep_square,
            halfmove_clock: self.halfmove_clock,
            #[cfg(debug_assertions)]
            fen,
        };

        let new_piece = match mv.queening_piece {
            None => self.squares[mv.from_index as usize],
            Some(piece) => PieceWithColour {
                piece_type: piece,
                colour: self.side_to_move,
            },
        };

        // Move is a pawn capture to e.p. square.
        if self.is_en_passant_capture(&mv) {
            self.remove_from_square(self.capture_square(&mv));
        }

        // Move is a castling move.
        if self.is_castling(&mv) {
            let (rook_from, rook_to) = match mv.to_index {
                6 => (Square::H1, Square::F1),  // G1
                62 => (Square::H8, Square::F8), // G8
                2 => (Square::A1, Square::D1),  // C1
                58 => (Square::A8, Square::D8), // C8
                _ => unreachable!(),
            };

            let rook = self.squares[rook_from as usize];
            self.remove_from_square(rook_from as SquareIndex64);
            self.set_square_to_piece(rook_to as SquareIndex64, rook);
        }

        // Unset castling rights.
        // Rook move.
        if moved_piece.piece_type == PieceType::Rook {
            match mv.from_index {
                0 => self.set_castling(Castling::WhiteQueenSide, false),
                7 => self.set_castling(Castling::WhiteKingSide, false),
                56 => self.set_castling(Castling::BlackQueenSide, false),
                63 => self.set_castling(Castling::BlackKingSide, false),
                _ => {}
            }
        // King move.
        } else if moved_piece.piece_type == PieceType::King {
            match mv.from_index {
                4 => {
                    self.set_castling(Castling::WhiteKingSide, false);
                    self.set_castling(Castling::WhiteQueenSide, false);
                }
                60 => {
                    self.set_castling(Castling::BlackKingSide, false);
                    self.set_castling(Castling::BlackQueenSide, false);
                }
                _ => {}
            }
        }
        // Rook capture.
        if captured_piece.piece_type == PieceType::Rook {
            match mv.to_index {
                0 => self.set_castling(Castling::WhiteQueenSide, false),
                7 => self.set_castling(Castling::WhiteKingSide, false),
                56 => self.set_castling(Castling::BlackQueenSide, false),
                63 => self.set_castling(Castling::BlackKingSide, false),
                _ => {}
            }
        }

        // Set ep square to zero before potentially setting it to a new value.
        self.set_ep_square(0);

        // Move creates ep square (i.e. pawn moves 2 squares)
        match self.ep_square(&mv) {
            Some(ep_square) => self.set_ep_square(ep_square),
            None => {}
        }

        // We can move now as ep square is set (this uses position functions that check the pieces on the board).
        self.remove_from_square(mv.from_index);
        self.set_square_to_piece(mv.to_index, new_piece);

        // Update halfmove clock.
        if moved_piece.piece_type == PieceType::Pawn
            || captured_piece.piece_type != PieceType::Empty
        {
            self.halfmove_clock = 0;
        } else {
            self.halfmove_clock += 1;
        }

        // Update fullmove counter.
        if self.side_to_move == Colour::Black {
            self.fullmove_number += 1;
        }

        // Update side to move.
        self.side_to_move = self.side_to_move.opposite();
        self.hash_key ^= ZOBRIST_NUMBERS.black_to_move;

        undo
    }

    fn undo_move(&mut self, mv: MoveUndo) {
        let capture_square = self.capture_square(&mv);

        self.set_square_to_piece(mv.from_index, mv.moved_piece);
        self.set_square_to_piece(capture_square, mv.captured_piece);

        if self.is_en_passant_capture(&mv) {
            self.remove_from_square(mv.to_index);
        }

        if self.is_castling(&mv) {
            let (rook_from, rook_to) = match mv.to_index {
                6 => (Square::H1, Square::F1),  // G1
                62 => (Square::H8, Square::F8), // G8
                2 => (Square::A1, Square::D1),  // C1
                58 => (Square::A8, Square::D8), // C8
                _ => unreachable!(),
            };

            let rook = self.squares[rook_to as usize];
            self.remove_from_square(rook_to as SquareIndex64);
            self.set_square_to_piece(rook_from as SquareIndex64, rook);
        }

        self.set_ep_square(mv.ep_square);
        for (castling, value) in mv.castling_rights.0.iter().enumerate() {
            self.set_castling(Castling::from_index(castling), *value);
        }
        self.halfmove_clock = mv.halfmove_clock;

        if self.side_to_move == Colour::White {
            self.fullmove_number -= 1;
        }
        self.side_to_move = self.side_to_move.opposite();
        self.hash_key ^= ZOBRIST_NUMBERS.black_to_move;

        #[cfg(debug_assertions)]
        {
            let fen = self.to_string();
            debug_assert!(fen == mv.fen);
        }
    }
}

impl From<Move> for LongAlgebraicNotationMove {
    fn from(mv: Move) -> Self {
        let mut text = String::new();
        text.push_str(mv.from_index.sq_to_algebraic_notation().as_str());
        text.push_str(mv.to_index.sq_to_algebraic_notation().as_str());
        if let Some(q) = mv.queening_piece {
            text.push_str(q.to_algebraic_notation().as_str());
        }
        LongAlgebraicNotationMove { text }
    }
}

impl TryFrom<LongAlgebraicNotationMove> for Move {
    type Error = FenError;

    fn try_from(value: LongAlgebraicNotationMove) -> Result<Self, Self::Error> {
        let from_str = value.text[0..2].to_string();
        let to_str = value.text[2..4].to_string();
        let piece_char = value.text.chars().nth(4);
        let piece = match piece_char {
            Some(x) => Some(PieceType::from_algebraic_notation(x.to_string().as_str())?),
            None => None,
        };
        Ok(Move::new(
            SquareIndex64::sq_from_algebraic_notation(&from_str)?,
            SquareIndex64::sq_from_algebraic_notation(&to_str)?,
            piece,
        ))
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_move_ep() {
        let original_fen = "6k1/8/8/3pP3/8/8/8/6K1 w - d6 2 1";
        let mut position: Position64 = original_fen.parse().unwrap();
        assert_eq!(43, position.ep_square.to_single_square().unwrap());
        assert_eq!(20100, position.material[Colour::White as usize]);
        assert_eq!(20100, position.material[Colour::Black as usize]);
        let mv = Move::new(36, 43, None);
        let undo = position.make_move(mv);
        assert_eq!(20000, position.material[Colour::Black as usize]);
        assert_eq!("6k1/8/3P4/8/8/8/8/6K1 b - - 0 1", position.to_string());
        position.undo_move(undo);
        assert_eq!(original_fen, position.to_string());
        assert_eq!(20100, position.material[Colour::Black as usize]);

        let original_fen = "2b1kbnB/rppqp3/3p3p/3P1pp1/pnP3P1/PP2P2P/4QP2/RN2KBNR b KQ - 0 1";
        let mut position: Position64 = original_fen.parse().unwrap();
        let mv = Move::new(50, 34, None);
        let undo = position.make_move(mv);
        assert_eq!(
            "2b1kbnB/rp1qp3/3p3p/2pP1pp1/pnP3P1/PP2P2P/4QP2/RN2KBNR w KQ c6 0 2",
            position.to_string()
        );
        position.undo_move(undo);
        assert_eq!(original_fen, position.to_string());
    }

    #[test]
    fn test_castling() {
        let original_fen = "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R3K2R w KQkq - 0 1";
        let mut position: Position64 = original_fen.parse().unwrap();
        let mv = Move::new(4, 6, None);
        let undo = position.make_move(mv);
        assert_eq!(
            "r3k2r/pppppppp/8/8/8/8/PPPPPPPP/R4RK1 b kq - 1 1",
            position.to_string()
        );
        position.undo_move(undo);
        assert_eq!(original_fen, position.to_string());
    }
}
