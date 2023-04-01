use chess_uci::messages::LongAlgebraicNotationMove;

use crate::{
    position::{BoardSide, MakeMoves, MoveUndo, SetPosition},
    position0x88::{
        file, get_piece,
        notation::{to_fen, A_ROOK_HOME_SQUARES, H_ROOK_HOME_SQUARES, KING_HOME_SQUARES},
        opposite_colour, piece_colour, piece_type, rank, square_index, BLACK, EMPTY, KING, PAWN,
        ROOK, WHITE,
    },
};

use super::{
    notation::{char_to_piece_type, str_to_square_index},
    CastlingRights0x88, MoveUndo0x88, PieceStandard, PieceType, Position0x88, SquareIndex0x88,
};

pub trait ExtractMove {
    fn from_square(&self) -> SquareIndex0x88;
    fn to_square(&self) -> SquareIndex0x88;
    fn queening_piece(&self) -> Option<PieceType>;
}

impl ExtractMove for LongAlgebraicNotationMove {
    fn from_square(&self) -> SquareIndex0x88 {
        str_to_square_index(&self.text[0..=1]).unwrap()
    }

    fn to_square(&self) -> SquareIndex0x88 {
        str_to_square_index(&self.text[2..=3]).unwrap()
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

impl MakeMoves<SquareIndex0x88, PieceStandard, CastlingRights0x88> for Position0x88 {
    fn make_moves(&mut self, moves: &Vec<LongAlgebraicNotationMove>) -> Vec<MoveUndo0x88> {
        let mut result: Vec<MoveUndo0x88> = vec![];
        for mv in moves {
            let from = mv.from_square();
            let to = mv.to_square();
            let queening_piece = mv.queening_piece();
            let undo = self.make_move(from, to, queening_piece);
            result.push(undo);
        }
        result
    }
    fn make_move(
        &mut self,
        from_index: SquareIndex0x88,
        to_index: SquareIndex0x88,
        queening_piece: Option<PieceType>,
    ) -> MoveUndo0x88 {
        let moved_piece = self.squares0x88[from_index];
        debug_assert!(moved_piece != EMPTY);

        let dbg_fen = to_fen(self);

        let moved_piece_type = piece_type(moved_piece);

        let is_pawn_move = moved_piece_type == PAWN;

        let is_enpassent = is_pawn_move && self.ep_square != 0 && to_index == self.ep_square;
        let mut capture_square = to_index;

        if is_enpassent {
            capture_square = match from_index < to_index {
                true => to_index - 16,
                false => to_index + 16,
            };
        }

        let captured_piece = self.squares0x88[capture_square];
        let undo = MoveUndo {
            from_index,
            to_index,
            moved_piece,
            captured_piece,
            castling_rights: self.castling_rights,
            ep_square: self.ep_square,
            halfmove_clock: self.halfmove_clock,
        };

        let ep_square = self.ep_square;
        self.set_ep_square(0);

        let moved_piece_colour = piece_colour(self.squares0x88[from_index]).unwrap();

        let new_piece = match queening_piece {
            None => self.squares0x88[from_index],
            Some(piece_type) => get_piece(piece_type, moved_piece_colour),
        };

        self.set_square_to_piece(to_index, new_piece);

        if is_pawn_move {
            let diff = from_index.abs_diff(to_index);
            if diff == 32 {
                let from_rank = rank(from_index);
                let from_file = file(from_index);
                if from_rank == 1 {
                    self.set_ep_square(square_index(2, from_file));
                } else if from_rank == 6 {
                    self.set_ep_square(square_index(5, from_file));
                } else {
                    panic!("Weird e.p. square!");
                }
            }

            if ep_square != 0 && to_index == ep_square {
                let captured_pawn_square = if moved_piece_colour == WHITE {
                    to_index - 16
                } else {
                    to_index + 16
                };
                let captured_pawn = self.squares0x88[captured_pawn_square];
                match piece_colour(captured_pawn) {
                    None => panic!("Not a piece"),
                    Some(colour) => {
                        if colour != opposite_colour(moved_piece_colour)
                            || piece_type(captured_pawn) != PAWN
                        {
                            panic!("Invalid e.p. capture piece {:#?} {}, e.p. square {}, from {} to {}, cap square {}", 
                                self, dbg_fen, ep_square, from_index, to_index, captured_pawn_square);
                        }
                        self.set_square_to_piece(captured_pawn_square, EMPTY);
                    }
                }
            }
        } else if moved_piece_type == KING {
            if from_index == KING_HOME_SQUARES[moved_piece_colour as SquareIndex0x88] {
                self.castling_remove(moved_piece_colour, None);
            }

            // Castling.
            if from_index.abs_diff(to_index) == 2 {
                let rook_square = if from_index < to_index {
                    H_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex0x88]
                } else {
                    A_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex0x88]
                };
                let rook = self.squares0x88[rook_square];
                if piece_colour(rook).unwrap() != moved_piece_colour {
                    panic!(
                        "Wrong piece colour: {:x} FEN: {}",
                        rook_square,
                        to_fen(self)
                    );
                }
                if piece_type(rook) != ROOK {
                    panic!("Not a rook");
                }
                let rook_to = (from_index + to_index) / 2; // Avoid negative numbers.
                self.set_square_to_piece(rook_to, rook);
                self.remove_from_square(rook_square);

                // TODO: Update hash key.
                self.castling_remove(moved_piece_colour, None);
            }

            self.king_squares[moved_piece_colour as SquareIndex0x88] = to_index;
        } else if moved_piece_type == ROOK {
            if from_index == A_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex0x88] {
                self.castling_remove(moved_piece_colour, Some(BoardSide::Queenside));
            } else if from_index == H_ROOK_HOME_SQUARES[moved_piece_colour as SquareIndex0x88] {
                self.castling_remove(moved_piece_colour, Some(BoardSide::Kingside));
            }
        }

        let opp_side_colour = opposite_colour(self.side_to_move);

        // Test for rook captures and remove castling rights.
        if captured_piece != EMPTY && piece_type(captured_piece) == ROOK {
            if capture_square == A_ROOK_HOME_SQUARES[opp_side_colour as usize] {
                self.castling_remove(opp_side_colour, Some(BoardSide::Queenside));
            } else if capture_square == H_ROOK_HOME_SQUARES[opp_side_colour as usize] {
                self.castling_remove(opp_side_colour, Some(BoardSide::Kingside));
            }
        }

        // Remove the e.p. capture if necessary.
        if is_enpassent {
            self.remove_from_square(capture_square);
        }

        // Reset the half move clock for pawn moves or captures.
        if captured_piece != EMPTY || is_pawn_move {
            self.halfmove_clock = 0;
        }

        self.remove_from_square(from_index);

        if self.side_to_move == BLACK {
            self.fullmove_number += 1;
        }

        self.side_to_move = opp_side_colour;
        self.hash_key ^= self.zobrist_numbers.black_to_move;

        undo
    }

    fn undo_move(&mut self, undo: MoveUndo0x88) {
        // En passent.
        let is_enpassent =
            undo.to_index == undo.ep_square && piece_type(self.squares0x88[undo.to_index]) == PAWN;
        let mut capture_square = undo.to_index;
        if is_enpassent {
            capture_square = match undo.from_index < undo.to_index {
                true => undo.to_index - 16,
                false => undo.to_index + 16,
            };
        }

        let is_king_move = piece_type(self.squares0x88[undo.to_index]) == KING;

        let side_moved = opposite_colour(self.side_to_move);

        // Castling.
        if is_king_move && undo.to_index.abs_diff(undo.from_index) == 2 {
            let rook_index = (undo.to_index + undo.from_index) / 2;

            debug_assert!(piece_type(self.squares0x88[rook_index]) == ROOK);

            let rook_from_index = match undo.to_index < undo.from_index {
                true => A_ROOK_HOME_SQUARES[side_moved as usize],
                false => H_ROOK_HOME_SQUARES[side_moved as usize],
            };

            self.set_square_to_piece(rook_from_index, self.squares0x88[rook_index]);
            self.remove_from_square(rook_index);
        }

        if is_king_move {
            self.king_squares[side_moved as usize] = undo.from_index;
        }

        // TODO: Deal with queening.

        self.set_square_to_piece(undo.from_index, undo.moved_piece);

        if is_enpassent {
            self.remove_from_square(undo.to_index);
        }

        self.set_square_to_piece(capture_square, undo.captured_piece);

        self.set_ep_square(undo.ep_square);
        self.set_castling_rights(undo.castling_rights);
        self.halfmove_clock = undo.halfmove_clock;
        if self.side_to_move == WHITE {
            self.fullmove_number -= 1;
        }
        self.side_to_move = side_moved;
        self.hash_key ^= self.zobrist_numbers.black_to_move;
    }
}

#[cfg(test)]
mod test {

    use crate::{
        fen::STARTPOS_FEN,
        position0x88::{
            notation::{set_from_fen, set_startpos},
            KNIGHT,
        },
    };

    use super::*;

    #[test]
    fn test_char_to_piece_type() {
        assert_eq!(PAWN, char_to_piece_type('p').unwrap());
        assert_eq!(PAWN, char_to_piece_type('P').unwrap());
        assert_eq!(None, char_to_piece_type('x'));
    }

    #[test]
    fn test_str_to_square_index() {
        assert_eq!(0x44, str_to_square_index("e5").unwrap());
        assert_eq!(0x44, str_to_square_index("E5").unwrap());
        assert_eq!(0x53, str_to_square_index("d6").unwrap());
        assert_eq!(None, str_to_square_index("X20"));
    }

    #[test]
    fn test_long_algebraic_notation_move() {
        let mv1 = LongAlgebraicNotationMove::from_text("e2e4".to_string()).unwrap();
        assert_eq!(0x14, mv1.from_square());
        assert_eq!(0x34, mv1.to_square());
    }

    #[test]
    fn test_move_ep() {
        let mut position = Position0x88::default();
        let original_fen = "6k1/8/8/3pP3/8/8/8/6K1 w - d6 2 1";
        set_from_fen(&mut position, &original_fen).unwrap();
        assert_eq!(0x53, position.ep_square);
        let undo = position.make_move(0x44, 0x53, None);
        assert_eq!("6k1/8/3P4/8/8/8/8/6K1 b - - 0 1", to_fen(&position));
        position.undo_move(undo);
        assert_eq!(original_fen, to_fen(&position));
        println!("{}", to_fen(&position));
    }

    #[test]
    fn test_make_move() {
        let fen = "r3kbnr/2qn2p1/8/pppBpp1P/3P1Pb1/P1P1P3/1P2Q2P/RNB1K1NR w KQkq - 0 1";
        let mut position = Position0x88::default();
        set_from_fen(&mut position, fen).unwrap();
        position.make_move(0x43, 0x70, None);
        assert_eq!(0b1110, position.castling_rights.flags);
    }

    #[test]
    fn test_make_move2() {
        let fen = "rn3b1r/1bqpp1k1/p7/2p2p1p/P2P4/2N1P1P1/1pK1NPP1/R3QB1R b - - 0 1";
        let mut position = Position0x88::default();
        set_from_fen(&mut position, fen).unwrap();
        let undo = position.make_move(17, 0, Some(KNIGHT));
        position.undo_move(undo);
        assert_eq!(fen, to_fen(&position));
    }

    #[test]
    fn test_undo_move() {
        let mut position = Position0x88::default();
        set_startpos(&mut position);
        let moves: Vec<LongAlgebraicNotationMove> = [
            "e2e4", "e7e5", "f2f4", "e5f4", "f1b5", "b8c6", "g1f3", "a7a6", "e1g1",
        ]
        .map(|x| LongAlgebraicNotationMove::from_text(x.to_string()).unwrap())
        .into();
        let mut undo_stack = position.make_moves(&moves);
        loop {
            let undo = undo_stack.pop();
            if let None = undo {
                break;
            }
            position.undo_move(undo.unwrap());
        }

        assert_eq!(STARTPOS_FEN, to_fen(&position));

        // println!("{:#?}", position);
    }
}
