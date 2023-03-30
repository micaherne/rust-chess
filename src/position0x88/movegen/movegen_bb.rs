use std::vec;

use crate::{
    bitboards::*,
    iterate_squares,
    position0x88::{PieceStandard, BISHOP, KING, QUEEN, ROOK},
};

use super::{
    super::{
        index0x88to64, movegen::GenerateMoves, piece_type, Colour, PieceType, Position0x88, BLACK,
        KNIGHT, PAWN, WHITE,
    },
    Move,
};

#[derive(Debug)]
pub struct MoveGenerator {
    pin_masks: SixtyFourBitboards,
    checkers: Bitboard,

    // Minimal position data.
    side_to_move: Colour,
    ep_square: Bitboard,
    bb_pieces: [Bitboard; 7],
    bb_colours: [Bitboard; 3],
}

impl Default for MoveGenerator {
    fn default() -> Self {
        Self {
            pin_masks: [Bitboard::MAX; 64],
            checkers: Default::default(),
            side_to_move: Default::default(),
            ep_square: Default::default(),
            bb_pieces: Default::default(),
            bb_colours: Default::default(),
        }
    }
}

pub type Move64 = Move<SquareIndex64, PieceStandard>;

fn opposite_side(side: Colour) -> Colour {
    1 - side
}

// Private methods required for position.
impl Position0x88 {
    fn ep_capture_square_bb(&self) -> Bitboard {
        if self.ep_square == 0 {
            0
        } else {
            let offset = match self.side_to_move {
                WHITE => -8,
                BLACK => 8,
                _ => 0,
            };
            1 << (index0x88to64(self.ep_square) as i32 + offset)
        }
    }

    fn opposite_side(&self) -> Colour {
        1 - self.side_to_move
    }

    fn get_piece64(&self, square: SquareIndex64) -> PieceStandard {
        self.squares[square as usize]
    }
}

impl GenerateMoves<SquareIndex64, PieceStandard> for MoveGenerator {
    fn generate_moves(&self) -> Vec<Move64> {
        let mut result = vec![];
        result.extend(self.generate_pawn_captures());
        result.extend(self.generate_slider_moves());
        result.extend(self.generate_knight_moves());
        result.extend(self.generate_pawn_pushes());
        result.extend(self.generate_king_moves());
        result
    }
}

// Generating functions.
impl MoveGenerator {
    fn generate_pawn_captures(&self) -> Vec<Move64> {
        let mut result = vec![];
        let mut our_pawns =
            self.bb_colours[self.side_to_move as usize] & self.bb_pieces[PAWN as usize];
        iterate_squares!(our_pawns -> from_square {
            let captures = PAWN_ATTACK_SQUARES[self.side_to_move as usize][from_square as usize]
                & (self.bb_colours[opposite_side(self.side_to_move) as usize] | self.ep_square) & self.pin_masks[from_square as usize];

            let mut promotions = captures & (RANK_MASK[0] | RANK_MASK[7]);
            let mut nonpromotions = captures & !(RANK_MASK[0] | RANK_MASK[7]);

            iterate_squares!(nonpromotions -> to_square {
                debug_assert!(to_square < 64);
                result.push(Move64 { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: None });
            });

            iterate_squares!(promotions -> to_square {
                debug_assert!(to_square < 64);
                for piece_type in [QUEEN, ROOK, KNIGHT, BISHOP].iter() {
                    result.push(Move64 { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: Some(*piece_type) });
                }
            });
        });
        result
    }

    fn generate_pawn_pushes(&self) -> Vec<Move64> {
        let mut result = vec![];

        let mut our_pawns = self.our_pieces(PAWN);
        let mut our_pawns_mask = our_pawns;

        let mut aggregate_pin_mask: Bitboard = Bitboard::MAX;
        iterate_squares!(our_pawns_mask -> from_square {
            let f = file(from_square as SquareIndex64) as usize;
            aggregate_pin_mask = (!FILE_MASK[f] & aggregate_pin_mask) | (FILE_MASK[f] & aggregate_pin_mask & self.pin_masks[from_square as usize]);
        });

        let move_dir = PAWN_PUSH_DIRECTIONS[self.side_to_move as usize];

        for i in 0..2 {
            if i == 1 {
                our_pawns &= RANK_MASK[2 + (3 * self.side_to_move) as usize];
            }

            match self.side_to_move {
                WHITE => our_pawns <<= 8,
                BLACK => our_pawns >>= 8,
                _ => panic!("Invalid side to move"),
            }
            our_pawns &= !(self.bb_colours[WHITE as usize] | self.bb_colours[BLACK as usize])
                & aggregate_pin_mask;

            if our_pawns == 0 {
                break;
            }

            let mut promotions = our_pawns & (RANK_MASK[0] | RANK_MASK[7]);
            let mut nonpromotions = our_pawns & !(RANK_MASK[0] | RANK_MASK[7]);

            iterate_squares!(nonpromotions -> from_square {
                let to_square = from_square as i8 + ((1 + i) * -move_dir);
                debug_assert!(to_square >= 0 && to_square < 64);
                result.push(Move64 { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: None });
            });

            iterate_squares!(promotions -> from_square {
                let to_square = from_square as i8 + ((1 + i) * -move_dir);
                debug_assert!(to_square >= 0 && to_square < 64);
                for piece_type in [QUEEN, ROOK, KNIGHT, BISHOP].iter() {
                    result.push(Move64 { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: Some(*piece_type) });
                }
            });
        }

        result
    }

    fn generate_knight_moves(&self) -> Vec<Move64> {
        let mut result = vec![];
        let mut our_knights = self.our_pieces(KNIGHT);
        iterate_squares!(our_knights -> from_square {
            let mut moves = KNIGHT_ATTACK_SQUARES[from_square as usize]
                & !(self.bb_colours[self.side_to_move as usize])
                & self.pin_masks[from_square as usize];
            iterate_squares!(moves -> to_square {
                debug_assert!(to_square < 64);
                result.push(Move64 { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: None });
            });
        });
        result
    }

    fn generate_slider_moves(&self) -> Vec<Move64> {
        let mut result = vec![];
        for piece_type in [QUEEN, ROOK, BISHOP].iter() {
            result.extend(self.generate_slider_moves_for_piece(*piece_type));
        }
        result
    }

    fn generate_slider_moves_for_piece(&self, piece_type: PieceType) -> Vec<Move64> {
        let mut result = vec![];
        let mut our_sliders = self.our_pieces(piece_type);
        iterate_squares!(our_sliders -> from_square {
            let mut moves = self.generate_slider_moves_for_piece_from_square(piece_type, from_square as SquareIndex64);
            iterate_squares!(moves -> to_square {
                debug_assert!(to_square < 64);
                result.push(Move64 { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: None });
            });
        });
        result
    }

    fn generate_slider_moves_for_piece_from_square(
        &self,
        piece_type: PieceType,
        from_square: SquareIndex64,
    ) -> Bitboard {
        let mut moves = 0;
        let blockers = self.bb_colours[WHITE as usize] | self.bb_colours[BLACK as usize];
        let pin_mask = self.pin_masks[from_square as usize];
        let attack_squares = match piece_type {
            QUEEN => QUEEN_ATTACK_SQUARES[from_square as usize],
            ROOK => ROOK_ATTACK_SQUARES[from_square as usize],
            BISHOP => BISHOP_ATTACK_SQUARES[from_square as usize],
            _ => panic!("Invalid piece type"),
        };
        let attack_squares_mask = attack_squares & pin_mask;

        if attack_squares_mask == 0 {
            return 0;
        }

        let directions = match piece_type {
            QUEEN => 0..8,
            ROOK => 0..4,
            BISHOP => 4..8,
            _ => panic!("Invalid piece type"),
        };

        for dir_index in directions {
            let dir = DIR_ALL_SLIDERS[dir_index];
            let potential_moves: Bitboard =
                SLIDER_DIRECTION_SQUARE[dir_index][from_square as usize] & pin_mask;

            if potential_moves == 0 {
                continue;
            }

            let mut square = square_mask64(from_square);
            while square != 0 {
                if dir > 0 {
                    square <<= dir;
                } else {
                    square >>= -dir;
                }

                if square & pin_mask == 0 {
                    break;
                }

                if square & blockers == 0 {
                    moves |= square;
                } else {
                    if square & self.bb_colours[self.side_to_move as usize] != 0 {
                        break;
                    }
                    moves |= square;
                    break;
                }
            }
        }

        moves
    }

    fn generate_king_moves(&self) -> Vec<Move64> {
        // TODO: Castling and check detection.
        let mut result = vec![];
        let mut our_king = self.our_pieces(KING);
        iterate_squares!(our_king -> from_square {
            let mut moves = KING_ATTACK_SQUARES[from_square as usize]
                & !(self.bb_colours[self.side_to_move as usize])
                & self.pin_masks[from_square as usize];
            iterate_squares!(moves -> to_square {
                debug_assert!(to_square < 64);
                result.push(Move64 { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: None });
            });
        });
        result
    }
}

/// Initialisation of helper data and functions.
impl MoveGenerator {
    #[inline]
    fn our_pieces(&self, piece_type: PieceType) -> Bitboard {
        self.bb_colours[self.side_to_move as usize] & self.bb_pieces[piece_type as usize]
    }
    pub fn init(&mut self, position: &Position0x88) {
        self.side_to_move = position.side_to_move;
        self.ep_square = square_mask0x88(position.ep_square);
        self.bb_colours = position.bb_colours;
        self.bb_pieces = position.bb_pieces;
        self.pin_masks = [Bitboard::MAX; 64];
        self.checkers = 0;
        let king_square = index0x88to64(position.king_squares[position.side_to_move as usize]);
        self.init_pin_masks_and_checkers(position, king_square)
            .unwrap();
    }

    /// Calculate masks for pinned pieces, and a bitboard of pieces giving check.
    /// TODO: This only adds sliding checkers, not knights or pawns.
    fn init_pin_masks_and_checkers(
        &mut self,
        position: &Position0x88,
        king_square: SquareIndex64,
    ) -> Result<SixtyFourBitboards, BitboardError> {
        self.pin_masks = [Bitboard::MAX; 64];

        for (dir_index, dir) in DIR_ALL_SLIDERS.iter().enumerate() {
            // Find the next opponent piece in the direction.
            // We remove the pawn on the e.p. capture square and treat it as one of our pieces later on.
            let opp_pieces_in_dir = (position.bb_colours[position.opposite_side() as usize]
                & !position.ep_capture_square_bb())
                & SLIDER_DIRECTION_SQUARE[dir_index][king_square as usize];

            if opp_pieces_in_dir == 0 {
                continue;
            }

            let next_opp_piece_square = if *dir > 0 {
                opp_pieces_in_dir.lowest_set_bit()
            } else {
                opp_pieces_in_dir.highest_set_bit()
            };

            // Check that it is a slider.
            let piece = position.get_piece64(next_opp_piece_square); // Can unwrap as we've checked it's a piece.
            let piece_type = piece_type(piece);
            if !is_slider(piece_type) || !slides_in_dir(piece_type, *dir) {
                continue;
            }

            // Find the squares between the king and it.
            let between = BETWEEN[king_square as usize][next_opp_piece_square as usize];

            let ours_between = (position.bb_colours[position.side_to_move as usize]
                | position.ep_capture_square_bb())
                & between;

            // 0 would be check, more than one there is no pin.
            let next_opp_piece_mask = square_mask64(next_opp_piece_square);
            if ours_between.count_ones() == 0 {
                self.checkers |= next_opp_piece_mask;
                continue;
            } else if ours_between.count_ones() != 1 {
                // Look for horizontal e.p. capture pin.
                if (ours_between & !position.ep_capture_square_bb()).count_ones() == 1 {
                    self.pin_masks[position.ep_capture_square_bb().to_single_square()? as usize] =
                        between | next_opp_piece_mask;
                }
                continue;
            }

            let pinned_square = ours_between.lowest_set_bit();

            // Add on the opponent's piece as that can be captured.
            self.pin_masks[pinned_square as usize] = between | next_opp_piece_mask;
        }

        // If it's e.p. work out the extra masks.
        if position.ep_square != 0 {
            let ep_index = index0x88to64(position.ep_square);
            let ep_square_bb = square_mask64(ep_index);
            let ep_potential_attackers =
                PAWN_ATTACK_SQUARES[position.opposite_side() as usize][ep_index as usize];
            let ep_attackers = position.bb_pieces[PAWN as usize]
                & position.bb_colours[position.side_to_move as usize]
                & ep_potential_attackers;

            // This works as we have calculated a pin mask for the capture pawn
            // even though it's not the colour to move.
            if ep_attackers != 0
                && self.pin_masks[position.ep_capture_square_bb().to_single_square()? as usize]
                    != Bitboard::MAX
                && (self.pin_masks[position.ep_capture_square_bb().to_single_square()? as usize]
                    & ep_square_bb
                    == 0)
            {
                // Check that e.p. capture pawn is pinned
                let mask = !ep_square_bb;
                let attacker_squares = ep_attackers.squares_from_bitboard();
                for sq in attacker_squares {
                    self.pin_masks[sq as usize] &= mask;
                }
            }
        }

        // Add non-sliding checkers.
        let knight_checkers = KNIGHT_ATTACK_SQUARES[king_square as usize]
            & position.bb_pieces[KNIGHT as usize]
            & position.bb_colours[position.opposite_side() as usize];

        let pawn_checkers = PAWN_ATTACK_SQUARES[position.side_to_move as usize]
            [king_square as usize]
            & position.bb_pieces[PAWN as usize]
            & position.bb_colours[position.opposite_side() as usize];

        self.checkers |= knight_checkers | pawn_checkers;

        Ok(self.pin_masks)
    }
}

#[cfg(test)]
mod test {
    use std::{env, fs};

    use crate::{
        fen::STARTPOS_FEN,
        position0x88::{index64to0x88, make_moves::MakeMoves, QUEEN, ROOK},
    };

    use super::*;

    #[test]
    fn test_generate_moves() {
        let mut pm = MoveGenerator::default();
        let pos: Position0x88 = STARTPOS_FEN.try_into().unwrap();
        pm.init(&pos);
        let moves = pm.generate_moves();
        assert_eq!(20, moves.len());
    }

    #[test]
    fn test_init_pin_masks_and_checkers() {
        let mut pm = MoveGenerator::default();
        let pos: Position0x88 = STARTPOS_FEN.try_into().unwrap();
        pm.init(&pos);
        assert_eq!(Bitboard::MAX, pm.pin_masks[3]);

        // Pinned on diagonal by queen.
        let pos: Position0x88 = "8/5K2/8/8/2R5/1q6/8/3k4 w - - 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(0x100804020000, pm.pin_masks[26]);

        // Not pinned.
        let pos: Position0x88 = "8/5K2/8/8/1qR5/1rn5/b7/3k4 w - - 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(Bitboard::MAX, pm.pin_masks[26]);

        let pos: Position0x88 = "6k1/8/8/2q5/8/4B3/8/6K1 w - - 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(0x408102000, pm.pin_masks[20]);

        let pos: Position0x88 = "6k1/8/2q5/8/8/4B3/8/6K1 w - - 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(Bitboard::MAX, pm.pin_masks[20]);

        // Diagonal e.p. pin.
        let pos: Position0x88 = "6k1/1q6/8/3pP3/8/4B3/8/7K w - d6 0 1".try_into().unwrap();
        pm.init(&pos);
        // Everything except the en-passent square.
        assert_eq!(0xfffff7ffffffffff, pm.pin_masks[36]);

        // Horizontal e.p. pin.
        let pos: Position0x88 = "6k1/8/8/1q1pP2K/8/4B3/8/8 w - d6 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(0xfffff7ffffffffff, pm.pin_masks[36]);

        // Vertical e.p. non-pin
        let pos: Position0x88 = "1r5k/8/8/Pp6/8/8/8/1K6 w - b6 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(Bitboard::MAX, pm.pin_masks[32]);

        let pos: Position0x88 = "r6k/8/8/Pp6/8/8/8/K7 w - - 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(0x101010101010100, pm.pin_masks[32]);

        let pos: Position0x88 = "2k5/6q1/8/2b5/8/8/8/6K1 w - - 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(0x40000400000000, pm.checkers);

        let pos: Position0x88 = "2k5/6q1/8/2r5/8/8/8/6K1 w - - 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(0x40000000000000, pm.checkers);

        // This is not a legal position, but it is useful for testing.
        let pos: Position0x88 = "3kr3/8/8/8/8/3n4/5p2/4K3 w - - 0 1".try_into().unwrap();
        pm.init(&pos);
        assert_eq!(0x1000000000082000, pm.checkers);
    }

    #[test]
    fn test_generate_pawn_captures() {
        let mut mg = MoveGenerator::default();
        let pos: Position0x88 = "rn1qkb1r/ppp1pppp/5n2/3p1b2/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"
            .try_into()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_pawn_captures();
        assert_eq!(2, moves.len());
        assert_eq!(28, moves[0].from_index);
        assert_eq!(35, moves[0].to_index);
    }

    #[test]
    fn test_generate_pawn_pushes() {
        let mut mg = MoveGenerator::default();
        let pos: Position0x88 = "rn1qkb1r/ppp1pppp/5n2/3p1b2/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"
            .try_into()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_pawn_pushes();
        assert_eq!(13, moves.len());

        let pos: Position0x88 = "rn1qk2r/ppp2ppp/4pn2/b2p1b2/3PP3/2P5/PP3PPP/RNBQKBNR w KQkq - 0 1"
            .try_into()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_pawn_pushes();
        assert_eq!(11, moves.len());
    }

    #[test]
    fn test_generate_pawn_pushes_with_promotion() {
        let mut mg = MoveGenerator::default();
        let pos: Position0x88 = "8/PPPPPPPP/8/8/8/8/8/8 w - - 0 1".try_into().unwrap();
        mg.init(&pos);
        let moves = mg.generate_pawn_pushes();
        assert_eq!(32, moves.len());
        assert_eq!(Some(QUEEN), moves[0].queening_piece);
        assert_eq!(Some(ROOK), moves[1].queening_piece);
    }

    #[test]
    fn test_generate_knight_moves() {
        let mut mg = MoveGenerator::default();
        let pos: Position0x88 = "rn1qkb1r/ppp1pppp/5n2/3p1b2/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"
            .try_into()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_knight_moves();
        assert_eq!(6, moves.len());
        assert_eq!(1, moves[0].from_index);
        assert_eq!(11, moves[0].to_index);
        assert_eq!(1, moves[1].from_index);
        assert_eq!(16, moves[1].to_index);
        assert_eq!(1, moves[2].from_index);
        assert_eq!(18, moves[2].to_index);
    }

    #[test]
    fn test_generate_slider_moves_for_piece_from_square() {
        let mut mg = MoveGenerator::default();
        let pos: Position0x88 =
            "r2qkb1r/1p2pppp/2n2n2/p1ppPbB1/3P4/2N5/1PP2PPP/R2QKBNR w KQkq - 0 1"
                .try_into()
                .unwrap();
        mg.init(&pos);
        let moves = mg.generate_slider_moves_for_piece_from_square(ROOK, 0);
        assert_eq!(0x101010106, moves);

        let pos: Position0x88 = "5k2/8/8/8/8/8/8/rR3K2 w - - 0 1".try_into().unwrap();
        mg.init(&pos);
        let moves = mg.generate_slider_moves_for_piece_from_square(ROOK, 1);
        assert_eq!(0x1d, moves);
    }

    #[test]
    fn test_generate_king_moves() {
        let mut mg = MoveGenerator::default();
        let pos: Position0x88 =
            "r2qkb1r/1p2pppp/2n2n2/p1ppPbB1/3P4/2N5/1PP2PPP/R2QKBNR w KQkq - 0 1"
                .try_into()
                .unwrap();
        mg.init(&pos);
        let moves = mg.generate_king_moves();
        assert_eq!(2, moves.len());
        assert_eq!(4, moves[0].from_index);
        assert_eq!(11, moves[0].to_index);
        assert_eq!(4, moves[1].from_index);
        assert_eq!(12, moves[1].to_index);
    }

    #[test]
    fn test_move_gen_from_perft() {
        let cwd = env::current_dir().unwrap();
        let root = cwd.ancestors().next().unwrap();
        let path = root.join("tests/perft.txt");
        let perft_contents = fs::read_to_string(path).unwrap();
        let perft_lines = perft_contents.split("\n");
        let mut line_no = 1;
        let mut move_gen = MoveGenerator::default();
        for line in perft_lines {
            if line.trim() == "" {
                break;
            }
            let line_parts: Vec<&str> = line.split(',').collect();

            let fen = line_parts[0];

            // println!("Creating from FEN: {}", fen);
            let mut position: Position0x88 = fen.try_into().unwrap();

            println!("{}: {}", line_no, fen);
            line_no += 1;
            let perft1 = perft(&mut position, &mut move_gen, 1);
            // let perft2 = perft(&mut position, 2);
            // let perft3 = perft(&mut position, 3);
            // let perft4 = perft(&mut position, 4);
            let target1: usize = line_parts[1].parse().unwrap();
            // let target2: usize = line_parts[2].parse().unwrap();
            // let target3: usize = line_parts[3].parse().unwrap();
            // let target4: usize = line_parts[4].parse().unwrap();
            assert_eq!(target1, perft1);
            // assert_eq!(target2, perft2);
            // assert_eq!(target3, perft3);
            // assert_eq!(target4, perft4);

            // println!("{:#?}", position);

            // println!("FEN: {}", fen);

            //for i in &moves {
            // print!("{} ", i.to_string());
            //}
            // println!("");
        }
    }

    fn perft(position: &mut Position0x88, move_gen: &mut MoveGenerator, depth: u8) -> usize {
        if depth == 0 {
            return 1;
        }

        let mut count = 0;
        move_gen.init(position);
        let moves = move_gen.generate_moves();
        for m in moves {
            let undo = position.make_move(
                index64to0x88(m.from_index),
                index64to0x88(m.to_index),
                m.queening_piece,
            );
            count += perft(position, move_gen, depth - 1);
            position.undo_move(undo);
        }
        count
    }
}
