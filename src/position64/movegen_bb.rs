use std::vec;

use crate::{
    bitboards::*,
    iterate_squares,
    magic::SQUARE_MAGICS,
    position::{Castling, SetPosition, PIECE_COUNT},
    position64::PieceType,
};

use super::{
    moves::{GenerateMoves, Move},
    CastlingRightsBool, Colour, Position64,
};

const PROMOTION_PIECES: [PieceType; 4] = [
    PieceType::Queen,
    PieceType::Rook,
    PieceType::Knight,
    PieceType::Bishop,
];

const SLIDERS: [PieceType; 3] = [PieceType::Queen, PieceType::Rook, PieceType::Bishop];

// Data on castling. To square, in-between square (mustn't be attacked), rook square.
const CASTLING_DATA: [(SquareIndex64, SquareIndex64, SquareIndex64); 4] =
    [(6, 5, 7), (2, 3, 0), (62, 61, 63), (58, 59, 56)];

#[derive(Debug)]
pub struct MoveGenerator {
    pin_masks: SixtyFourBitboards,
    checkers: Bitboard,
    king_squares: [SquareIndex64; 2],

    // Minimal position data.
    side_to_move: Colour,
    ep_square: Bitboard,
    bb_pieces: [Bitboard; 7],
    bb_colours: [Bitboard; 3],
    castling: CastlingRightsBool,
}

impl Default for MoveGenerator {
    fn default() -> Self {
        Self {
            pin_masks: [Bitboard::MAX; 64],
            checkers: Default::default(),
            king_squares: [0; 2],
            side_to_move: Default::default(),
            ep_square: Default::default(),
            bb_pieces: Default::default(),
            bb_colours: Default::default(),
            castling: Default::default(),
        }
    }
}

impl GenerateMoves for MoveGenerator {
    fn generate_moves(&self) -> Vec<Move> {
        let mut result = vec![];
        if self.checkers != 0 {
            result.extend(self.generate_check_evasions());
        } else {
            result.extend(self.generate_pawn_captures());
            result.extend(self.generate_slider_moves());
            result.extend(self.generate_knight_moves());
            result.extend(self.generate_pawn_pushes());
            result.extend(self.generate_castling());
            result.extend(self.generate_non_castling_king_moves(0));
        }

        result
    }
}

// Generating functions.
impl MoveGenerator {
    pub fn generate_check_evasions(&self) -> Vec<Move> {
        let mut result = vec![];

        if self.checkers.count_ones() == 1 {
            // Generate check-evading captures
            result.extend(self.generate_checking_piece_captures());
            result.extend(self.generate_non_castling_king_moves(self.checkers));

            result.extend(self.generate_interpositions());
        } else {
            // Double check. Only king moves but allow capture of one of the checking pieces.
            result.extend(self.generate_non_castling_king_moves(0));
        }

        result
    }

    fn generate_interpositions(&self) -> Vec<Move> {
        // Generate interpositions.
        let intermediate_squares = BETWEEN[self.checkers.to_single_square().unwrap() as usize]
            [self.king_squares[self.side_to_move as usize] as usize];

        if intermediate_squares == 0 {
            return vec![];
        }

        let mut possible_from_squares = 0;

        // Need to copy it as we need it later and the iterator empties it.
        let mut temp_intermediate_squares = intermediate_squares;

        iterate_squares!(temp_intermediate_squares -> square {
            possible_from_squares |= self.possible_attackers(square as SquareIndex64);
        });

        // Just remove both kings, it doesn't matter.
        possible_from_squares &= !self.bb_pieces[PieceType::King as usize];

        if possible_from_squares == 0 {
            return vec![];
        }

        // TODO: This is rubbish at the moment as it's creating all moves and then filtering them.
        // We should be able just to generate the required moves.

        let mut temp_result = vec![];
        temp_result.extend(self.generate_pawn_captures());
        temp_result.extend(self.generate_slider_moves());
        temp_result.extend(self.generate_knight_moves());
        temp_result.extend(self.generate_pawn_pushes());

        let result: Vec<Move> = temp_result
            .into_iter()
            .filter(|m| {
                intermediate_squares & Bitboard::from_single_square(m.to_index) != 0
                    && possible_from_squares & Bitboard::from_single_square(m.from_index) != 0
            })
            .collect();

        result
    }

    /// Generates check evading captures. This is captures of the piece giving check.
    fn generate_checking_piece_captures(&self) -> Vec<Move> {
        let mut result = vec![];

        let all_pieces =
            self.bb_colours[Colour::White as usize] | self.bb_colours[Colour::Black as usize];

        let targets = self.checkers;

        debug_assert!(targets.count_ones() == 1);

        // Get a bitboard of all possible pieces that could potentially capture the checker.
        let square = targets.to_single_square().unwrap();

        let possible_attackers = self.possible_attackers(square);

        // Check by piece type to find attackers (that can move).
        for piece_type_index in 1..PIECE_COUNT {
            let piece_type: PieceType = piece_type_index.into();

            if piece_type == PieceType::King && self.is_attacked_by_opposite(square, 0) {
                continue;
            }

            let mut attackers = possible_attackers & self.bb_pieces[piece_type_index as usize];

            iterate_squares!(attackers -> from_square {

                let attack_squares = if piece_type == PieceType::Pawn {
                    PAWN_ATTACK_SQUARES[self.side_to_move as usize][from_square as usize]
                } else {
                    PIECE_ATTACK_SQUARES[piece_type_index][from_square as usize]
                };

                let all_targets = if self.ep_square != 0 && piece_type == PieceType::Pawn && (self.ep_capture_square() & targets) != 0 {
                    targets | self.ep_square
                } else {
                    targets
                };

                let captures = attack_squares
                    & all_targets
                    & self.pin_masks[from_square as usize];

                if captures != 0 && (!is_slider(piece_type) || (all_pieces & BETWEEN[square as usize][from_square as usize]) == 0) {
                    if piece_type == PieceType::Pawn {
                        if captures == self.ep_square {
                            let to_square = self.ep_square.to_single_square();
                            debug_assert!(to_square.is_ok());
                            result.extend(self.create_pawn_moves(from_square as SquareIndex64, to_square.unwrap() as SquareIndex64));
                        } else {
                            result.extend(self.create_pawn_moves(from_square as SquareIndex64, square as SquareIndex64));
                        }
                    } else {
                        result.push(Move::new(
                            from_square as SquareIndex64,
                            square as SquareIndex64,
                            None,
                        ));
                    }
                }
            });
        }
        result
    }

    /// Get a bitboard of all possible attackers to a square.
    /// Queen squares cover all possible attackers apart from knights including pawns and king.
    #[inline]
    const fn possible_attackers(&self, square: SquareIndex64) -> u64 {
        (QUEEN_ATTACK_SQUARES[square as usize] | KNIGHT_ATTACK_SQUARES[square as usize])
            & self.bb_colours[self.side_to_move as usize]
    }

    pub fn generate_pawn_captures(&self) -> Vec<Move> {
        let mut result = vec![];
        let mut our_pawns =
            self.bb_colours[self.side_to_move as usize] & self.bb_pieces[PieceType::Pawn as usize];
        iterate_squares!(our_pawns -> from_square {
            let mut captures = PAWN_ATTACK_SQUARES[self.side_to_move as usize][from_square as usize]
                & (self.bb_colours[self.side_to_move.opposite() as usize] | self.ep_square) & self.pin_masks[from_square as usize];

            iterate_squares!(captures -> to_square {
                debug_assert!(to_square < 64);
                result.extend(self.create_pawn_moves(from_square as SquareIndex64, to_square as SquareIndex64));
            });

        });
        result
    }

    fn generate_pawn_pushes(&self) -> Vec<Move> {
        let mut result = vec![];

        let mut our_pawns = self.our_pieces(PieceType::Pawn);
        let mut our_pawns_mask = our_pawns;

        iterate_squares!(our_pawns_mask -> from_square {
            let f = file(from_square as SquareIndex64) as usize;
            let pin_mask_slice = &self.pin_masks[from_square as usize] & FILE_MASK[f];
            // We don't really want an if statement here, so it would be good to find a better way.
            if pin_mask_slice.count_ones() == 1 {
                our_pawns &= !Bitboard::from_single_square(from_square as SquareIndex64);
            }
        });

        let move_dir = PAWN_PUSH_DIRECTIONS[self.side_to_move as usize];

        let rank_to_mask = match self.side_to_move {
            Colour::White => RANK_MASK[2],
            Colour::Black => RANK_MASK[5],
            _ => panic!("Invalid side to move"),
        };

        for i in 0..2 {
            if i == 1 {
                our_pawns &= rank_to_mask;
            }

            match self.side_to_move {
                Colour::White => our_pawns <<= 8,
                Colour::Black => our_pawns >>= 8,
                _ => panic!("Invalid side to move"),
            }
            our_pawns &= !(self.bb_colours[Colour::White as usize]
                | self.bb_colours[Colour::Black as usize]);

            if our_pawns == 0 {
                break;
            }

            let mut to_squares = our_pawns;

            iterate_squares!(to_squares -> to_square {
                let from_square = to_square as i8 + ((1 + i) * -move_dir);
                debug_assert!(from_square >= 0 && from_square < 64);
                result.extend(self.create_pawn_moves(from_square as SquareIndex64, to_square as SquareIndex64));
            });
        }

        result
    }

    fn create_pawn_moves(&self, from_square: SquareIndex64, to_square: SquareIndex64) -> Vec<Move> {
        if to_square < 8 || to_square > 55 {
            let mut result = vec![];
            for piece_type in PROMOTION_PIECES {
                result.push(Move {
                    from_index: from_square,
                    to_index: to_square,
                    queening_piece: Some(piece_type),
                });
            }
            result
        } else {
            vec![Move {
                from_index: from_square,
                to_index: to_square,
                queening_piece: None,
            }]
        }
    }

    fn generate_knight_moves(&self) -> Vec<Move> {
        let mut result = vec![];
        let mut our_knights = self.our_pieces(PieceType::Knight);
        iterate_squares!(our_knights -> from_square {
            let mut moves = KNIGHT_ATTACK_SQUARES[from_square as usize]
                & !(self.bb_colours[self.side_to_move as usize])
                & self.pin_masks[from_square as usize];
            iterate_squares!(moves -> to_square {
                debug_assert!(to_square < 64);
                result.push(Move { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: None });
            });
        });
        result
    }

    fn generate_slider_moves(&self) -> Vec<Move> {
        let mut result = vec![];
        for piece_type in SLIDERS {
            result.extend(self.generate_slider_moves_for_piece(piece_type));
        }
        result
    }

    fn generate_slider_moves_for_piece(&self, piece_type: PieceType) -> Vec<Move> {
        let mut result = vec![];
        let mut our_sliders = self.our_pieces(piece_type);
        iterate_squares!(our_sliders -> from_square {
            let mut moves = self.generate_slider_moves_for_piece_from_square(piece_type, from_square as SquareIndex64);
            iterate_squares!(moves -> to_square {
                debug_assert!(to_square < 64);
                result.push(Move { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: None });
            });
        });
        result
    }

    fn generate_slider_moves_for_piece_from_square(
        &self,
        piece_type: PieceType,
        from_square: SquareIndex64,
    ) -> Bitboard {
        let mut moves;
        let blockers =
            self.bb_colours[Colour::White as usize] | self.bb_colours[Colour::Black as usize];
        let pin_mask = self.pin_masks[from_square as usize];

        match piece_type {
            PieceType::Rook => {
                moves = SQUARE_MAGICS[from_square as usize].rook_attacks(blockers) & pin_mask
            }
            PieceType::Bishop => {
                moves = SQUARE_MAGICS[from_square as usize].bishop_attacks(blockers) & pin_mask
            }
            PieceType::Queen => {
                moves = (SQUARE_MAGICS[from_square as usize].rook_attacks(blockers)
                    | SQUARE_MAGICS[from_square as usize].bishop_attacks(blockers))
                    & pin_mask
            }
            _ => panic!("Not a slider"),
        }

        moves &= !(self.bb_colours[self.side_to_move as usize]);

        moves
    }

    fn generate_non_castling_king_moves(&self, exclude_squares: Bitboard) -> Vec<Move> {
        let mut result = vec![];
        let mut our_king = self.our_pieces(PieceType::King);

        iterate_squares!(our_king -> from_square {
            let mut moves = KING_ATTACK_SQUARES[from_square as usize]
                & !(self.bb_colours[self.side_to_move as usize])
                & self.pin_masks[from_square as usize]
                & !exclude_squares;

            iterate_squares!(moves -> to_square {
                debug_assert!(to_square < 64);

                if !self.is_attacked_by_opposite(to_square as SquareIndex64, our_king) {
                    result.push(Move { from_index: from_square as SquareIndex64, to_index: to_square as SquareIndex64, queening_piece: None });
                }

            });
        });
        result
    }

    fn generate_castling(&self) -> Vec<Move> {
        let mut result = vec![];

        if self.is_check() {
            return result;
        }

        let options = match self.side_to_move {
            Colour::White => [Castling::WhiteKingSide, Castling::WhiteQueenSide],
            Colour::Black => [Castling::BlackKingSide, Castling::BlackQueenSide],
            Colour::None => panic!("Invalid side"),
        };

        for castling_type in &options {
            if !self.castling.0[*castling_type as usize] {
                continue;
            }

            let (to_square, through_square, rook_square) = CASTLING_DATA[*castling_type as usize];
            debug_assert!(
                self.bb_colours[self.side_to_move as usize]
                    & self.bb_pieces[PieceType::Rook as usize]
                    & Bitboard::from_single_square(rook_square)
                    != 0
            );

            let king_square = self.king_squares[self.side_to_move as usize];

            // Check that there are no pieces between the king and the rook.
            if BETWEEN[king_square as usize][rook_square as usize]
                & (self.bb_colours[Colour::White as usize]
                    | self.bb_colours[Colour::Black as usize])
                != 0
            {
                continue;
            }

            // Check that the through square and the to square are not attacked.
            if self.is_attacked_by_opposite(to_square, 0)
                || self.is_attacked_by_opposite(through_square, 0)
            {
                continue;
            }

            result.push(Move {
                from_index: king_square,
                to_index: to_square,
                queening_piece: None,
            });
        }

        result
    }

    fn ep_capture_square(&self) -> Bitboard {
        match self.side_to_move {
            Colour::White => self.ep_square >> 8,
            Colour::Black => self.ep_square << 8,
            Colour::None => 0,
        }
    }

    pub fn is_check(&self) -> bool {
        self.checkers != 0
    }

    fn is_attacked_by_opposite(&self, square: SquareIndex64, ignore_squares: Bitboard) -> bool {
        let potential_attackers = self.bb_colours[self.side_to_move.opposite() as usize];
        let mut all_pieces =
            self.bb_colours[Colour::White as usize] | self.bb_colours[Colour::Black as usize];

        if ignore_squares != 0 {
            all_pieces &= !ignore_squares;
        }

        // Do the quick ones first.
        // Knights.
        if potential_attackers
            & KNIGHT_ATTACK_SQUARES[square as usize]
            & self.bb_pieces[PieceType::Knight as usize]
            != 0
        {
            return true;
        }
        // King.
        if potential_attackers
            & KING_ATTACK_SQUARES[square as usize]
            & self.bb_pieces[PieceType::King as usize]
            != 0
        {
            return true;
        }
        // Pawns. Note this is the side to move pawn attacks as we are looking out from the square.
        if potential_attackers
            & PAWN_ATTACK_SQUARES[self.side_to_move as usize][square as usize]
            & self.bb_pieces[PieceType::Pawn as usize]
            != 0
        {
            return true;
        }

        // Now the sliders.
        let potential_slider_attackers = potential_attackers
            & QUEEN_ATTACK_SQUARES[square as usize]
            & (self.bb_pieces[PieceType::Queen as usize]
                | self.bb_pieces[PieceType::Rook as usize]
                | self.bb_pieces[PieceType::Bishop as usize]);
        for (dir_index, dir) in DIR_ALL_SLIDERS.iter().enumerate() {
            let slider_dir_squares = SLIDER_DIRECTION_SQUARE[dir_index][square as usize];
            let opposite_pieces_in_dir = slider_dir_squares & potential_slider_attackers;

            if opposite_pieces_in_dir == 0 {
                continue;
            }
            let nearest_piece = if *dir > 0 {
                opposite_pieces_in_dir.trailing_zeros()
            } else {
                63 - opposite_pieces_in_dir.leading_zeros()
            };

            if all_pieces & BETWEEN[square as usize][nearest_piece as usize] == 0 {
                let mask = Bitboard::from_single_square(nearest_piece as SquareIndex64);
                if is_diagonal(*dir) {
                    if (self.bb_pieces[PieceType::Bishop as usize]
                        | self.bb_pieces[PieceType::Queen as usize])
                        & mask
                        != 0
                    {
                        return true;
                    }
                } else {
                    if (self.bb_pieces[PieceType::Rook as usize]
                        | self.bb_pieces[PieceType::Queen as usize])
                        & mask
                        != 0
                    {
                        return true;
                    }
                }
            }
        }

        false
    }
}

/// Initialisation of helper data and functions.
impl MoveGenerator {
    #[inline]
    fn our_pieces(&self, piece_type: PieceType) -> Bitboard {
        self.bb_colours[self.side_to_move as usize] & self.bb_pieces[piece_type as usize]
    }
    pub fn init(&mut self, position: &Position64) {
        self.side_to_move = position.side_to_move;
        self.ep_square = position.ep_square;
        self.castling = position.castling_rights;

        self.bb_colours = position.bb_colours;
        self.bb_pieces = position.bb_pieces;
        self.pin_masks = [Bitboard::MAX; 64];
        self.checkers = 0;
        self.init_king_squares(position);
        self.init_pin_masks_and_checkers(position, self.king_squares[self.side_to_move as usize])
            .unwrap();
    }

    fn init_king_squares(&mut self, position: &Position64) {
        self.king_squares = [0; 2];
        for (colour, king_square) in self.king_squares.iter_mut().enumerate() {
            let king_bb =
                position.bb_pieces[PieceType::King as usize] & position.bb_colours[colour];
            if king_bb.count_ones() != 1 {
                panic!("Invalid position: {} ", position.to_string());
            }
            *king_square = king_bb.to_single_square().unwrap();
        }
    }

    /// Calculate masks for pinned pieces, and a bitboard of pieces giving check.
    fn init_pin_masks_and_checkers(
        &mut self,
        position: &Position64,
        king_square: SquareIndex64,
    ) -> Result<SixtyFourBitboards, BitboardError> {
        self.pin_masks = [Bitboard::MAX; 64];

        for (dir_index, dir) in DIR_ALL_SLIDERS.iter().enumerate() {
            // Find the next opponent piece in the direction.
            // We remove the pawn on the e.p. capture square and treat it as one of our pieces later on.
            let opp_pieces_in_dir = (position.bb_colours
                [position.side_to_move.opposite() as usize]
                & !self.ep_capture_square())
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
            let piece = position.square_piece(next_opp_piece_square); // Can unwrap as we've checked it's a piece.
            let piece_type = piece.piece_type;
            if !is_slider(piece_type) || !slides_in_dir(piece_type, *dir) {
                continue;
            }

            // Find the squares between the king and it.
            let between = BETWEEN[king_square as usize][next_opp_piece_square as usize];

            let ours_between = (position.bb_colours[position.side_to_move as usize]
                | self.ep_capture_square())
                & between;

            // 0 would be check, more than one there is no pin.
            let next_opp_piece_mask = square_mask64(next_opp_piece_square);
            if ours_between.count_ones() == 0 {
                self.checkers |= next_opp_piece_mask;
                continue;
            } else if ours_between.count_ones() != 1 {
                // Look for horizontal e.p. capture pin.
                if dir.abs() == 1 && (ours_between & !self.ep_capture_square()).count_ones() == 1 {
                    self.pin_masks[self.ep_capture_square().to_single_square()? as usize] =
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
            let ep_index = position.ep_square.to_single_square()?;
            let ep_square_bb = square_mask64(ep_index);
            let ep_potential_attackers =
                PAWN_ATTACK_SQUARES[position.side_to_move.opposite() as usize][ep_index as usize];
            let ep_attackers = position.bb_pieces[PieceType::Pawn as usize]
                & position.bb_colours[position.side_to_move as usize]
                & ep_potential_attackers;

            // This works as we have calculated a pin mask for the capture pawn
            // even though it's not the colour to move.
            if ep_attackers != 0
                && self.pin_masks[self.ep_capture_square().to_single_square()? as usize]
                    != Bitboard::MAX
                && (self.pin_masks[self.ep_capture_square().to_single_square()? as usize]
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
            & position.bb_pieces[PieceType::Knight as usize]
            & position.bb_colours[position.side_to_move.opposite() as usize];

        let pawn_checkers = PAWN_ATTACK_SQUARES[position.side_to_move as usize]
            [king_square as usize]
            & position.bb_pieces[PieceType::Pawn as usize]
            & position.bb_colours[position.side_to_move.opposite() as usize];

        self.checkers |= knight_checkers | pawn_checkers;

        Ok(self.pin_masks)
    }
}

#[cfg(test)]
mod test {

    use std::{env, fs};

    use crate::{fen::STARTPOS_FEN, perft::Perft, position::Square};

    use super::*;

    #[test]
    fn test_generate_moves() {
        let mut pm = MoveGenerator::default();
        let pos: Position64 = STARTPOS_FEN.parse().unwrap();
        pm.init(&pos);
        let moves = pm.generate_moves();
        assert_eq!(20, moves.len());

        let pos: Position64 = "2b1kbnB/rp1qp3/3p3p/2pP1pp1/pnP3P1/PP2P2P/4QP2/RN2KBNR w KQ c6 0 1"
            .parse()
            .unwrap();
        pm.init(&pos);
        let moves = pm.generate_moves();
        assert_eq!(29, moves.len());

        let pos: Position64 = "rnbqk2r/1p1p4/1b2p1pn/pPp2p1p/3PP3/5P2/P1P2KPP/RNBQ1BNR w kq c6 0 1"
            .parse()
            .unwrap();
        pm.init(&pos);
        let moves = pm.generate_moves();
        assert_eq!(37, moves.len());
    }

    #[test]
    fn test_generate_check_evasions() {
        let mut pm = MoveGenerator::default();
        let pos: Position64 = "8/8/r6b/P3p2N/1nk3pp/K1PBP3/4b3/B7 b - -".parse().unwrap();
        pm.init(&pos);
        let moves = pm.generate_check_evasions();
        assert_eq!(5, moves.len());

        let pos: Position64 = "8/8/k7/2R5/N2p2PP/3K2b1/8/5b2 w - -".parse().unwrap();
        pm.init(&pos);
        let moves = pm.generate_check_evasions();
        assert_eq!(4, moves.len());

        let pos: Position64 = "8/8/1k2N3/1p2P3/1p1p2B1/3K1r2/3n4/7q w - -"
            .parse()
            .unwrap();
        pm.init(&pos);
        let moves = pm.generate_check_evasions();
        assert_eq!(5, moves.len());

        let pos: Position64 = "r2n3r/1bNk2pp/6P1/pP3p2/3pPq1P/1P1PB2R/2P3p1/Q3bKN1 w - -"
            .parse()
            .unwrap();
        pm.init(&pos);
        let moves = pm.generate_check_evasions();
        assert_eq!(3, moves.len());

        let pos: Position64 = "rb6/5b2/1p2r3/p1k1P3/PpPPp3/2R4P/8/1N1K2R1 b - d3 0 1"
            .parse()
            .unwrap();
        pm.init(&pos);
        let moves = pm.generate_check_evasions();
        assert_eq!(3, moves.len());
        // We had e4d4 instead.
        let moves_alg: Vec<String> = moves.iter().map(|mv| mv.to_string()).collect();
        assert!(moves_alg.contains(&"e4d3".to_string()));
    }

    #[test]
    fn test_generate_interpositions() {
        let mut pm = MoveGenerator::default();
        let pos: Position64 = "4k2R/5r2/2N1r3/1P1n2p1/8/K5p1/8/8 b - - 0 1"
            .parse()
            .unwrap();
        pm.init(&pos);
        let moves = pm.generate_interpositions();
        assert_eq!(1, moves.len());
        let mv1 = moves[0];
        assert_eq!(Square::F7 as SquareIndex64, mv1.from_index);
        assert_eq!(Square::F8 as SquareIndex64, mv1.to_index);
    }

    #[test]
    fn test_init_pin_masks_and_checkers() {
        let mut pm = MoveGenerator::default();
        let pos: Position64 = STARTPOS_FEN.parse().unwrap();
        pm.init(&pos);
        assert_eq!(Bitboard::MAX, pm.pin_masks[3]);

        // Pinned on diagonal by queen.
        let pos: Position64 = "8/5K2/8/8/2R5/1q6/8/3k4 w - - 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(0x100804020000, pm.pin_masks[26]);

        // Not pinned.
        let pos: Position64 = "8/5K2/8/8/1qR5/1rn5/b7/3k4 w - - 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(Bitboard::MAX, pm.pin_masks[26]);

        let pos: Position64 = "6k1/8/8/2q5/8/4B3/8/6K1 w - - 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(0x408102000, pm.pin_masks[20]);

        let pos: Position64 = "6k1/8/2q5/8/8/4B3/8/6K1 w - - 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(Bitboard::MAX, pm.pin_masks[20]);

        // Diagonal e.p. pin.
        let pos: Position64 = "6k1/1q6/8/3pP3/8/4B3/8/7K w - d6 0 1".parse().unwrap();
        pm.init(&pos);
        // Everything except the en-passant square.
        assert_eq!(0xfffff7ffffffffff, pm.pin_masks[36]);

        // Horizontal e.p. pin.
        let pos: Position64 = "6k1/8/8/1q1pP2K/8/4B3/8/8 w - d6 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(0xfffff7ffffffffff, pm.pin_masks[36]);

        // Vertical e.p. non-pin
        let pos: Position64 = "1r5k/8/8/Pp6/8/8/8/1K6 w - b6 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(Bitboard::MAX, pm.pin_masks[32]);

        let pos: Position64 = "r6k/8/8/Pp6/8/8/8/K7 w - - 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(0x101010101010100, pm.pin_masks[32]);

        let pos: Position64 = "2k5/6q1/8/2b5/8/8/8/6K1 w - - 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(0x40000400000000, pm.checkers);

        let pos: Position64 = "2k5/6q1/8/2r5/8/8/8/6K1 w - - 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(0x40000000000000, pm.checkers);

        // This is not a legal position, but it is useful for testing.
        let pos: Position64 = "3kr3/8/8/8/8/3n4/5p2/4K3 w - - 0 1".parse().unwrap();
        pm.init(&pos);
        assert_eq!(0x1000000000082000, pm.checkers);

        let pos: Position64 = "rnbqk2r/1p1p4/1b2p1pn/pPp2p1p/3PP3/5P2/P1P2KPP/RNBQ1BNR w kq c6 0 1"
            .parse()
            .unwrap();
        pm.init(&pos);
        assert_eq!(0xffffffffffffffff, pm.pin_masks[33]);
    }

    #[test]
    fn test_generate_pawn_captures() {
        let mut mg = MoveGenerator::default();
        let pos: Position64 = "rn1qkb1r/ppp1pppp/5n2/3p1b2/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"
            .parse()
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
        let pos: Position64 = "rn1qkb1r/ppp1pppp/5n2/3p1b2/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"
            .parse()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_pawn_pushes();
        assert_eq!(13, moves.len());

        let pos: Position64 = "rn1qk2r/ppp2ppp/4pn2/b2p1b2/3PP3/2P5/PP3PPP/RNBQKBNR w KQkq - 0 1"
            .parse()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_pawn_pushes();
        assert_eq!(11, moves.len());

        let pos: Position64 = "4r3/q4r2/5k2/p3p3/PB1Q2pp/1PnPpP1N/1RKN2P1/5B1R b - - 0 1"
            .parse()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_pawn_pushes();
        assert_eq!(2, moves.len());

        let pos: Position64 = "rn1q1bnr/3kpp2/2pp2pp/pp6/1P2b2N/2PQ3P/P2PPP2/RNB1KB1R b KQ - 0 1"
            .parse()
            .unwrap();
        mg.init(&pos);
        assert_eq!(9, mg.generate_pawn_pushes().len());
    }

    #[test]
    fn test_generate_pawn_pushes_with_promotion() {
        let mut mg = MoveGenerator::default();
        let pos: Position64 = "8/PPPPPPPP/8/8/8/8/8/k6K w - - 0 1".parse().unwrap();
        mg.init(&pos);
        let moves = mg.generate_pawn_pushes();
        assert_eq!(32, moves.len());
        assert_eq!(PieceType::Queen, moves[0].queening_piece.unwrap());
        assert_eq!(PieceType::Rook, moves[1].queening_piece.unwrap());
    }

    #[test]
    fn test_generate_knight_moves() {
        let mut mg = MoveGenerator::default();
        let pos: Position64 = "rn1qkb1r/ppp1pppp/5n2/3p1b2/3PP3/8/PPP2PPP/RNBQKBNR w KQkq - 0 1"
            .parse()
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
        let pos: Position64 = "r2qkb1r/1p2pppp/2n2n2/p1ppPbB1/3P4/2N5/1PP2PPP/R2QKBNR w KQkq - 0 1"
            .parse()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_slider_moves_for_piece_from_square(PieceType::Rook, 0);
        assert_eq!(0x101010106, moves);

        let pos: Position64 = "5k2/8/8/8/8/8/8/rR3K2 w - - 0 1".parse().unwrap();
        mg.init(&pos);
        let moves = mg.generate_slider_moves_for_piece_from_square(PieceType::Rook, 1);
        assert_eq!(0x1d, moves);

        let pos: Position64 = "3q4/8/8/8/8/8/8/K6k b - - 0 1".parse().unwrap();
        mg.init(&pos);
        let moves = mg.generate_slider_moves_for_piece_from_square(PieceType::Queen, 59);
        assert_eq!(0xf71c2a4988080808, moves);
    }

    #[test]
    fn test_generate_king_moves() {
        let mut mg = MoveGenerator::default();
        let pos: Position64 = "r2qkb1r/1p2pppp/2n2n2/p1ppPbB1/3P4/2N5/1PP2PPP/R2QKBNR w KQkq - 0 1"
            .parse()
            .unwrap();
        mg.init(&pos);
        let moves = mg.generate_non_castling_king_moves(0);
        assert_eq!(2, moves.len());
        assert_eq!(4, moves[0].from_index);
        assert_eq!(11, moves[0].to_index);
        assert_eq!(4, moves[1].from_index);
        assert_eq!(12, moves[1].to_index);
    }

    #[test]
    fn test_generate_castles() {
        let mut mg = MoveGenerator::default();
        let pos: Position64 = "r3k2r/8/8/8/8/8/8/R3K2R w KQkq - 0 1".parse().unwrap();
        mg.init(&pos);
        let moves = mg.generate_castling();
        assert_eq!(2, moves.len());

        // Change castling rights.
        let pos: Position64 = "r3k2r/8/8/8/8/8/8/R3K2R w Kkq - 0 1".parse().unwrap();
        mg.init(&pos);
        let moves = mg.generate_castling();
        assert_eq!(1, moves.len());
        assert_eq!(4, moves[0].from_index);
        assert_eq!(6, moves[0].to_index);

        // To squares attacked.
        let pos: Position64 = "r3k2r/8/8/2q5/8/8/8/R3K2R w KQkq - 0 1".parse().unwrap();
        mg.init(&pos);
        let moves = mg.generate_castling();
        assert_eq!(0, moves.len());
    }

    #[test]
    fn test_is_attacked_by_opposite() {
        let mut mg = MoveGenerator::default();
        let pos: Position64 = "r3k2r/8/8/2q5/8/8/8/R3K2R w KQkq - 0 1".parse().unwrap();
        mg.init(&pos);
        assert_eq!(true, mg.is_attacked_by_opposite(6, 0));
        let pos: Position64 = "4k2r/8/8/2r5/8/8/8/R3K2R w KQk - 0 1".parse().unwrap();
        mg.init(&pos);
        assert_eq!(false, mg.is_attacked_by_opposite(6, 0));
        assert_eq!(true, mg.is_attacked_by_opposite(2, 0));
        let pos: Position64 = "4k2r/8/8/2b5/8/8/8/R3K2R w KQk - 0 1".parse().unwrap();
        mg.init(&pos);
        assert_eq!(true, mg.is_attacked_by_opposite(6, 0));
        assert_eq!(false, mg.is_attacked_by_opposite(2, 0));
    }

    #[test]
    fn test_move_gen_from_perft() {
        let cwd = env::current_dir().unwrap();
        let root = cwd.ancestors().next().unwrap();
        let path = root.join("tests/perft.txt");
        let perft_contents = fs::read_to_string(path).unwrap();
        let perft_lines = perft_contents.split("\n");
        let mut line_no = 1;
        for line in perft_lines {
            if line.trim() == "" {
                break;
            }
            let line_parts: Vec<&str> = line.split(',').collect();

            let fen = line_parts[0];

            // println!("Creating from FEN: {}", fen);
            let position: Position64 = fen.parse().unwrap();

            println!("{}: {}", line_no, fen);
            line_no += 1;

            let mut perft = Perft::new(position);

            let perft_depth = 1;

            let perft1 = perft.perft(perft_depth).unwrap();
            let target1: usize = line_parts[perft_depth as usize].parse().unwrap();
            assert_eq!(target1, perft1);
        }
    }

    /* fn perft(position: &mut Position64, move_gen: &mut MoveGenerator, depth: u8) -> usize {
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
    } */
}
