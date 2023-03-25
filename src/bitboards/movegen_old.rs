use crate::{
    bitboards::{unwrap_bitboard, DIR_ALL_SLIDERS},
    position::{
        index0x88to64, index64to0x88,
        movegen::{is_sliding_piece, PIECE_TYPES_COUNT},
        opposite_colour, piece_colour, piece_type, PieceType, Position, BISHOP, BLACK, KING,
        KNIGHT, PAWN, QUEEN, ROOK, WHITE,
    },
};

use super::{
    dir_index, highest_set_bit, is_diagonal, is_linear, is_slider, lowest_set_bit, rank,
    slides_in_dir, square_mask64, Bitboard, Direction, SixtyFourBitboards, SquareIndex64, BETWEEN,
    DIRECTION_FROM_TO, KING_ATTACK_SQUARES, KNIGHT_ATTACK_SQUARES, PAWN_ATTACK_SQUARES,
    SLIDER_DIRECTION_SQUARE,
};

pub trait ColourType {
    fn colour_index() -> usize;
    fn opposite_colour_index() -> usize;
    fn pawn_attack_squares() -> SixtyFourBitboards;
}

pub struct Black {}
impl ColourType for Black {
    fn colour_index() -> usize {
        1
    }

    fn opposite_colour_index() -> usize {
        0
    }

    fn pawn_attack_squares() -> SixtyFourBitboards {
        PAWN_ATTACK_SQUARES[Black::colour_index()]
    }
}

pub struct White {}

impl ColourType for White {
    fn colour_index() -> usize {
        0
    }

    fn opposite_colour_index() -> usize {
        1
    }

    fn pawn_attack_squares() -> SixtyFourBitboards {
        PAWN_ATTACK_SQUARES[White::colour_index()]
    }
}

const PIECES_BY_VALUE: [PieceType; PIECE_TYPES_COUNT - 1] =
    [PAWN, KNIGHT, BISHOP, ROOK, QUEEN, KING];

impl Position {
    #[inline]
    pub fn colour_piece_locations<Colour: ColourType>(&self, piece_type: PieceType) -> Bitboard {
        self.bb_colours[Colour::colour_index()] & self.bb_pieces[piece_type as usize]
    }

    pub fn quiescence_moves<Colour: ColourType>(&self) -> Vec<QuiescenceMove> {
        let mut result = vec![];
        let opposite_pieces = self.bb_colours[Colour::opposite_colour_index()];
        for piece_type in PIECES_BY_VALUE {
            let mut locations = self.colour_piece_locations::<Colour>(piece_type);

            while locations != 0 {
                let location = locations.trailing_zeros();
                locations &= locations - 1;

                let pin_mask = self.pin_mask(location as SquareIndex64);

                if piece_type == PAWN {
                    // get pawn captures.
                    let pawn_captures =
                        self.pawn_captures_from::<Colour>(location as SquareIndex64, pin_mask);

                    result.extend(pawn_captures.iter().map(|x| QuiescenceMove {
                        from: location as SquareIndex64,
                        to: *x,
                        mvv_lvm: None,
                        is_queening: rank(*x) == 0 || rank(*x) == 7,
                        is_capture: true,
                    }));
                } else if piece_type == KNIGHT {
                    for ksquare in unwrap_bitboard!(
                        KNIGHT_ATTACK_SQUARES[location as usize] & opposite_pieces & pin_mask
                    ) {
                        result.push(QuiescenceMove {
                            from: location as SquareIndex64,
                            to: ksquare,
                            mvv_lvm: None,
                            is_queening: false,
                            is_capture: true,
                        });
                    }
                } else if piece_type == KING {
                    for ksquare in
                        unwrap_bitboard!(KING_ATTACK_SQUARES[location as usize] & opposite_pieces)
                    {
                        if self.is_attacked_by_opposition(location as SquareIndex64) {
                            continue;
                        }
                        result.push(QuiescenceMove {
                            from: location as SquareIndex64,
                            to: ksquare,
                            mvv_lvm: None,
                            is_queening: false,
                            is_capture: true,
                        });
                    }
                } else if is_sliding_piece(piece_type) {
                    let dir_indexes = match piece_type {
                        BISHOP => 4..8,
                        ROOK => 0..4,
                        QUEEN => 0..8,
                        _ => panic!("Invalid piece"),
                    };
                    for dir_index in dir_indexes {
                        let dir = DIR_ALL_SLIDERS[dir_index];
                        if let Some(sq) = self.next_occupied_square_in_direction(
                            location as SquareIndex64,
                            dir,
                            0,
                        ) {
                            if square_mask64(sq) & pin_mask == 0 {
                                continue;
                            }

                            if let Some(pc) = piece_colour(self.square_piece(index64to0x88(sq))) {
                                if pc as usize == Colour::opposite_colour_index() {
                                    result.push(QuiescenceMove {
                                        from: location as SquareIndex64,
                                        to: sq,
                                        mvv_lvm: None,
                                        is_queening: false,
                                        is_capture: true,
                                    });
                                }
                            }
                        }
                    }
                }
            }
        }
        result
    }

    pub fn opposition_pieces(&self) -> Bitboard {
        self.bb_colours[opposite_colour(self.side_to_move) as usize]
    }

    pub fn side_to_move_pieces(&self) -> Bitboard {
        self.bb_colours[self.side_to_move as usize]
    }

    pub fn is_attacked_by_opposition(&self, square: SquareIndex64) -> bool {
        let opposition_pieces = self.opposition_pieces();
        let mut x = PAWN_ATTACK_SQUARES[self.side_to_move as usize][square as usize]
            & (opposition_pieces & self.bb_pieces[PAWN as usize]);
        if x != 0 {
            return true;
        }
        x = KNIGHT_ATTACK_SQUARES[square as usize]
            & (opposition_pieces & self.bb_pieces[KNIGHT as usize]);
        if x != 0 {
            return true;
        }
        let diagonal_sliders =
            opposition_pieces & (self.bb_pieces[QUEEN as usize] | self.bb_pieces[BISHOP as usize]);
        let linear_sliders =
            opposition_pieces & (self.bb_pieces[QUEEN as usize] | self.bb_pieces[ROOK as usize]);

        for direction in DIR_ALL_SLIDERS {
            let next_square = self.next_occupied_square_in_direction(square, direction, 0);
            if let Some(next) = next_square {
                if is_diagonal(direction) && ((diagonal_sliders & square_mask64(next)) != 0) {
                    return true;
                }
                if is_linear(direction) && ((linear_sliders & square_mask64(next)) != 0) {
                    return true;
                }
            }
        }

        false
    }

    pub fn pawn_captures_from<T: ColourType>(
        &self,
        square: SquareIndex64,
        pin_mask: Bitboard,
    ) -> Vec<SquareIndex64> {
        debug_assert!(
            piece_colour(self.square_piece(index64to0x88(square))).unwrap()
                == (T::colour_index() as u8)
        );

        let opposite_pieces = self.bb_colours[T::opposite_colour_index()];
        let pawn_cap_squares = T::pawn_attack_squares()[square as usize];
        let captures = opposite_pieces & pawn_cap_squares & pin_mask;
        unwrap_bitboard!(captures)
    }

    pub fn ep_capture_square(&self) -> SquareIndex64 {
        if self.ep_square == 0 {
            return 0;
        }
        if self.side_to_move == WHITE {
            index0x88to64(self.ep_square - 16)
        } else {
            index0x88to64(self.ep_square + 16)
        }
    }

    #[inline]
    pub fn occupied_squares(&self) -> Bitboard {
        self.bb_pieces[WHITE as usize] | self.bb_pieces[BLACK as usize]
    }

    pub fn next_occupied_square_in_direction(
        &self,
        square_index: SquareIndex64,
        direction: Direction,
        ignore: Bitboard,
    ) -> Option<SquareIndex64> {
        debug_assert!(DIR_ALL_SLIDERS.contains(&direction));

        let other_way = SLIDER_DIRECTION_SQUARE[dir_index(direction)][square_index as usize];
        let pieces_beyond = other_way & self.occupied_squares() & !ignore;

        if pieces_beyond == 0 {
            None
        } else {
            if direction > 0 {
                Some(lowest_set_bit(pieces_beyond))
            } else {
                Some(highest_set_bit(pieces_beyond))
            }
        }
    }

    /// Check whether a piece is pinned against the king of the side to move.
    /// TODO: This currently uses square_of_pinning_piece but could potentially be optimised for this use.
    ///
    pub fn is_pinned_against_king<T: ColourType>(&self, square_index: SquareIndex64) -> bool {
        let target_index = self.king_squares[T::colour_index()] as SquareIndex64;
        self.square_of_pinning_piece(square_index, target_index, 0)
            .is_some()
    }

    /// Find the square on which a piece pinning square_index against target_index sits.
    ///
    /// We can ignore any intermediate squares by passing an "ignore" bitboard.
    pub fn square_of_pinning_piece(
        &self,
        square_index: SquareIndex64,
        target_index: SquareIndex64,
        ignore: Bitboard,
    ) -> Option<SquareIndex64> {
        let direction = DIRECTION_FROM_TO[target_index as usize][square_index as usize];
        if direction == 0 {
            return None;
        }

        let next_occupied = self.next_occupied_square_in_direction(
            target_index,
            direction,
            (1 << square_index) | ignore,
        )?;

        // Check if there's a piece between the target square and square.
        if DIRECTION_FROM_TO[square_index as usize][next_occupied as usize] != direction {
            return None;
        }

        // TODO: This shouldn't have to use the 0x88 board.
        let piece = self.square_piece(index64to0x88(next_occupied));
        let piece_colour = piece_colour(piece);

        debug_assert!(piece_colour.is_some());

        if piece_colour.unwrap() != self.side_to_move {
            // Check if piece can move in direction dir_from_king.
            let piece_type = piece_type(piece);

            if is_slider(piece_type) && slides_in_dir(piece_type, -direction) {
                Some(next_occupied)
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Returns a bitboard of all the squares that a piece is allowed to move to, taking into
    /// account pins against the king.
    ///
    /// Note that this is not in any way valid squares for that particular piece, just a mask of squares
    /// that wouldn't result in a "discovered" check.
    ///
    pub fn pin_mask(&self, square: SquareIndex64) -> Bitboard {
        let king_square = index0x88to64(self.king_squares[self.side_to_move as usize]);

        // Let the king move!
        if king_square == square {
            return Bitboard::MAX;
        }

        // Take account of e.p.
        let mut allow_ep = true;
        let ep_capture = self.ep_capture_square();
        if ep_capture != 0 {
            // For horizontal pins wit
            let mut ignore = 0;
            if ep_capture != 0
                && DIRECTION_FROM_TO[king_square as usize][square as usize].abs() == 1
                && square.abs_diff(ep_capture) == 1
            {
                ignore = 1 << square;
            }

            let ep_pinning_square = self.square_of_pinning_piece(ep_capture, king_square, ignore);
            if ep_pinning_square.is_some() {
                allow_ep = false;
            }
        }

        let pinning_piece_square = self.square_of_pinning_piece(square, king_square, 0);

        let mut result = match pinning_piece_square {
            Some(end_square) => {
                BETWEEN[king_square as usize][end_square as usize] | (1 << end_square)
            }
            None => Bitboard::MAX,
        };

        if !allow_ep {
            result &= !(1 << index0x88to64(self.ep_square));
        }

        result
    }
}

#[derive(Debug)]
pub struct QuiescenceMove {
    pub from: SquareIndex64,
    pub to: SquareIndex64,
    pub is_queening: bool,
    pub is_capture: bool,
    pub mvv_lvm: Option<i16>,
}

struct SquaresIterator(Bitboard);

impl Iterator for SquaresIterator {
    type Item = SquareIndex64;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            None
        } else {
            let result = Some(self.0.trailing_zeros() as SquareIndex64);
            self.0 &= self.0 - 1;
            result
        }
    }
}

#[cfg(test)]
mod test {

    use super::*;

    #[test]
    fn test_quiescense_moves() {
        let pos1: Position = "5bbr/3kB3/rp3q2/p4p1P/PpPpP3/3Q1N2/3P1PBP/RN2K2R b KQ e3 0 1".into();
        let qm1 = pos1.quiescence_moves::<Black>();
        assert_eq!(6, qm1.len())
    }

    #[test]
    fn test_pawn_captures() {
        let pos1: Position = "5bbr/3kB3/rp3q2/p4p1P/PpPpP3/3Q1N2/3P1PBP/RN2K2R b KQ e3 0 1".into();
        let caps1 = pos1.pawn_captures_from::<Black>(37, pos1.pin_mask(37));
        assert_eq!(vec![28], caps1);
        let caps2 = pos1.pawn_captures_from::<Black>(27, pos1.pin_mask(27));
        assert_eq!(Vec::<u8>::new(), caps2);
    }

    #[test]
    fn test_square_of_pinning_piece() {
        let pos1: Position = "6k1/1q6/8/3pP3/8/4B3/8/7K w - d6 0 1".into();
        let pp1 = pos1.square_of_pinning_piece(35, 7, 0);
        assert_eq!(49, pp1.unwrap());
    }

    #[test]
    fn test_ep_capture_square() {
        let pos1: Position = "6k1/1q6/8/3pP3/8/4B3/8/7K w - d6 0 1".into();
        let ep1 = pos1.ep_capture_square();
        assert_eq!(35, ep1);
    }

    #[test]
    fn test_pin_mask() {
        let pos1: Position = "6k1/8/8/2q5/8/4B3/8/6K1 w - - 0 1".into();
        assert_eq!(0x408102000, pos1.pin_mask(20));
        let pos2: Position = "6k1/8/2q5/8/8/4B3/8/6K1 w - - 0 1".into();
        assert_eq!(Bitboard::MAX, pos2.pin_mask(20));
        // Diagonal e.p. pin.
        let pos3: Position = "6k1/1q6/8/3pP3/8/4B3/8/7K w - d6 0 1".into();
        // Everything except the en-passent square.
        assert_eq!(0xfffff7ffffffffff, pos3.pin_mask(36));

        // Horizontal e.p. pin.
        let pos4: Position = "6k1/8/8/1q1pP2K/8/4B3/8/8 w - d6 0 1".into();
        assert_eq!(0xfffff7ffffffffff, pos4.pin_mask(36));
    }

    #[test]
    fn test_is_attacked_by_opposition() {
        let pos1: Position = "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 1".into();
        assert!(pos1.is_attacked_by_opposition(35));
        assert!(!pos1.is_attacked_by_opposition(36));
        assert!(pos1.is_attacked_by_opposition(30));
        assert!(!pos1.is_attacked_by_opposition(33));
    }
}
