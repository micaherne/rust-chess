use crate::position0x88::{antidiagonal, diagonal, piece_colour, piece_type, EMPTY};

use super::{
    file,
    notation::{piece_type_to_char, square_index_to_str, LongAlgebraicNotationMove},
    opposite_colour, rank, BoardSide, Colour, Piece, PieceType, Position, SquareIndex, BISHOP,
    BLACK, KING, KNIGHT, PAWN, QUEEN, ROOK,
};

pub type Direction = i16; // Not i8 as it simplifies adding it and ANDing with 0x88.

const PIECE_TYPES_COUNT: usize = 7;
const MAX_DIRECTIONS_COUNT: usize = 8;

pub type PieceDirections = [Direction; MAX_DIRECTIONS_COUNT];

const PIECE_DIRECTIONS: [PieceDirections; PIECE_TYPES_COUNT] = [
    [0; 8],
    [16, 0, 0, 0, 0, 0, 0, 0], // Will be flipped for black.
    [1, -1, 16, -16, 0, 0, 0, 0],
    [-14, 14, -18, 18, -31, 31, -33, 33],
    [15, -15, 17, -17, 0, 0, 0, 0],
    [1, -1, 16, -16, 15, -15, 17, -17],
    [1, -1, 16, -16, 15, -15, 17, -17],
];

const PAWN_PUSH_DIRECTIONS: [Direction; 2] = [16, -16];

const PAWN_CAPTURE_DIRECTIONS: [PieceDirections; 2] =
    [[15, 17, 0, 0, 0, 0, 0, 0], [-15, -17, 0, 0, 0, 0, 0, 0]];

const PAWN_HOME_RANK: [u8; 2] = [1, 6];
const PAWN_QUEEN_RANK: [u8; 2] = [7, 0];

const ALLOWED_QUEENING_PIECES: [PieceType; 4] = [QUEEN, ROOK, BISHOP, KNIGHT];

#[derive(Debug)]
pub struct Move {
    from_index: SquareIndex,
    to_index: SquareIndex,
    queening_piece: Option<Piece>,
}

impl ToString for Move {
    fn to_string(&self) -> String {
        let mut result = String::new();
        result.push_str(square_index_to_str(self.from_index).as_str());
        result.push_str(square_index_to_str(self.to_index).as_str());
        match self.queening_piece {
            Some(piece) => result.push(piece_type_to_char(piece).or(Some(' ')).unwrap()),
            None => (),
        }
        result
    }
}

impl From<&Move> for LongAlgebraicNotationMove {
    fn from(value: &Move) -> Self {
        let mut text = String::new();
        text.push_str(&square_index_to_str(value.from_index));
        text.push_str(&square_index_to_str(value.to_index));
        if let Some(q) = value.queening_piece {
            text.push(piece_type_to_char(q).unwrap_or(' '));
        }
        LongAlgebraicNotationMove { text }
    }
}

pub fn generate_moves(position: &Position) -> Vec<Move> {
    let mut result: Vec<Move> = vec![];
    let side_to_move = position.side_to_move;

    let is_check = side_to_move_in_check(position);

    // TODO: Only generate check evasions / filter generated moves to check evasions at end.

    for piece_square in 0..=0x77 {
        if piece_square & 0x88 != 0 {
            continue;
        }

        let piece = position.squares[piece_square];

        if piece == EMPTY {
            continue;
        }
        let current_piece_colour = piece_colour(piece).unwrap();

        if current_piece_colour != side_to_move {
            continue;
        }

        let piece_type = piece_type(piece);

        let opt_pinned_direction = pinned_against_king_direction(position, piece_square);

        if is_sliding_piece(piece_type) {
            create_sliding_moves(position, piece_square, true, &mut result);
        } else if piece_type == PAWN {
            // Generate pawn pushes.
            let direction = match opt_pinned_direction {
                Some(dir) => {
                    let forward_dir = PAWN_PUSH_DIRECTIONS[current_piece_colour as usize];
                    if forward_dir == dir || -forward_dir == dir {
                        forward_dir
                    } else {
                        continue;
                    }
                }
                None => PAWN_PUSH_DIRECTIONS[current_piece_colour as usize],
            };
            let allowed_squares =
                match rank(piece_square) == PAWN_HOME_RANK[current_piece_colour as usize] {
                    true => 2,
                    false => 1,
                };
            let mut next_square = piece_square as i16;
            for _ in 0..allowed_squares {
                next_square = next_square + direction;
                if !is_valid_square(next_square) {
                    break;
                }
                if position.squares[next_square as SquareIndex] != EMPTY {
                    break;
                }

                let is_queening =
                    rank(next_square as usize) == PAWN_QUEEN_RANK[current_piece_colour as usize];

                create_pawn_moves(
                    piece_square,
                    next_square as SquareIndex,
                    is_queening,
                    &mut result,
                );
            }

            // Generate pawn captures.
            // TODO: Take account of pins against the king.
            // TODO: en passent captures.
            for dir in PAWN_CAPTURE_DIRECTIONS[current_piece_colour as usize] {
                let to_square = piece_square as i16 + dir;
                if !is_valid_square(to_square) {
                    continue;
                }

                let is_en_passent =
                    (to_square as usize == position.ep_square) && position.ep_square != 0;

                if !is_en_passent && position.squares[to_square as SquareIndex] == EMPTY {
                    continue;
                }

                let capture_piece_square = if is_en_passent {
                    match piece_square < position.ep_square {
                        true => position.ep_square - 16,
                        false => position.ep_square + 16,
                    }
                } else {
                    to_square as SquareIndex
                };

                let attacked_colour = piece_colour(position.squares[capture_piece_square]);

                if attacked_colour == None || attacked_colour == Some(current_piece_colour) {
                    continue;
                }

                let is_queening =
                    rank(to_square as usize) == PAWN_QUEEN_RANK[current_piece_colour as usize];

                create_pawn_moves(
                    piece_square,
                    to_square as SquareIndex,
                    is_queening,
                    &mut result,
                );
            }
        } else if piece_type == KNIGHT {
            create_non_sliding_moves(position, piece_square, piece_type, &mut result);
        } else if piece_type == KING {
            let mut temp_moves: Vec<Move> = vec![];
            create_non_sliding_moves(position, piece_square, piece_type, &mut temp_moves);
            // Check for check and add to result if ok.
            for potential_move in temp_moves {
                if is_legal_king_move(position, &potential_move, current_piece_colour) {
                    if potential_move.from_index.abs_diff(potential_move.to_index) == 1 {
                        let move_dir =
                            potential_move.to_index as i32 - potential_move.from_index as i32;
                        let board_side = match move_dir {
                            -1 => BoardSide::Queenside,
                            1 => BoardSide::Kingside,
                            _ => panic!("Invalid board side"),
                        };

                        if position
                            .castling_rights
                            .allowed(current_piece_colour, board_side)
                        {
                            let to_square = potential_move.from_index as i32 + move_dir;
                            let castling_move = Move {
                                from_index: potential_move.from_index,
                                to_index: to_square as usize,
                                queening_piece: None,
                            };

                            // Note that this call should be fine even though we're ignoring the intermediate square
                            // rather than the actual king square as the initial sideways move wouldn't be legal if the
                            // king is being attacked from the other side.
                            if is_legal_king_move(position, &castling_move, current_piece_colour) {
                                result.push(castling_move);
                            }
                        }
                    }

                    result.push(potential_move);
                }
            }
        }
    }

    // Remove non-evasion moves if necessary.
    if is_check {
        result = filter_non_check_evasions(position, result);
    }

    result
}

fn filter_non_check_evasions(position: &Position, moves: Vec<Move>) -> Vec<Move> {
    let mut result = vec![];
    let king_square = position.king_squares[position.side_to_move as usize];

    let attackers = attackers_of_square(&position, king_square);

    // If it's double check, only allow king moves.
    if attackers.len() > 1{
        for m in moves {
            if m.from_index != king_square {
                continue;
            }
            result.push(m);
        }
        return result;
    }

    let attacking_piece = piece_type(attackers[0].1);

    if attacking_piece == KNIGHT || attacking_piece == PAWN {
        for m in moves {
            if m.from_index != king_square {
                continue;
            }
            result.push(m);
        }
        return result;
    }

    let opt_direction = direction(king_square, attackers[0].0);
    
    debug_assert!(opt_direction != None);

    let direction = opt_direction.unwrap();

    let mut intermediate_squares: Vec<SquareIndex> = vec![];

    let mut next_square = king_square as Direction;
    loop {
        next_square += direction;
        if !is_valid_square(next_square) {
            break;
        }

        if position.squares[next_square as SquareIndex] != EMPTY {
            break;
        }

        intermediate_squares.push(next_square as SquareIndex);
    }
    
    for m in moves {
        if m.from_index == king_square || intermediate_squares.contains(&m.to_index) {
            result.push(m);
        }
    }

    result
}

fn attackers_of_square(position: &Position, piece_square: SquareIndex) -> Vec<(SquareIndex, Piece)> {
    let mut result = vec![];
    let opt_piece_colour = piece_colour(position.squares[piece_square]);
    if opt_piece_colour == None {
        return result;
    }
    let the_piece_colour = opt_piece_colour.unwrap();

    // Check for sliders.
    for dir in PIECE_DIRECTIONS[QUEEN as usize] {
        let opt_possible_attacker = next_piece_in_direction(position, piece_square, dir, None);
        if opt_possible_attacker == None {
            continue;
        }
        let possible_attacker = opt_possible_attacker.unwrap();
        if piece_colour(possible_attacker.1).unwrap() == the_piece_colour {
            continue;
        }
        let piece_type = piece_type(possible_attacker.1);
        if piece_type == PAWN {
        } else {
            if PIECE_DIRECTIONS[piece_type as usize].contains(&dir) {
                result.push(possible_attacker);
            }
        }
    }

    for p in [
        (KNIGHT, PIECE_DIRECTIONS[KNIGHT as usize]),
        (PAWN, PAWN_CAPTURE_DIRECTIONS[the_piece_colour as usize]),
        (KING, PIECE_DIRECTIONS[KING as usize]),
    ] {
        for dir in p.1 {
            let test_square = dir + piece_square as i16;
            if !is_valid_square(test_square) {
                continue;
            }
            let directions_piece = p.0;
            let test_piece = position.squares[test_square as usize];
            let test_piece_type = piece_type(test_piece);
            if test_piece_type == EMPTY {
                continue;
            }
            if test_piece_type == directions_piece {
                match piece_colour(test_piece) {
                    None => continue,
                    Some(pc) => {
                        if pc != the_piece_colour {
                            result.push((test_square as SquareIndex, test_piece));
                        }
                    }
                }
            }
        }
    }
    
    result
}

pub fn allowed_directions(position: &Position, piece_square: SquareIndex) -> PieceDirections {
    let opt_pinned_direction = pinned_against_king_direction(position, piece_square);
    let piece_type = piece_type(position.squares[piece_square]);

    if piece_type == EMPTY {
        return [0i16; MAX_DIRECTIONS_COUNT];
    }

    let directions = PIECE_DIRECTIONS[piece_type as usize];

    match opt_pinned_direction {
        Some(dir) => {
            if directions.contains(&dir) || directions.contains(&-dir) {
                [dir, -dir, 0, 0, 0, 0, 0, 0]
            } else {
                [0; 8]
            }
        }
        None => directions,
    }
}

pub fn is_square_attacked(
    position: &Position,
    square: SquareIndex,
    moving_colour: Colour,
    ignore_square: Option<SquareIndex>,
) -> bool {
    if is_square_attacked_by_slider(position, square, moving_colour, ignore_square) {
        return true;
    }

    for test_piece_type in [PAWN, KNIGHT, KING] {
        if is_square_attacked_by_non_slider(position, square, moving_colour, test_piece_type) {
            return true;
        }
    }

    false
}

pub fn is_square_attacked_by_non_slider(
    position: &Position,
    square: SquareIndex,
    moving_colour: Colour,
    test_piece_type: PieceType,
) -> bool {
    let piece_directions = match test_piece_type {
        PAWN => PAWN_CAPTURE_DIRECTIONS[moving_colour as usize],
        piece_type => PIECE_DIRECTIONS[piece_type as usize],
    };

    for dir in piece_directions {
        if dir == 0 {
            break;
        }
        let attack_square = square as i16 + dir;
        if !is_valid_square(attack_square) {
            continue;
        }
        let attack_piece = position.squares[attack_square as usize];
        if super::piece_type(attack_piece) != test_piece_type {
            continue;
        }
        if piece_colour(attack_piece) == Some(moving_colour) {
            continue;
        }
        return true;
    }

    return false;
}

pub fn is_square_attacked_by_slider(
    position: &Position,
    square: SquareIndex,
    moving_colour: Colour,
    ignore_square: Option<SquareIndex>,
) -> bool {
    for test_piece in [ROOK, BISHOP] {
        for dir in PIECE_DIRECTIONS[test_piece as usize] {
            if dir == 0 {
                break;
            }
            let p = next_piece_in_direction(position, square, dir, ignore_square);
            if p == None {
                continue;
            }
            let next_piece = p.unwrap();
            if piece_colour(next_piece.1) == Some(moving_colour) {
                continue;
            }
            let next_piece_type = super::piece_type(next_piece.1);
            if next_piece_type == test_piece || next_piece_type == QUEEN {
                return true;
            }
        }
    }
    false
}

pub fn is_legal_king_move(position: &Position, move_to_test: &Move, moving_colour: Colour) -> bool {
    !is_square_attacked(
        position,
        move_to_test.to_index,
        moving_colour,
        Some(move_to_test.from_index),
    )
}

pub fn create_sliding_moves(
    position: &Position,
    piece_square: SquareIndex,
    allow_captures: bool,
    result: &mut Vec<Move>,
) {
    let opt_current_piece_colour = piece_colour(position.squares[piece_square]);
    if opt_current_piece_colour == None {
        // Should not happen.
        return;
    }
    let piece_type = piece_type(position.squares[piece_square]);
    let current_piece_colour = opt_current_piece_colour.unwrap();
    let allowed_directions = allowed_directions(position, piece_square);
    for direction in allowed_directions {
        if direction == 0 {
            break;
        }

        let mut dir = direction;
        if piece_type == PAWN && current_piece_colour == BLACK {
            dir = -dir;
        }

        let mut next_square = piece_square as i16;
        loop {
            next_square += dir;
            if next_square < 0 || next_square & 0x88 != 0 {
                break;
            }

            if position.squares[next_square as usize] == EMPTY {
                result.push(Move {
                    from_index: piece_square,
                    to_index: next_square as SquareIndex,
                    queening_piece: None,
                });
                continue;
            }
            match piece_colour(position.squares[next_square as usize]) {
                Some(colour) => {
                    if !allow_captures {
                        break;
                    }

                    if colour == current_piece_colour {
                        break;
                    } else {
                        result.push(Move {
                            from_index: piece_square,
                            to_index: next_square as SquareIndex,
                            queening_piece: None,
                        });
                        break;
                    }
                }
                None => (), // TODO: Do something with this error.
            }
        }
    }
}

pub fn create_non_sliding_moves(
    position: &Position,
    piece_square: SquareIndex,
    piece_type: PieceType,
    result: &mut Vec<Move>,
) {
    let opt_current_piece_colour = piece_colour(position.squares[piece_square]);
    if opt_current_piece_colour == None {
        // Should not happen.
        return;
    }
    let current_piece_colour = opt_current_piece_colour.unwrap();
    for offset in PIECE_DIRECTIONS[piece_type as usize] {
        let next_square = piece_square as i16 + offset;
        if !is_valid_square(next_square) {
            continue;
        }
        if position.squares[next_square as usize] != EMPTY {
            let capture_colour = piece_colour(position.squares[next_square as usize]);
            // We know it's a real colour as it's not empty but unwrap it properly anyway.
            if let None = capture_colour {
                // This should never happen.
                continue;
            }
            if capture_colour.unwrap() == current_piece_colour {
                continue;
            }
        }
        result.push(Move {
            from_index: piece_square,
            to_index: next_square as SquareIndex,
            queening_piece: None,
        });
    }
}

pub fn create_pawn_moves(
    from_square: SquareIndex,
    to_square: SquareIndex,
    is_queening: bool,
    result: &mut Vec<Move>,
) {
    if is_queening {
        for queening_piece in ALLOWED_QUEENING_PIECES {
            result.push(Move {
                from_index: from_square,
                to_index: to_square,
                queening_piece: Some(queening_piece),
            });
        }
    } else {
        result.push(Move {
            from_index: from_square,
            to_index: to_square,
            queening_piece: None,
        });
    }
}

pub fn side_to_move_in_check(position: &Position) -> bool {
    is_square_attacked(
        position,
        position.king_squares[position.side_to_move as usize],
        position.side_to_move,
        None,
    )
}

pub fn is_sliding_piece(piece_type: PieceType) -> bool {
    return piece_type == QUEEN || piece_type == ROOK || piece_type == BISHOP;
}

/// Check if the piece is pinned against the king, and return the direction of the pin if it is.
pub fn pinned_against_king_direction(
    position: &Position,
    piece_square: SquareIndex,
) -> Option<Direction> {
    if position.squares[piece_square] == EMPTY {
        return None;
    }

    let side_to_move = position.side_to_move;
    let king_square = position.king_squares[side_to_move as usize];

    if king_square == piece_square {
        return None;
    }
    let opposite_side = opposite_colour(side_to_move);

    let dir_from_king = direction(king_square, piece_square);
    if let Some(dir) = dir_from_king {
        let next_piece = next_piece_in_direction(&position, king_square, dir, None);
        if let Some(piece_location) = next_piece {
            if piece_location.0 == piece_square {
                let piece_beyond = next_piece_in_direction(&position, piece_square, dir, None);
                if let Some(piece_beyond_location) = piece_beyond {
                    let piece_beyond_colour = piece_colour(piece_beyond_location.1).unwrap();
                    if piece_beyond_colour == opposite_side {
                        // We need to check that the piece can move in the direction given.
                        if can_slide_in_direction(piece_type(piece_beyond_location.1), dir) {
                            return Some(-dir);
                        }
                    }
                }
            }
        }
    }
    None
}

pub fn can_slide_in_direction(piece_type: PieceType, direction: Direction) -> bool {
    match direction.abs() {
        1 | 16 => piece_type == ROOK || piece_type == QUEEN,
        15 | 17 => piece_type == BISHOP || piece_type == QUEEN,
        _ => false,
    }
}

/// Is the given square index valid?
/// Note that this is an i16, not a SquareIndex, as it may have been calculated and could
/// be negative.
#[inline]
pub fn is_valid_square(square: i16) -> bool {
    square >= 0 && (square & 0x88 == 0)
}

pub fn next_piece_in_direction(
    position: &Position,
    from: SquareIndex,
    direction: Direction,
    ignore_square: Option<SquareIndex>,
) -> Option<(SquareIndex, Piece)> {
    if direction == 0 {
        // TODO: This should not be called in the first place.
        return None;
    }
    let mut next_square = (direction as i16) + from as i16;
    loop {
        if !is_valid_square(next_square) {
            break;
        }
        if let Some(ignore) = ignore_square {
            if ignore == next_square as usize {
                next_square += direction as i16;
                continue;
            }
        }
        let piece = position.squares[next_square as usize];
        if piece != EMPTY {
            return Some((next_square as SquareIndex, piece));
        }
        next_square += direction as i16;
    }
    None
}

/// Find the direction from one square to the other, if they are on the same line or diagonal.
pub fn direction(from: SquareIndex, to: SquareIndex) -> Option<Direction> {
    debug_assert!(from != to);
    if rank(from) == rank(to) {
        Some(if from > to { -1 } else { 1 })
    } else if file(from) == file(to) {
        Some(if from > to { -16 } else { 16 })
    } else if diagonal(from) == diagonal(to) {
        Some(if from > to { -17 } else { 17 })
    } else if antidiagonal(from) == antidiagonal(to) {
        Some(if from > to { -15 } else { 15 })
    } else {
        None
    }
}

#[cfg(test)]
mod test {
    use crate::position0x88::{
        get_piece,
        notation::{set_from_fen, set_startpos, make_move},
        BLACK, KING, PAWN,
    };

    use super::*;

    const PERFT_POSITIONS: [&str; 3] = [
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -",
        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -",
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -",
    ];

    const PERFT_COUNT_1: [usize; 3] = [48, 14, 6];

    #[test]
    fn test_generate_moves() {
        let mut pos = Position::default();
        set_startpos(&mut pos);
        let m = generate_moves(&pos);
        assert_eq!(20, m.len());
    }

    #[test]
    fn test_generate_moves_check() {
        let mut position = Position::default();
        set_from_fen(&mut position, "rnbk1bnr/6pp/4Pp2/1N6/4PB2/P4N2/P3BPPP/R2R2K1 b - - 1 15 ").expect("Failed");
        let m = generate_moves(&position);
        for mov in &m {
            let m2:LongAlgebraicNotationMove = mov.into();
            print!("{:#?}", m2);
        }
        assert_eq!(5, m.len());
    }

    #[test]
    fn test_generate_moves_perft() {
        for (i, fen) in PERFT_POSITIONS.iter().enumerate() {
            let mut pos = Position::default();
            let mut full_fen = fen.to_owned().to_string();
            full_fen.push_str(" 0 1");
            set_from_fen(&mut pos, &full_fen).expect("Valid FEN");
            let m = generate_moves(&pos);
            if PERFT_COUNT_1[i] != m.len() {
                for mv in &m {
                    println!("{}", mv.to_string());
                }
            }
            assert_eq!(PERFT_COUNT_1[i], m.len());
        }
    }

    #[test]
    fn test_direction() {
        assert_eq!(-1, direction(0x02, 0x01).unwrap());
        assert_eq!(1, direction(0x01, 0x07).unwrap());
        assert_eq!(16, direction(0x04, 0x64).unwrap());
        assert_eq!(15, direction(0x07, 0x70).unwrap());
        assert_eq!(1, direction(0x42, 0x46).unwrap());
    }

    #[test]
    fn test_pinned_against_king_direction() {
        let mut pos = Position::default();
        pos.set_square_to_piece(0x44, PAWN);
        pos.set_square_to_piece(0x42, get_piece(ROOK, BLACK));
        pos.set_square_to_piece(0x46, KING);
        assert_eq!(1, pinned_against_king_direction(&pos, 0x44).unwrap());
        pos.set_square_to_piece(0x42, EMPTY);
        assert_eq!(None, pinned_against_king_direction(&pos, 0x44));
    }

    #[test]
    fn test_side_to_move_in_check() {
        let mut pos = Position::default();
        set_from_fen(&mut pos, "rnbk1bnr/6pp/4Pp2/1N6/4PB2/P4N2/P3BPPP/R2R2K1 b - - 1 15 ").expect("Aaargh!");
        assert_eq!(true, side_to_move_in_check(&pos));
    }

    #[test]
    fn test_temp() {
        let mut pos = Position::default();
        // set_from_fen(&mut pos, "rnbq1bnr/3Pkppp/4p3/8/2P1P3/2N2N2/PP2BPPP/R1BQ1RK1 b - - 2 12").unwrap();
        // set_pos_from_position_command(&mut pos, "position startpos moves e2e4 a7a6 d2d4 a6a5 g1f3 a5a4 c2c4 a4a3 b1a3 b7b6 f1e2 b6b5 a3b5 c7c6 b5c3 c6c5 d4c5 d7d6 c5d6 e7e6 d6d7 e8e7 e1g1");
        set_pos_from_position_command(&mut pos, "position startpos moves c2c4 a7a6 c4c5 a6a5 a2a4 b7b6 c5b6 c7c6 d2d4 c6c5 c1f4 c5c4 b1c3 d7d6 d1c2 d6d5 e1c1 e7e6 g1f3 e6e5 f3e5 f7f6 e5f3 f6f5 e2e4 d5e4 c3e4 c4c3 d1e1 c3b2 c2b2 f5e4 e1e4 c8e6 e4e6 d8e7 e6e7 e8d8 f1b5");
        let moves = generate_moves(&pos);
        for mv in &moves {
            println!("{}", mv.to_string());
        }
    }

    fn set_pos_from_position_command(position: &mut Position, command: &str) {
        let mut parts = command.split_ascii_whitespace();
        if parts.next().unwrap() != "position" {
            panic!("Position not found")
        }
        if parts.next().unwrap() == "startpos" {
            set_startpos(position);
        } else {
            todo!("Implement FEN");
        }

        let moves_token = parts.next();
        if moves_token == None {
            return;
        } else if moves_token.unwrap() != "moves" {
            panic!("Moves not found");
        }

        loop {
            let m_opt = parts.next();
            if m_opt == None {
                break;
            }
            let m = LongAlgebraicNotationMove::from_text(m_opt.unwrap().to_string()).unwrap();
            make_move(position, m.from_square(), m.to_square(), m.queening_piece());
        }
    }
}
