

use crate::{position0x88::{antidiagonal, diagonal, piece_colour, piece_type, EMPTY}, position::SetPosition};

use super::{
    file,
    notation::{KING_HOME_SQUARES},
    opposite_colour, rank, BoardSide, Colour, Piece, PieceType, Position0x88, SquareIndex, BISHOP,
    BLACK, KING, KNIGHT, PAWN, QUEEN, ROOK, movegen::{GenerateMoves, Move}, index64to0x88,
};


impl GenerateMoves for Position0x88 {
    fn generate_moves(&self) -> Vec<Move> {
        generate_moves(&self)
    }
}

pub type Direction = i16; // Not i8 as it simplifies adding it and ANDing with 0x88.

pub type SquareAndPiece = (SquareIndex, Piece);

pub const PIECE_TYPES_COUNT: usize = 7;
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

pub fn generate_moves(position: &Position0x88) -> Vec<Move> {
    let mut result: Vec<Move> = vec![];
    let side_to_move = position.side_to_move;

    let is_check = side_to_move_in_check(position);

    let mut our_pieces = position.bb_colours[side_to_move as usize];

    // Check evasions are filtered out at the end.

    while our_pieces != 0 {

        let piece_square = index64to0x88(our_pieces.trailing_zeros() as u8);
        our_pieces &= our_pieces - 1;
        
        let piece = position.square_piece(piece_square);

        debug_assert!(piece != EMPTY);

        let current_piece_colour = piece_colour(piece).unwrap();

        debug_assert!(current_piece_colour == side_to_move);

        let piece_type = piece_type(piece);

        debug_assert!(piece_type != EMPTY);

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
                        0
                    }
                }
                None => PAWN_PUSH_DIRECTIONS[current_piece_colour as usize],
            };

            if direction != 0 {
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

                    let is_queening = rank(next_square as usize)
                        == PAWN_QUEEN_RANK[current_piece_colour as usize];

                    create_pawn_moves(
                        piece_square,
                        next_square as SquareIndex,
                        is_queening,
                        &mut result,
                    );
                }
            }

            // Generate pawn captures.
            // TODO: Take account of pins against the king.
            for dir in PAWN_CAPTURE_DIRECTIONS[current_piece_colour as usize] {
                if dir == 0 {
                    break;
                }

                match opt_pinned_direction {
                    None => (),
                    Some(d) => {
                        if dir != d && dir != -d {
                            continue;
                        }
                    }
                }

                let to_square = piece_square as i16 + dir;
                if !is_valid_square(to_square) {
                    continue;
                }

                let is_en_passent =
                    (to_square as usize == position.ep_square) && position.ep_square != 0;

                if !is_en_passent && position.squares[to_square as SquareIndex] == EMPTY {
                    continue;
                }

                if is_en_passent && is_en_passent_pin(&position, piece_square) {
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
            // A pinned knight can't move.
            if opt_pinned_direction == None {
                create_non_sliding_moves(position, piece_square, piece_type, &mut result);
            }
        } else if piece_type == KING {
            let mut temp_moves: Vec<Move> = vec![];
            create_non_sliding_moves(position, piece_square, piece_type, &mut temp_moves);

            // Check for check and add to result if ok.
            for potential_move in temp_moves {
                if is_legal_king_move(position, &potential_move, current_piece_colour) {
                    // Check if we can also castle.
                    if !is_check 
                        && potential_move.from_index == KING_HOME_SQUARES[current_piece_colour as usize]
                        // it's horizontal
                        && potential_move.from_index.abs_diff(potential_move.to_index) == 1
                        // and not a capture
                        && super::piece_type(position.squares[potential_move.to_index]) == EMPTY
                    {
                        let move_dir =
                            (potential_move.to_index as i32) - (potential_move.from_index as i32);

                        let board_side = match move_dir {
                            -1 => BoardSide::Queenside,
                            1 => BoardSide::Kingside,
                            _ => panic!("Invalid board side"),
                        };

                        add_castling_if_allowed(position, current_piece_colour, board_side, &mut result);

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


/// Test for the capture pawn being pinned against the king in an en passent capture.
/// This does not check if the position is en passent so should only be called when we already know it is.
fn is_en_passent_pin(position: &Position0x88, capturing_pawn_square: SquareIndex) -> bool {

    let captured_square = match capturing_pawn_square < position.ep_square {
        true => position.ep_square - 16,
        false => position.ep_square + 16,
    };

    let king_square = position.king_squares[position.side_to_move as usize];

    let opt_dir_from_king = direction(king_square, captured_square);
    if opt_dir_from_king == None {
        return false;
    }
    let dir_from_king = opt_dir_from_king.unwrap();

    // If it's a vertical pin the capturing pawn will continue the pin.
    if dir_from_king.abs() == 16  {
        return false;
    }

    let king_on_same_rank = rank(capturing_pawn_square) == rank(king_square);

    if !king_on_same_rank {
        let p = next_piece_in_direction(position, king_square, dir_from_king, Some(captured_square));
        if p == None {
            return false;
        }
        let potential_attacker = p.unwrap().1;
        if piece_colour(potential_attacker).unwrap() == position.side_to_move {
            return false;
        }
        let potential_attacker_piece = piece_type(potential_attacker);
        if !is_sliding_piece(potential_attacker_piece) {
            return false;
        }
        
        return PIECE_DIRECTIONS[potential_attacker_piece as usize].contains(&dir_from_king);
    }

    // Check for horizontal pin. That is, is the king protected across a rank by only the two pawns involved
    // in an en passent capture?

    // This could use the next_piece_in_direction() function if we were to expand that to allow multiple
    // ignore squares but this is the only place where that would be useful so simpler to code it separately.

    let mut next_square = position.king_squares[position.side_to_move as usize] as i16 + dir_from_king;

    while is_valid_square(next_square) {
        if next_square as usize == capturing_pawn_square || next_square as usize == captured_square || position.squares[next_square as usize] == EMPTY {
            next_square += dir_from_king;
            continue;
        }
        
        let pt = piece_type(position.squares[next_square as usize]);
        debug_assert!(pt != EMPTY);

        let colour = piece_colour(position.squares[next_square as usize]);

        debug_assert!(colour.is_some());

        if colour.unwrap() == position.side_to_move {
            return false;
        }

        return pt == ROOK || pt == QUEEN;
        
    }

    false
}

fn add_castling_if_allowed(position: &Position0x88, colour: Colour, board_side: BoardSide, moves: &mut Vec<Move>) {
    
    // This also implicitly asserts that the rook is there.
    if !position.castling_rights.allowed(colour, board_side) {
        return;
    }
    
    let king_square = position.king_squares[colour as usize];
    
    debug_assert!(piece_type(position.squares[king_square]) == KING );

    let move_dir: Direction = match board_side {
        BoardSide::Kingside => 1,
        BoardSide::Queenside => -1
    };

    debug_assert!(piece_type(position.squares[(king_square as i16 + move_dir) as SquareIndex]) == EMPTY);

    let to_square = (king_square as i16 + 2i16 * move_dir) as SquareIndex;

    debug_assert!(is_valid_square(to_square as i16));

    let squares_are_empty = match board_side {
        BoardSide::Kingside => {
            position.squares[to_square as SquareIndex] == EMPTY
        }
        BoardSide::Queenside => {
            let knights_square = (to_square as i16 + move_dir) as SquareIndex;
            position.squares[to_square as SquareIndex] == EMPTY
                && position.squares[knights_square]
                    == EMPTY
        }
    };

    if !squares_are_empty && !is_square_attacked(position, to_square, colour, None) {
        return;
    }

    let castling_move = Move {
        from_index: king_square,
        to_index: to_square,
        queening_piece: None,
    };

    // Note that this call should be fine even though we're ignoring the intermediate square
    // rather than the actual king square as the initial sideways move wouldn't be legal if the
    // king is being attacked from the other side.
    if is_legal_king_move(position, &castling_move, colour) {
        moves.push(castling_move);
    }

}

fn filter_non_check_evasions(position: &Position0x88, moves: Vec<Move>) -> Vec<Move> {
    let mut result = vec![];
    let king_square = position.king_squares[position.side_to_move as usize];

    let attackers = attackers_of_square(&position, king_square);

    if attackers.len() == 0 {
        println!("{:#?} Square: {:x}", position, king_square);
    }

    debug_assert!(attackers.len() != 0);

    // If it's double check, only allow king moves.
    if attackers.len() > 1 {
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
            // Check for e.p. capture of a checking pawn.
            if attacking_piece == PAWN && m.to_index == position.ep_square && piece_type(position.squares[m.from_index]) == PAWN {
                if rank(attackers[0].0) == rank(m.from_index) 
                    && file(attackers[0].0) == file(m.to_index) {
                        result.push(m);
                        continue;
                }
            }
            if m.from_index != king_square && m.to_index != attackers[0].0 {
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
        } else if m.to_index == attackers[0].0 {
            result.push(m);
        }
    }

    result
}

/// Find pieces attacking the piece on the given square.
fn attackers_of_square(
    position: &Position0x88,
    piece_square: SquareIndex,
) -> Vec<SquareAndPiece> {
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
        if piece_type == KING {
            continue;
        }

        // Presumably pawn attacks are checked elsewhere.
        if piece_type == PAWN {
            continue;
        }

        if PIECE_DIRECTIONS[piece_type as usize].contains(&dir) {
            result.push(possible_attacker);
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

pub fn allowed_directions(position: &Position0x88, piece_square: SquareIndex) -> PieceDirections {
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
    position: &Position0x88,
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
    position: &Position0x88,
    square: SquareIndex,
    moving_colour: Colour,
    test_piece_type: PieceType,
) -> bool {
    let piece_directions = match test_piece_type {
        PAWN => PAWN_CAPTURE_DIRECTIONS[moving_colour as usize],
        piece_type => PIECE_DIRECTIONS[piece_type as usize],
    };

    debug_assert!(test_piece_type != EMPTY);

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

        debug_assert!(attack_piece != EMPTY);

        return true;
    }

    return false;
}

pub fn is_square_attacked_by_slider(
    position: &Position0x88,
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

/// Check that the king is not moving into check.
/// Note that this doesn't check it's an actual legal move.
/// TODO: Rename this function.
pub fn is_legal_king_move(position: &Position0x88, move_to_test: &Move, moving_colour: Colour) -> bool {
    !is_square_attacked(
        position,
        move_to_test.to_index,
        moving_colour,
        Some(move_to_test.from_index),
    )
}

pub fn create_sliding_moves(
    position: &Position0x88,
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
            if !is_valid_square(next_square) {
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
    position: &Position0x88,
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

pub fn side_to_move_in_check(position: &Position0x88) -> bool {
    is_square_attacked(
        position,
        position.king_squares[position.side_to_move as usize],
        position.side_to_move,
        None,
    )
}

/// Can check be evaded? Along with side_to_move_in_check() this determines checkmate.
pub fn can_evade_check(position: &Position0x88) -> bool {
    debug_assert!(side_to_move_in_check(position));

    let king_square = position.king_squares[position.side_to_move as usize];

    // First check for legal moves / captures.
    for mut dir in PIECE_DIRECTIONS[KING as usize] {
        if dir == 0 {
            break;
        }
        if position.side_to_move == BLACK {
            dir = -dir;
        }
        let to_square = king_square as i16 + dir;
        if !is_valid_square(to_square) {
            continue;
        }
        if position.squares[to_square as SquareIndex] == EMPTY {
            if !is_square_attacked(&position, to_square as SquareIndex, position.side_to_move, None) {
                return true;
            } else {
                continue;
            }
        } else {
            let piece_colour = piece_colour(position.squares[to_square as SquareIndex]).unwrap();
            // Blocked by own piece.
            if piece_colour == position.side_to_move {
                continue;
            }
            // Can capture the piece.
            if !is_square_attacked(&position, to_square as SquareIndex, position.side_to_move, None) {
                return true;
            }
        }
    }

    // Check for capturing checking piece.
    let checking_pieces = attackers_of_square(&position, king_square);
    
    debug_assert!(checking_pieces.len() != 0);

    // Make sure it's not double check.
    if checking_pieces.len() > 1 {
        return false;
    }

    let (checking_piece_square, checking_piece) = checking_pieces[0];
    
    let attackers = attackers_of_square(&position, checking_piece_square);

    for (attacker_square, _attacking_piece) in attackers {
        if attacker_square == king_square {
            // We have already checked the king can't capture any of the pieces around it.
            continue;
        }
        let pin_dir = pinned_against_king_direction(&position, attacker_square);
        match pin_dir {
            None => {return true; },
            Some(dir) => {
                let check_dir = direction(checking_piece_square, king_square);
                match check_dir {
                    None => continue,
                    Some(d) => {
                        if dir != d {
                            return true;
                        }
                    }
                }
            }
        }
    }

    // Check for interjections.
    let checking_piece_type = piece_type(checking_piece);
    if !is_sliding_piece(checking_piece_type) {
        return false;
    }
    let opt_direction_from_king = direction(king_square, checking_piece_square);

    debug_assert!(opt_direction_from_king.is_some());

    let direction_from_king = opt_direction_from_king.unwrap();

    let mut interject_square = king_square as i16;

    while interject_square != checking_piece_square as i16 {
        debug_assert!(is_valid_square(interject_square as i16));

        interject_square += direction_from_king;

        if interject_square as usize == position.ep_square {
            for dir in PAWN_CAPTURE_DIRECTIONS[position.side_to_move as usize] {
                // Minus is because we're looking for the source from the target.
                let poss_pawn_square = interject_square - dir;
                if !is_valid_square(poss_pawn_square) {
                    continue;
                }
                let poss_piece = position.squares[poss_pawn_square as usize];
                if piece_type(poss_piece) == PAWN 
                    && piece_colour(poss_piece).unwrap() == position.side_to_move {
                        return true;
                    }
            }
        }

        // Can't use is_square_attacked() as-is as it uses pawn attacks not moves, and we need
        // moves here.

        for (the_piece_type, piece_dirs) in PIECE_DIRECTIONS.iter().enumerate() {
            let pt = the_piece_type as PieceType;
            if [EMPTY, KING].contains(&pt) {
                continue;
            }
            for dir in piece_dirs {
                if *dir == 0i16 {
                    break;
                }
                for i in 1..8 {
                    let test_square = interject_square as Direction + i * dir;
                    if !is_valid_square(test_square) {
                        break;
                    }
                    let piece = position.squares[test_square as usize];
                    if piece == EMPTY {
                        continue;
                    }
                    if piece_colour(piece).unwrap() != position.side_to_move {
                        break;
                    }
                    if the_piece_type == piece_type(piece).into() {
                        if pinned_against_king_direction(&position, test_square as usize).is_some() {
                            break;
                        }
                        return true;
                    }
                }
            }
        }

    }

    // DON'T FORGET - if a slider check is through the e.p. square, a pawn capture could block it!
    // (They can't any other time as the captured piece would block the check anyway)

    // todo!("This still needs to check for interjections");

    false
}

pub fn is_sliding_piece(piece_type: PieceType) -> bool {
    return piece_type == QUEEN || piece_type == ROOK || piece_type == BISHOP;
}

/// Check if the piece is pinned against the king, and return the direction of the pin if it is.
pub fn pinned_against_king_direction(
    position: &Position0x88,
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
pub const fn is_valid_square(square: i16) -> bool {
    !(square < 0 || (square & 0x88 != 0))
}

pub fn next_piece_in_direction(
    position: &Position0x88,
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
    use std::{env, fs};

    use chess_uci::messages::LongAlgebraicNotationMove;

    use crate::{position0x88::{
        get_piece,
        notation::{set_from_fen, set_startpos},
        BLACK, KING, PAWN, WHITE,
    }, perft::{perft, divide}, position::SetPosition};

    use super::*;

    const PERFT_POSITIONS: [&str; 3] = [
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -",
        "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -",
        "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq -",
    ];

    const PERFT_COUNT_1: [usize; 3] = [48, 14, 6];

    #[test]
    fn test_generate_moves() {
        let mut pos = Position0x88::default();
        set_startpos(&mut pos);
        let m = pos.generate_moves();
        assert_eq!(20, m.len());
    }

    #[test]
    fn test_generate_moves_check() {
        let mut position = Position0x88::default();
        set_from_fen(
            &mut position,
            "rnbk1bnr/6pp/4Pp2/1N6/4PB2/P4N2/P3BPPP/R2R2K1 b - - 1 15 ",
        )
        .expect("Failed");
        let m = position.generate_moves();
        for mov in &m {
            let m2: LongAlgebraicNotationMove = mov.into();
            print!("{:#?}", m2);
        }
        assert_eq!(5, m.len());
    }

    #[test]
    fn test_create_non_sliding_moves() {
        let mut position = Position0x88::default();
        set_from_fen(
            &mut position,
            "rnb1kbnr/ppq1pppp/2pp4/8/6P1/2P5/PP1PPPBP/RNBQK1NR w KQkq -",
        )
        .unwrap();
        let mut r: Vec<Move> = vec![];
        create_non_sliding_moves(
            &position,
            position.king_squares[WHITE as usize],
            KING,
            &mut r,
        );
        assert_eq!(1, r.len());
        assert_eq!(0x04, r[0].from_index);
        assert_eq!(0x05, r[0].to_index);
    }

    #[test]
    fn test_generate_moves_perft() {
        for (i, fen) in PERFT_POSITIONS.iter().enumerate() {
            let mut pos = Position0x88::default();
            let mut full_fen = fen.to_owned().to_string();
            full_fen.push_str(" 0 1");
            set_from_fen(&mut pos, &full_fen).expect("Valid FEN");
            let m = pos.generate_moves();
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
        let mut pos = Position0x88::default();
        pos.set_square_to_piece(0x44, PAWN);
        pos.set_square_to_piece(0x42, get_piece(ROOK, BLACK));
        pos.set_square_to_piece(0x46, KING);
        assert_eq!(1, pinned_against_king_direction(&pos, 0x44).unwrap());
        pos.set_square_to_piece(0x42, EMPTY);
        assert_eq!(None, pinned_against_king_direction(&pos, 0x44));
    }

    #[test]
    fn test_side_to_move_in_check() {
        let mut pos = Position0x88::default();
        set_from_fen(
            &mut pos,
            "rnbk1bnr/6pp/4Pp2/1N6/4PB2/P4N2/P3BPPP/R2R2K1 b - - 1 15 ",
        )
        .expect("Aaargh!");
        assert_eq!(true, side_to_move_in_check(&pos));
    }

    #[test]
    fn test_can_evade_check() {
        let pos: Position0x88 = "5k2/8/8/8/7b/1N6/5q2/5K2 w - - 0 1".into();
        assert!(!can_evade_check(&pos));
        let pos: Position0x88 = "5k2/8/8/8/5q1b/1N6/8/5K2 w - - 0 1".into();
        assert!(can_evade_check(&pos));
        let pos: Position0x88 = "5k2/8/8/6r1/7b/1N6/4q3/5K2 w - - 0 1".into();
        assert!(can_evade_check(&pos));

        // Can take the checking piece.
        let pos: Position0x88 = "5k2/8/8/8/7b/3N4/5q2/5K2 w - - 0 1".into();
        assert!(can_evade_check(&pos));

        // Double check.
        let pos: Position0x88 = "5k2/8/8/1bN2qr1/8/8/8/r2Q1K2 w - - 0 1".into();
        assert!(can_evade_check(&pos));
        let pos: Position0x88 = "5k2/8/8/1bN2qr1/8/8/8/r3QK2 w - - 0 1".into();
        assert!(!can_evade_check(&pos));
        let pos: Position0x88 = "5k2/8/8/1b3qr1/8/8/8/r3QK2 w - - 0 1".into();
        assert!(!can_evade_check(&pos));

        // Interjection.
        let pos: Position0x88 = "5k2/8/8/5qr1/8/8/4P3/r2NQK2 w - - 0 1".into();
        assert!(can_evade_check(&pos));
        let pos: Position0x88 = "5k2/8/8/5qr1/8/8/4P3/r3QK2 w - - 0 1".into();
        assert!(!can_evade_check(&pos));
        let pos: Position0x88 = "5k2/8/7Q/5qr1/8/8/4P3/3N1K2 b - - 0 1".into();
        assert!(can_evade_check(&pos));

        // Pawn interjection 
        let pos: Position0x88 = "4qk2/8/8/P1P5/KB6/RN6/8/8 w - - 0 1".into();
        assert!(can_evade_check(&pos));
        let pos: Position0x88 = "8/4rbr1/4pkp1/5pp1/8/8/1B6/1K6 b - - 0 1".into();
        assert!(can_evade_check(&pos));

        // Interjection of e.p. capture.
        let pos: Position0x88 = "4qk2/8/8/P1pP4/KB6/RN6/8/8 w - c6 0 1".into();
        assert!(can_evade_check(&pos));
        
        
    }

    #[test]
    fn test_ep_pin() {
        let mut pos = Position0x88::default();
        let fen = "8/5K2/8/3pP3/8/8/b4k2/8 w - d6 0 1";
        set_from_fen(&mut pos, fen).unwrap();
        assert!(is_en_passent_pin(&pos, 0x44));

        let mut pos2 = Position0x88::default();
        let fen2 = "3K4/8/8/3pP3/8/8/5k2/3r4 b - d6 0 1";
        set_from_fen(&mut pos2, fen2).unwrap();
        assert!(!is_en_passent_pin(&pos2, 0x44));
    }

    #[test]
    fn test_ep_horizontal_pin() {
        let mut pos = Position0x88::default();
        set_from_fen(&mut pos, "1k6/8/8/r2pP1K1/8/8/8/8 w - d6 0 1 ").unwrap();
        assert!(is_en_passent_pin(&pos, 0x44));
    }

    #[test]
    fn test_ep_vertical_pin() {
        let mut pos = Position0x88::default();
        set_from_fen(&mut pos, "rB5r/pp4k1/5n2/q3p2p/Pb3pP1/1P1P3p/R2QPP2/1N2KBR1 b - g3 0 2").unwrap();
        assert!(!is_en_passent_pin(&pos, 0x35));
    }

    #[test]
    fn test_move_gen_temp() {
        let mut position: Position0x88 = Position0x88::default();
        // let fen = "r3kbnr/8/1p1B1q2/p2b1p1P/PpPp4/4Q2N/3PPP1P/RN2KB1R b KQkq - 0 1".to_string();
        // let fen = "rb6/5b2/1p2r3/p1k1P3/PpPPp3/2R4P/8/1N1K2R1 b - d3 0 1".to_string();
        // let fen = "r3kbnr/2qn2p1/8/pppBpp1P/3P1Pb1/P1P1P3/1P2Q2P/RNB1K1NR w KQkq - 0 1".to_string();
        // let fen = "B3kbnr/2qn2p1/8/ppp1pp1P/3P1Pb1/P1P1P3/1P2Q2P/RNB1K1NR b KQkq - 0 1".to_string();
        // let fen = "rb6/8/1pr5/p1k1P3/PpbPp3/5R1P/6R1/1N1K4 b - d3 0 2".to_string();
        // let fen = "2b1kbn1/r1pq4/n2p3p/3Pp1p1/ppP2PP1/PPB4P/Q4P2/RN2KBNR w KQ e6 0 3".to_string();
        let fen = "rB5r/pp4k1/5n2/q3p2p/Pb3pP1/1P1P3p/R2QPP2/1N2KBR1 b - g3 0 2".to_string();
        set_from_fen(&mut position, &fen).unwrap();
        let moves = divide(&mut position, 1);
        println!("{:#?}", moves);
    }

    #[test]
    fn test_move_gen() {
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
            let mut position = Position0x88::default();
            set_from_fen(&mut position, fen).unwrap();

            println!("{}: {}", line_no, fen);
            line_no += 1;
            let perft1 = perft(&mut position, 1);
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



}
