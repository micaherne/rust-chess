use crate::position::PIECE_COUNT;

pub type ZobristNumber = u64;
pub type ZobristPieceSquare = [ZobristNumber; 64];

#[derive(Debug, Clone, Copy)]
pub struct ZobristNumbers {
    pub piece_square: [[ZobristPieceSquare; PIECE_COUNT]; 3],
    pub black_to_move: ZobristNumber,
    pub castling_rights: [ZobristNumber; 4],
    pub ep_file: [ZobristNumber; 8],
}

pub const ZOBRIST_NUMBERS: ZobristNumbers = generate_zobrist_numbers();

const fn generate_zobrist_numbers() -> ZobristNumbers {
    let mut i: u32 = 1;
    let seed = 321u64;

    let mut result = ZobristNumbers {
        piece_square: [[[0; 64]; PIECE_COUNT]; 3],
        black_to_move: 0,
        castling_rights: [0; 4],
        ep_file: [0; 8],
    };
    let mut piece = 0;
    while piece < PIECE_COUNT {
        let mut square = 0;
        while square < 64 {
            let mut colour = 0;
            while colour < 3 {
                result.piece_square[colour as usize][piece][square] = seed.wrapping_pow(i);
                i += 1;
                colour += 1;
            }
            square += 1;
        }
        piece += 1;
    }

    result.black_to_move = seed.wrapping_pow(i);
    i += 1;
    let mut j = 0;
    while j < 4 {
        result.castling_rights[j] = seed.wrapping_pow(i);
        i += 1;
        j += 1;
    }

    let mut j = 0;
    while j < 8 {
        result.ep_file[j] = seed.wrapping_pow(i);
        i += 1;
        j += 1;
    }

    result
}
