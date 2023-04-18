use crate::{
    bitboards::{
        Bitboard, SixtyFourBitboards, SquareIndex64, BETWEEN, DIR_ALL_SLIDERS, FILE_MASK,
        RANK_MASK, SLIDER_DIRECTION_SQUARE,
    },
    position::SQUARE_COUNT,
};

#[derive(Clone, Copy, Debug, Default)]
struct MagicEntry {
    mask: u64,
    magic: u64,
    offset: usize,
}

/// Magic numbers for rook attacks, pre-generated by trial and error.
const ROOK_MAGICS: SixtyFourBitboards = [
    0x5080008011400020,
    0x140001000402000,
    0x280091000200480,
    0x700081001002084,
    0x1200082044100200,
    0x8100040008020900,
    0x900108100440200,
    0x8c80030000492080,
    0x1162800020400c90,
    0x40a4400020100040,
    0xa00080200082d000,
    0x100100021000a,
    0x800400800800,
    0x1126000200108804,
    0x12000a00040108,
    0x2901000200815900,
    0x240028024800041,
    0x420010100400080,
    0x6060012208042,
    0x1022000a029040,
    0x31010010048800,
    0x116808042002400,
    0x1020040008900112,
    0x82c00200041080c1,
    0x400024800880c0,
    0x20500140052002,
    0x20018480205000,
    0x8200a00401200,
    0x120800800c0180,
    0x1126000200108804,
    0x12000a00040108,
    0x40011120000a044,
    0x400060800481,
    0x1000200242401000,
    0x4010020010010c0,
    0x2c42808800801001,
    0x800400800800,
    0x10808200800400,
    0x800116651c000810,
    0xc024982000104,
    0x848000e000c04000,
    0x60004000808024,
    0x200200041010016,
    0x100100021000a,
    0x408008024018008,
    0x809e001008420004,
    0x4000084230440001,
    0x200200510482000c,
    0x2004610040800100,
    0x10c00423028900,
    0x2003100060038480,
    0x8200a00401200,
    0x120800800c0180,
    0x40800400020080,
    0x1005504201182400,
    0x900108100440200,
    0x4001004210a28009,
    0x10110080400028d1,
    0x108a00043001309,
    0x806810000d002089,
    0x125000800040611,
    0x212000824100906,
    0x809215010820804,
    0x9001010020804402,
];

const BISHOP_MAGICS: SixtyFourBitboards = [
    0x40420094048181,
    0x14100c86008400,
    0x12300c0040400020,
    0x42442c1080040000,
    0x8902061050002000,
    0x1022451040081140,
    0x1a080124101001,
    0x2c2a010048140404,
    0x80c86045020228,
    0x180808028020,
    0x2000100c18424080,
    0x5142080481000610,
    0x120031040090080,
    0x14820184a02128,
    0x802004802101030,
    0x8001424048280820,
    0xb20041488100120,
    0x22002408483500,
    0x2202004104130200,
    0x14000804105000,
    0x4006080a00044,
    0x2c2a010048140404,
    0x2c2a010048140404,
    0x8020848044040100,
    0x8e405020090202,
    0x2244108002060800,
    0x24040002080210,
    0x40800a22200c0,
    0x490840120802001,
    0x838120000412080,
    0x2408010001444a50,
    0x8080010400a0,
    0x10101000280240,
    0x108080400320400,
    0xc004500080200,
    0x50801a0080080080,
    0x8020400841010,
    0x2a60028080010040,
    0x20c0e40140222,
    0xc004040044012700,
    0x3ea11009200a2409,
    0x2021004003200,
    0x280101088201000,
    0x1080204022083020,
    0x8000081014012040,
    0x20040082000020,
    0x600c04448014d4,
    0x9086a02400480,
    0x1a080124101001,
    0x3010105604000,
    0x40420094048181,
    0x9000040042120004,
    0x803040800,
    0x8409801010000,
    0x10400224040081a0,
    0x14100c86008400,
    0x2c2a010048140404,
    0x8001424048280820,
    0x8000000204560810,
    0x200048200420200,
    0x10420200,
    0x3268001020090100,
    0x80c86045020228,
    0x40420094048181,
];

pub struct SquareMagics {
    rook_magic: MagicEntry,
    bishop_magic: MagicEntry,
    rook_attacks: [Bitboard; 4096],
    bishop_attacks: [Bitboard; 512],
}

impl SquareMagics {
    const fn generate(square: SquareIndex64) -> SquareMagics {
        let (rook_magic, rook_attacks) = generate_rook_magic(square);
        let (bishop_magic, bishop_attacks) = generate_bishop_magic(square);
        SquareMagics {
            rook_magic,
            bishop_magic,
            rook_attacks,
            bishop_attacks,
        }
    }
    pub fn rook_attacks(&self, blockers: u64) -> u64 {
        let magic_entry = self.rook_magic;
        let blockers_masked = blockers & magic_entry.mask;
        let index =
            ((blockers_masked.wrapping_mul(magic_entry.magic)) >> magic_entry.offset) as usize;
        self.rook_attacks[index]
    }
    pub fn bishop_attacks(&self, blockers: u64) -> u64 {
        let magic_entry = self.bishop_magic;
        let blockers_masked = blockers & magic_entry.mask;
        let index =
            ((blockers_masked.wrapping_mul(magic_entry.magic)) >> magic_entry.offset) as usize;
        self.bishop_attacks[index]
    }
}

// This nonsense is to get around the const eval limit.
const MAGICS_A1: SquareMagics = SquareMagics::generate(0);
const MAGICS_B1: SquareMagics = SquareMagics::generate(1);
const MAGICS_C1: SquareMagics = SquareMagics::generate(2);
const MAGICS_D1: SquareMagics = SquareMagics::generate(3);
const MAGICS_E1: SquareMagics = SquareMagics::generate(4);
const MAGICS_F1: SquareMagics = SquareMagics::generate(5);
const MAGICS_G1: SquareMagics = SquareMagics::generate(6);
const MAGICS_H1: SquareMagics = SquareMagics::generate(7);
const MAGICS_A2: SquareMagics = SquareMagics::generate(8);
const MAGICS_B2: SquareMagics = SquareMagics::generate(9);
const MAGICS_C2: SquareMagics = SquareMagics::generate(10);
const MAGICS_D2: SquareMagics = SquareMagics::generate(11);
const MAGICS_E2: SquareMagics = SquareMagics::generate(12);
const MAGICS_F2: SquareMagics = SquareMagics::generate(13);
const MAGICS_G2: SquareMagics = SquareMagics::generate(14);
const MAGICS_H2: SquareMagics = SquareMagics::generate(15);
const MAGICS_A3: SquareMagics = SquareMagics::generate(16);
const MAGICS_B3: SquareMagics = SquareMagics::generate(17);
const MAGICS_C3: SquareMagics = SquareMagics::generate(18);
const MAGICS_D3: SquareMagics = SquareMagics::generate(19);
const MAGICS_E3: SquareMagics = SquareMagics::generate(20);
const MAGICS_F3: SquareMagics = SquareMagics::generate(21);
const MAGICS_G3: SquareMagics = SquareMagics::generate(22);
const MAGICS_H3: SquareMagics = SquareMagics::generate(23);
const MAGICS_A4: SquareMagics = SquareMagics::generate(24);
const MAGICS_B4: SquareMagics = SquareMagics::generate(25);
const MAGICS_C4: SquareMagics = SquareMagics::generate(26);
const MAGICS_D4: SquareMagics = SquareMagics::generate(27);
const MAGICS_E4: SquareMagics = SquareMagics::generate(28);
const MAGICS_F4: SquareMagics = SquareMagics::generate(29);
const MAGICS_G4: SquareMagics = SquareMagics::generate(30);
const MAGICS_H4: SquareMagics = SquareMagics::generate(31);
const MAGICS_A5: SquareMagics = SquareMagics::generate(32);
const MAGICS_B5: SquareMagics = SquareMagics::generate(33);
const MAGICS_C5: SquareMagics = SquareMagics::generate(34);
const MAGICS_D5: SquareMagics = SquareMagics::generate(35);
const MAGICS_E5: SquareMagics = SquareMagics::generate(36);
const MAGICS_F5: SquareMagics = SquareMagics::generate(37);
const MAGICS_G5: SquareMagics = SquareMagics::generate(38);
const MAGICS_H5: SquareMagics = SquareMagics::generate(39);
const MAGICS_A6: SquareMagics = SquareMagics::generate(40);
const MAGICS_B6: SquareMagics = SquareMagics::generate(41);
const MAGICS_C6: SquareMagics = SquareMagics::generate(42);
const MAGICS_D6: SquareMagics = SquareMagics::generate(43);
const MAGICS_E6: SquareMagics = SquareMagics::generate(44);
const MAGICS_F6: SquareMagics = SquareMagics::generate(45);
const MAGICS_G6: SquareMagics = SquareMagics::generate(46);
const MAGICS_H6: SquareMagics = SquareMagics::generate(47);
const MAGICS_A7: SquareMagics = SquareMagics::generate(48);
const MAGICS_B7: SquareMagics = SquareMagics::generate(49);
const MAGICS_C7: SquareMagics = SquareMagics::generate(50);
const MAGICS_D7: SquareMagics = SquareMagics::generate(51);
const MAGICS_E7: SquareMagics = SquareMagics::generate(52);
const MAGICS_F7: SquareMagics = SquareMagics::generate(53);
const MAGICS_G7: SquareMagics = SquareMagics::generate(54);
const MAGICS_H7: SquareMagics = SquareMagics::generate(55);
const MAGICS_A8: SquareMagics = SquareMagics::generate(56);
const MAGICS_B8: SquareMagics = SquareMagics::generate(57);
const MAGICS_C8: SquareMagics = SquareMagics::generate(58);
const MAGICS_D8: SquareMagics = SquareMagics::generate(59);
const MAGICS_E8: SquareMagics = SquareMagics::generate(60);
const MAGICS_F8: SquareMagics = SquareMagics::generate(61);
const MAGICS_G8: SquareMagics = SquareMagics::generate(62);
const MAGICS_H8: SquareMagics = SquareMagics::generate(63);

pub const SQUARE_MAGICS: [SquareMagics; 64] = [
    MAGICS_A1, MAGICS_B1, MAGICS_C1, MAGICS_D1, MAGICS_E1, MAGICS_F1, MAGICS_G1, MAGICS_H1,
    MAGICS_A2, MAGICS_B2, MAGICS_C2, MAGICS_D2, MAGICS_E2, MAGICS_F2, MAGICS_G2, MAGICS_H2,
    MAGICS_A3, MAGICS_B3, MAGICS_C3, MAGICS_D3, MAGICS_E3, MAGICS_F3, MAGICS_G3, MAGICS_H3,
    MAGICS_A4, MAGICS_B4, MAGICS_C4, MAGICS_D4, MAGICS_E4, MAGICS_F4, MAGICS_G4, MAGICS_H4,
    MAGICS_A5, MAGICS_B5, MAGICS_C5, MAGICS_D5, MAGICS_E5, MAGICS_F5, MAGICS_G5, MAGICS_H5,
    MAGICS_A6, MAGICS_B6, MAGICS_C6, MAGICS_D6, MAGICS_E6, MAGICS_F6, MAGICS_G6, MAGICS_H6,
    MAGICS_A7, MAGICS_B7, MAGICS_C7, MAGICS_D7, MAGICS_E7, MAGICS_F7, MAGICS_G7, MAGICS_H7,
    MAGICS_A8, MAGICS_B8, MAGICS_C8, MAGICS_D8, MAGICS_E8, MAGICS_F8, MAGICS_G8, MAGICS_H8,
];

const BLOCKER_MASKS: [SixtyFourBitboards; 2] = generate_blocker_masks();

const fn generate_bishop_magic(square: SquareIndex64) -> (MagicEntry, [Bitboard; 512]) {
    let mask = BLOCKER_MASKS[MagicType::Bishop as usize][square as usize];
    let magic = BISHOP_MAGICS[square as usize];

    let offset = 64 - mask.count_ones() as usize;
    let mut attacks = [0; 512];
    let mut mask_subset = 0u64;
    loop {
        let index = ((mask_subset.wrapping_mul(magic)) >> offset) as usize;
        let attack_moves = moves(square as SquareIndex64, mask_subset, MagicType::Bishop);
        if attacks[index] != 0 && attacks[index] != attack_moves {
            panic!("Index collision!");
        }
        attacks[index] = attack_moves;
        mask_subset = (mask_subset.wrapping_sub(mask)) & mask;
        if mask_subset == 0 {
            break;
        }
    }

    (
        MagicEntry {
            mask,
            magic,
            offset,
        },
        attacks,
    )
}

const fn generate_rook_magic(square: SquareIndex64) -> (MagicEntry, [Bitboard; 4096]) {
    let mask = BLOCKER_MASKS[MagicType::Rook as usize][square as usize];
    let magic = ROOK_MAGICS[square as usize];

    let offset = 64 - mask.count_ones() as usize;
    let mut attacks = [0; 4096];
    let mut mask_subset = 0u64;
    loop {
        let index = ((mask_subset.wrapping_mul(magic)) >> offset) as usize;
        let attack_moves = moves(square as SquareIndex64, mask_subset, MagicType::Rook);
        if attacks[index] != 0 && attacks[index] != attack_moves {
            panic!("Index collision!");
        }
        attacks[index] = attack_moves;
        mask_subset = (mask_subset.wrapping_sub(mask)) & mask;
        if mask_subset == 0 {
            break;
        }
    }

    (
        MagicEntry {
            mask,
            magic,
            offset,
        },
        attacks,
    )
}
enum MagicType {
    Rook,
    Bishop,
}

const fn generate_blocker_masks() -> [SixtyFourBitboards; 2] {
    let mut masks = [[0; 64]; 2];
    let mut sq = 0;
    while sq < SQUARE_COUNT {
        masks[MagicType::Rook as usize][sq] = (SLIDER_DIRECTION_SQUARE[0][sq] & !FILE_MASK[0])
            | (SLIDER_DIRECTION_SQUARE[1][sq] & !FILE_MASK[7])
            | (SLIDER_DIRECTION_SQUARE[2][sq] & !RANK_MASK[7])
            | (SLIDER_DIRECTION_SQUARE[3][sq] & !RANK_MASK[0]);
        masks[MagicType::Bishop as usize][sq] = (SLIDER_DIRECTION_SQUARE[4][sq]
            | SLIDER_DIRECTION_SQUARE[5][sq]
            | SLIDER_DIRECTION_SQUARE[6][sq]
            | SLIDER_DIRECTION_SQUARE[7][sq])
            & !(RANK_MASK[0] | RANK_MASK[7] | FILE_MASK[0] | FILE_MASK[7]);
        sq += 1;
    }
    masks
}

const fn moves(square: SquareIndex64, blockers: u64, magic_type: MagicType) -> u64 {
    let mut moves = 0;
    let dirs = match magic_type {
        MagicType::Rook => (0, 4),
        MagicType::Bishop => (4, 8),
    };
    let mut dir = dirs.0;
    while dir < dirs.1 {
        let ray = SLIDER_DIRECTION_SQUARE[dir][square as usize];
        let blockers_on_ray = blockers & ray;
        if blockers_on_ray == 0 {
            moves |= ray;
            dir += 1;
            continue;
        }
        let next_blocker = if DIR_ALL_SLIDERS[dir] > 0 {
            blockers_on_ray.trailing_zeros() as SquareIndex64
        } else {
            blockers_on_ray.ilog2() as SquareIndex64
        };

        moves |= BETWEEN[square as usize][next_blocker as usize];
        moves |= 1 << next_blocker;
        dir += 1;
    }
    moves
}

pub const fn next_seed(last: u128) -> u128 {
    last.wrapping_mul(0x2360ED051FC65DA44385DF649FCCF645)
}

pub const fn next_u64(seed: u128) -> u64 {
    let rot = (seed >> 122) as u32;
    let xsl = (seed >> 64) as u64 ^ seed as u64;
    xsl.rotate_right(rot)
}

#[cfg(test)]
mod test {

    use crate::bitboards::BitboardDisplay;

    use super::*;

    #[test]
    fn test_blocker_masks() {
        println!(
            "{}",
            BLOCKER_MASKS[MagicType::Rook as usize][35].bitboard_to_string()
        );
        println!(
            "{}",
            BLOCKER_MASKS[MagicType::Bishop as usize][35].bitboard_to_string()
        );

        let mut max_bishop = 0;
        for sq in 0..64 {
            let count = BLOCKER_MASKS[MagicType::Bishop as usize][sq].count_ones();
            if count > max_bishop {
                max_bishop = count;
            }
        }
        println!("Max bishop: {}, {}", max_bishop, 1 << max_bishop);

        let mut max_rook = 0;
        for sq in 0..64 {
            let count = BLOCKER_MASKS[MagicType::Rook as usize][sq].count_ones();
            if count > max_rook {
                max_rook = count;
            }
        }
        println!("Max rook: {}, {}", max_rook, 1 << max_rook);
    }

    #[test]
    fn test_moves() {
        let blockers = 0x0000000000000000;
        let mvs = moves(35, blockers, MagicType::Rook);
        assert_eq!(mvs, 0x80808f708080808);

        let blockers = 0x282800f10808f808;
        let mvs = moves(35, blockers, MagicType::Rook);
        assert_eq!(mvs, 0x8081708000000);

        let blockers = 0x0000000000000000;
        let mvs = moves(35, blockers, MagicType::Bishop);
        assert_eq!(mvs, 0x4122140014224180);
    }

    #[test]
    fn temp_count_moves() {
        let mut sum = 0;
        for square in 0..64 {
            let mut moves_in_dir = [0; 4];
            for dir in 5..8 {
                moves_in_dir[dir - 5] = SLIDER_DIRECTION_SQUARE[dir][square].count_ones();
            }
            let count = moves_in_dir
                .iter()
                .filter(|x| *x != &0)
                .fold(1, |x, y| x * y);

            // println!("Square: {}, moves: {:?}", square, count);
            sum += count;

            let mask = BLOCKER_MASKS[MagicType::Bishop as usize][square as usize];
            let attacks_size = 1usize << mask.count_ones();
            let offset = 64 - attacks_size.ilog2();

            let mut attacks;
            let mut magic;
            let mut rnd: u128 = 0xE926E6210D9E3486 | 1;
            'magic: loop {
                rnd = next_seed(rnd);
                magic = next_u64(rnd);
                rnd = next_seed(rnd);
                magic &= next_u64(rnd);
                rnd = next_seed(rnd);
                magic &= next_u64(rnd);
                attacks = vec![0; attacks_size];
                let mut mask_subset = 0u64;
                loop {
                    let index = ((mask_subset.wrapping_mul(magic)) >> offset) as usize;
                    let attack_moves =
                        moves(square as SquareIndex64, mask_subset, MagicType::Bishop);
                    if attacks[index] != 0 && attacks[index] != attack_moves {
                        continue 'magic;
                    }
                    attacks[index] = attack_moves;
                    mask_subset = (mask_subset.wrapping_sub(mask)) & mask;
                    if mask_subset == 0 {
                        break 'magic;
                    }
                }
            }

            let offset_usize = offset as usize;

            let entry = (
                MagicEntry {
                    mask,
                    magic,
                    offset: offset_usize,
                },
                attacks,
            );

            print!("{:#x}, ", entry.0.magic);
            // println!("Square: {}, magic: {:#?}", square, entry.0);
        }
        println!("Total: {}", sum);
    }

    #[test]
    fn test_rook_magics() {
        let rook_moves = SQUARE_MAGICS[35].rook_attacks(0x80800210000fffd);
        assert_eq!(rook_moves, 0x8083708080800);
    }

    #[test]
    fn test_bishop_magics() {
        let bishop_moves = SQUARE_MAGICS[35].bishop_attacks(0x80800210000fffd);
        assert_eq!(bishop_moves, 0x4122140014224100);
        let bishop_moves = SQUARE_MAGICS[35].bishop_attacks(0x80800211000fffd);
        assert_eq!(bishop_moves, 0x4122140014020100);
    }
}