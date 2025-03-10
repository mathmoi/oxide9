use std::sync::OnceLock;

use crate::chess::{Bitboard, Color, File, Piece, PieceType, Position, Square};

mod naive_sliders {
    use crate::chess::{Bitboard, Square};

    /// Returns a bitboard with all squares attacked by a rook on a given square. This function has
    /// poor performance and should not be used during move generation.
    pub fn attacks_from_rook(occupied: Bitboard, from_sq: Square) -> Bitboard {
        let mut attacks = Bitboard::EMPTY;

        let directions = [Square::up, Square::down, Square::left, Square::right];
        for direction in directions.iter() {
            let mut to = from_sq;
            while let Ok(sq) = direction(to, 1) {
                to = sq;
                attacks |= to;
                if occupied.get(to) {
                    break;
                }
            }
        }

        attacks
    }

    /// Returns a bitboard with all squares attacked by a bishop on a given square. This function
    /// has poor performance and should not be used during move generation.
    pub fn attacks_from_bishop(occupied: Bitboard, from_sq: Square) -> Bitboard {
        let mut attacks = Bitboard::EMPTY;

        let directions = [
            |sq: Square| sq.up(1).and_then(|sq| sq.right(1)),
            |sq: Square| sq.up(1).and_then(|sq| sq.left(1)),
            |sq: Square| sq.down(1).and_then(|sq| sq.right(1)),
            |sq: Square| sq.down(1).and_then(|sq| sq.left(1)),
        ];

        for direction in directions.iter() {
            let mut to = from_sq;
            while let Ok(sq) = direction(to) {
                to = sq;
                attacks |= to;
                if occupied.get(to) {
                    break;
                }
            }
        }

        attacks
    }

    #[cfg(test)]
    mod tests {
        use super::*;

        #[test]
        fn test_attacks_from_rook_without_obstructions() {
            let sq = Square::E4;
            let occupied = Bitboard::from(Square::E4);
            let expected = Square::E1
                | Square::E2
                | Square::E3
                | Square::E5
                | Square::E6
                | Square::E7
                | Square::E8
                | Square::A4
                | Square::B4
                | Square::C4
                | Square::D4
                | Square::F4
                | Square::G4
                | Square::H4;
            let attacks = attacks_from_rook(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_attacks_from_rook_obstructions_on_edge() {
            let sq = Square::E4;
            let occupied = Square::E4 | Square::A4 | Square::H4 | Square::E1 | Square::E8;
            let expected = Square::E1
                | Square::E2
                | Square::E3
                | Square::E5
                | Square::E6
                | Square::E7
                | Square::E8
                | Square::A4
                | Square::B4
                | Square::C4
                | Square::D4
                | Square::F4
                | Square::G4
                | Square::H4;
            let attacks = attacks_from_rook(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_attacks_from_rook_obstructions_one_off_edge() {
            let sq = Square::E4;
            let occupied = Square::E4 | Square::B4 | Square::G4 | Square::E2 | Square::E7;
            let expected = Square::E2
                | Square::E3
                | Square::E5
                | Square::E6
                | Square::E7
                | Square::B4
                | Square::C4
                | Square::D4
                | Square::F4
                | Square::G4;
            let attacks = attacks_from_rook(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_attacks_from_rook_obstructions_next_to_piece() {
            let sq = Square::E4;
            let occupied = Square::E4 | Square::D4 | Square::F4 | Square::E3 | Square::E5;
            let expected = Square::D4 | Square::F4 | Square::E3 | Square::E5;
            let attacks = attacks_from_rook(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_attacks_from_rook_in_corner() {
            let sq = Square::A1;
            let occupied = Bitboard::from(Square::A1);
            let expected = Square::A2
                | Square::A3
                | Square::A4
                | Square::A5
                | Square::A6
                | Square::A7
                | Square::A8
                | Square::B1
                | Square::C1
                | Square::D1
                | Square::E1
                | Square::F1
                | Square::G1
                | Square::H1;
            let attacks = attacks_from_rook(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_attacks_from_bishop_without_obstructions() {
            let sq = Square::E4;
            let occupied = Bitboard::from(Square::E4);
            let expected = Square::D3
                | Square::C2
                | Square::B1
                | Square::F3
                | Square::G2
                | Square::H1
                | Square::D5
                | Square::C6
                | Square::B7
                | Square::A8
                | Square::F5
                | Square::G6
                | Square::H7;
            let attacks = attacks_from_bishop(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_atacks_from_bishop_obstructions_on_edge() {
            let sq = Square::E4;
            let occupied = Square::E4 | Square::B1 | Square::H7 | Square::A8 | Square::H1;
            let expected = Square::D3
                | Square::C2
                | Square::B1
                | Square::F3
                | Square::G2
                | Square::H1
                | Square::D5
                | Square::C6
                | Square::B7
                | Square::A8
                | Square::F5
                | Square::G6
                | Square::H7;
            let attacks = attacks_from_bishop(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_attacks_from_bishop_obstructions_one_off_edge() {
            let sq = Square::E4;
            let occupied = Square::E4 | Square::C2 | Square::G2 | Square::B7 | Square::G6;
            let expected = Square::D3
                | Square::C2
                | Square::F3
                | Square::G2
                | Square::D5
                | Square::C6
                | Square::B7
                | Square::F5
                | Square::G6;
            let attacks = attacks_from_bishop(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_attacks_from_bishop_obstructions_next_to_piece() {
            let sq = Square::E4;
            let occupied = Square::E4 | Square::D3 | Square::F3 | Square::D5 | Square::F5;
            let expected = Square::D3 | Square::F3 | Square::D5 | Square::F5;
            let attacks = attacks_from_bishop(occupied, sq);
            assert_eq!(expected, attacks);
        }

        #[test]
        fn test_attacks_from_bishop_in_corner() {
            let sq = Square::A1;
            let occupied = Bitboard::from(Square::A1);
            let expected = Square::B2 | Square::C3 | Square::D4 | Square::E5 | Square::F6 | Square::G7 | Square::H8;
            let attacks = attacks_from_bishop(occupied, sq);
            assert_eq!(expected, attacks);
        }
    }
}

mod pext_sliders {
    use std::{array, sync::OnceLock};

    use crate::chess::{Bitboard, File, Rank, Square};

    /// This structure contains the data required to perform the PEXT operation for a given square.   
    pub struct PextData {
        mask: Bitboard,
        lookup: Vec<Bitboard>,
    }

    //==================================================================================================================
    // Rook attacks
    //==================================================================================================================

    static ROOK_PEXT_DATA: OnceLock<[PextData; Square::COUNT]> = OnceLock::new();

    fn get_rook_pext_data_for_square(sq: Square) -> PextData {
        let first_and_last_rank: Bitboard = Rank::R1 | Rank::R8;
        let first_and_last_file: Bitboard = File::A | File::H;

        let mask = ((sq.file() & !first_and_last_rank) | (sq.rank() & !first_and_last_file)) & !Bitboard::from(sq);
        let lookup_size = 1u64 << mask.popcnt();
        let lookup: Vec<Bitboard> =
            (0..lookup_size).map(|index| super::naive_sliders::attacks_from_rook(mask.pdep(index), sq)).collect();

        PextData { mask, lookup }
    }

    fn get_rook_pext_data() -> &'static [PextData; Square::COUNT] {
        ROOK_PEXT_DATA.get_or_init(|| array::from_fn(|index| get_rook_pext_data_for_square(Square::from(index as u8))))
    }

    /// Returns a bitboard with all squares attacked by a rook on a given square.
    pub fn attacks_from_rook(occupied: Bitboard, from_sq: Square) -> Bitboard {
        let pext_data = &get_rook_pext_data()[usize::from(from_sq)];
        let index = occupied.pext(pext_data.mask);
        pext_data.lookup[index as usize]
    }

    //==================================================================================================================
    // Bishop attacks
    //==================================================================================================================

    static BISHOP_PEXT_DATA: OnceLock<[PextData; Square::COUNT]> = OnceLock::new();

    fn get_bishop_pext_data_for_square(sq: Square) -> PextData {
        let border = Rank::R1 | Rank::R8 | File::A | File::H;
        let mask = (Bitboard::from(sq.diagonal()) ^ Bitboard::from(sq.antidiagonal())) & !border;
        let lookup_size = 1u64 << mask.popcnt();
        let lookup: Vec<Bitboard> =
            (0..lookup_size).map(|index| super::naive_sliders::attacks_from_bishop(mask.pdep(index), sq)).collect();

        PextData { mask, lookup }
    }

    fn get_bishop_pext_data() -> &'static [PextData; Square::COUNT] {
        BISHOP_PEXT_DATA
            .get_or_init(|| array::from_fn(|index| get_bishop_pext_data_for_square(Square::from(index as u8))))
    }

    /// Returns a bitboard with all squares attacked by a bishop on a given square.
    pub fn attacks_from_bishop(occupied: Bitboard, from_sq: Square) -> Bitboard {
        let pext_data = &get_bishop_pext_data()[usize::from(from_sq)];
        let index = occupied.pext(pext_data.mask);
        pext_data.lookup[index as usize]
    }
}

// TODO : Check if making this a const has any performance benefits
/// Lookup table for all squares attacked by a king on a given square.
static KING_ATTACKS: OnceLock<[Bitboard; Square::COUNT]> = OnceLock::new();

fn attacks_from_kings(square: Square) -> Bitboard {
    let lookup = KING_ATTACKS.get_or_init(|| {
        let mut attacks = [Bitboard::EMPTY; Square::COUNT];

        for square in Square::ALL {
            let mut sq_attacks = Bitboard::EMPTY;

            if let Ok(to) = square.up(1) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.down(1) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.left(1) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.right(1) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.up(1).and_then(|sq| sq.left(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.up(1).and_then(|sq| sq.right(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.down(1).and_then(|sq| sq.left(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.down(1).and_then(|sq| sq.right(1)) {
                sq_attacks |= to;
            }

            attacks[usize::from(square)] = sq_attacks;
        }
        attacks
    });
    lookup[usize::from(square)]
}

static KNIGHT_ATTACKS: OnceLock<[Bitboard; Square::COUNT]> = OnceLock::new();

fn attacks_from_knight(square: Square) -> Bitboard {
    let lookup = KNIGHT_ATTACKS.get_or_init(|| {
        let mut attacks = [Bitboard::EMPTY; Square::COUNT];

        for square in Square::ALL {
            let mut sq_attacks = Bitboard::EMPTY;

            if let Ok(to) = square.up(2).and_then(|sq| sq.left(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.up(2).and_then(|sq| sq.right(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.down(2).and_then(|sq| sq.left(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.down(2).and_then(|sq| sq.right(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.left(2).and_then(|sq| sq.up(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.left(2).and_then(|sq| sq.down(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.right(2).and_then(|sq| sq.up(1)) {
                sq_attacks |= to;
            }

            if let Ok(to) = square.right(2).and_then(|sq| sq.down(1)) {
                sq_attacks |= to;
            }

            attacks[usize::from(square)] = sq_attacks;
        }
        attacks
    });
    lookup[usize::from(square)]
}

/// Returns a bitboard with all squares attacked by a piece on a given square.
pub fn attacks_from<const PIECE_TYPE_VALUE: u8>(occupied: Bitboard, from_sq: Square) -> Bitboard {
    match PIECE_TYPE_VALUE {
        PieceType::KING_VALUE => attacks_from_kings(from_sq),
        PieceType::KNIGHT_VALUE => attacks_from_knight(from_sq),
        PieceType::ROOK_VALUE => pext_sliders::attacks_from_rook(occupied, from_sq),
        PieceType::BISHOP_VALUE => pext_sliders::attacks_from_bishop(occupied, from_sq),
        PieceType::QUEEN_VALUE => {
            pext_sliders::attacks_from_rook(occupied, from_sq) | pext_sliders::attacks_from_bishop(occupied, from_sq)
        }
        _ => unimplemented!("Piece type not implemented"),
    }
}

pub fn attacks_to<const COLOR_VALUE: u8>(position: &Position, sq: Square) -> Bitboard {
    let color = Color::from(COLOR_VALUE);
    let queens = position.bb_piece(Piece::new(color, PieceType::Queen));
    let rooks = position.bb_piece(Piece::new(color, PieceType::Rook));
    let bishops = position.bb_piece(Piece::new(color, PieceType::Bishop));
    let knights = position.bb_piece(Piece::new(color, PieceType::Knight));
    let kings = position.bb_piece(Piece::new(color, PieceType::King));
    let pawns = position.bb_piece(Piece::new(color, PieceType::Pawn));
    let occupied = position.bb_occupied();

    let mut attacks = attacks_from::<{ PieceType::ROOK_VALUE }>(occupied, sq) & (queens | rooks);
    attacks |= attacks_from::<{ PieceType::BISHOP_VALUE }>(occupied, sq) & (queens | bishops);
    attacks |= attacks_from::<{ PieceType::KNIGHT_VALUE }>(occupied, sq) & knights;
    attacks |= attacks_from::<{ PieceType::KING_VALUE }>(occupied, sq) & kings;

    let sq_bb = Bitboard::from(sq);
    attacks |= if color == Color::White {
        ((sq_bb & !Bitboard::from(File::A)) >> 9) | ((sq_bb & !Bitboard::from(File::H)) >> 7)
    } else {
        ((sq_bb & !Bitboard::from(File::A)) << 7) | ((sq_bb & !Bitboard::from(File::H)) << 9)
    } & pawns;

    attacks
}
