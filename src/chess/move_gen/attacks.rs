use std::sync::OnceLock;

use crate::chess::{
    bitboard::Bitboard,
    coordinates::{File, Square},
    piece::{Color, PieceType},
};

mod naive_sliders {
    use crate::chess::{bitboard::Bitboard, coordinates::Square};

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

    use crate::chess::{
        bitboard::Bitboard,
        coordinates::{File, Rank, Square},
    };

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

static ROOK_ATTACKS: OnceLock<[Bitboard; Square::COUNT]> = OnceLock::new();

/// Returns a bitboard representing all squares attacked by a rook on a given square on an empty board.
///
/// # Parameters
/// * `square` - The square from which to calculate rook attacks
///
/// # Returns
/// A bitboard representing all squares that would be attacked by a rook on the given square on an empty board.
pub fn attacks_from_rooks(square: Square) -> Bitboard {
    let lookup = ROOK_ATTACKS.get_or_init(|| {
        std::array::from_fn(|index| {
            attacks_from::<{ PieceType::ROOK_VALUE }>(Bitboard::EMPTY, Square::from(index as u8))
        })
    });
    lookup[usize::from(square)]
}

static BISHOP_ATTACKS: OnceLock<[Bitboard; Square::COUNT]> = OnceLock::new();

/// Returns a bitboard representing all squares attacked by a bishop on a given square on an empty board.
///
/// # Parameters
/// * `square` - The square from which to calculate bishop attacks
///
/// # Returns
/// A bitboard representing all squares that would be attacked by a bishop on the given square on an empty board.
pub fn attacks_from_bishops(square: Square) -> Bitboard {
    let lookup = BISHOP_ATTACKS.get_or_init(|| {
        std::array::from_fn(|index| {
            attacks_from::<{ PieceType::BISHOP_VALUE }>(Bitboard::EMPTY, Square::from(index as u8))
        })
    });
    lookup[usize::from(square)]
}

/// Lookup table for all squares attacked by a king on a given square.
static KING_ATTACKS: OnceLock<[Bitboard; Square::COUNT]> = OnceLock::new();

fn attacks_from_kings(square: Square) -> Bitboard {
    let lookup = KING_ATTACKS.get_or_init(|| {
        let mut attacks = [Bitboard::EMPTY; Square::COUNT];

        let directions = [
            |sq: Square| sq.up(1),
            |sq: Square| sq.down(1),
            |sq: Square| sq.left(1),
            |sq: Square| sq.right(1),
            |sq: Square| sq.up(1).and_then(|sq| sq.left(1)),
            |sq: Square| sq.up(1).and_then(|sq| sq.right(1)),
            |sq: Square| sq.down(1).and_then(|sq| sq.left(1)),
            |sq: Square| sq.down(1).and_then(|sq| sq.right(1)),
        ];

        for square in Square::ALL {
            let mut sq_attacks = Bitboard::EMPTY;

            for direction in directions.iter() {
                if let Ok(to) = direction(square) {
                    sq_attacks |= to;
                }
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

        let directions = [
            |sq: Square| sq.up(2).and_then(|sq| sq.left(1)),
            |sq: Square| sq.up(2).and_then(|sq| sq.right(1)),
            |sq: Square| sq.down(2).and_then(|sq| sq.left(1)),
            |sq: Square| sq.down(2).and_then(|sq| sq.right(1)),
            |sq: Square| sq.left(2).and_then(|sq| sq.up(1)),
            |sq: Square| sq.left(2).and_then(|sq| sq.down(1)),
            |sq: Square| sq.right(2).and_then(|sq| sq.up(1)),
            |sq: Square| sq.right(2).and_then(|sq| sq.down(1)),
        ];

        for square in Square::ALL {
            let mut sq_attacks = Bitboard::EMPTY;

            for direction in directions.iter() {
                if let Ok(to) = direction(square) {
                    sq_attacks |= to;
                }
            }

            attacks[usize::from(square)] = sq_attacks;
        }
        attacks
    });
    lookup[usize::from(square)]
}

/// Calculates the squares attacked by a pawn of the given color from the specified square.
///
/// This function determines which squares would be attacked if a pawn of the given color were placed on the specified
/// square. For white pawns, attacks are diagonal captures going northeast and northwest. For black pawns, attacks are
/// diagonal captures going southeast and southwest.
///
/// # Parameters
/// * `color` - The color of the pawn (White or Black)
/// * `sq` - The square from which to calculate attacks
///
/// # Returns
/// A bitboard representing all squares that would be attacked by a pawn of the specified color if it were on the given
/// square.
pub fn attacks_from_pawns(color: Color, sq: Square) -> Bitboard {
    let sq_bb = Bitboard::from(sq);
    if color == Color::White {
        ((sq_bb & !Bitboard::from(File::A)) >> 9) | ((sq_bb & !Bitboard::from(File::H)) >> 7)
    } else {
        ((sq_bb & !Bitboard::from(File::A)) << 7) | ((sq_bb & !Bitboard::from(File::H)) << 9)
    }
}

/// Returns a bitboard with all squares attacked by a specific piece type from a given square.
///
/// This function calculates all squares that would be attacked by a piece of the specified type if it were placed on
/// the given square, considering the current board occupation.
///
/// # Type Parameters
/// * `PIECE_TYPE_VALUE` - A compile-time constant representing the piece type (must be one of the values defined in
///   PieceType: KING_VALUE, KNIGHT_VALUE, ROOK_VALUE, BISHOP_VALUE, or QUEEN_VALUE)
///
/// # Parameters
/// * `occupied` - A bitboard representing all occupied squares on the board
/// * `sq` - The square from which to calculate attacks
///
/// # Returns
/// A bitboard representing all squares that are attacked by the specified piece type from the given square, taking into
/// account the current board occupation.
///
/// # Panics
/// Panics if an unsupported piece type value is provided (such as pawns, which have special attack patterns that depend
/// on color).
///
/// # Note
/// For sliding pieces (rook, bishop, queen), the occupied squares are considered to block the attack rays.
pub fn attacks_from<const PIECE_TYPE_VALUE: u8>(occupied: Bitboard, sq: Square) -> Bitboard {
    match PIECE_TYPE_VALUE {
        PieceType::KING_VALUE => attacks_from_kings(sq),
        PieceType::KNIGHT_VALUE => attacks_from_knight(sq),
        PieceType::ROOK_VALUE => pext_sliders::attacks_from_rook(occupied, sq),
        PieceType::BISHOP_VALUE => pext_sliders::attacks_from_bishop(occupied, sq),
        PieceType::QUEEN_VALUE => {
            pext_sliders::attacks_from_rook(occupied, sq) | pext_sliders::attacks_from_bishop(occupied, sq)
        }
        _ => unimplemented!("Piece type not implemented"),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_attacks_from_rooks() {
        let expected = Square::E2
            | Square::E3
            | Square::E4
            | Square::E5
            | Square::E6
            | Square::E7
            | Square::E8
            | Square::A1
            | Square::B1
            | Square::C1
            | Square::D1
            | Square::F1
            | Square::G1
            | Square::H1;
        let attacks = attacks_from_rooks(Square::E1);
        assert_eq!(expected, attacks);
    }

    #[test]
    fn test_attacks_from_bishop() {
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
        let attacks = attacks_from_bishops(Square::E4);
        assert_eq!(expected, attacks);
    }
}
