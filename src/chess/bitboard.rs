use super::{coordinates::CoordinatesResult, File, Rank, Square};
use once_cell::sync::Lazy;

/// A bitboard is a 64-bit integer that represents the state of a chess board. Each bit represents
/// a square on the board.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Bitboard(u64);

impl Bitboard {
    /// Represents an empty bitboard.
    pub const EMPTY: Bitboard = Bitboard(0);

    /// Represents a filled bitboard.
    pub const ALL: Bitboard = Bitboard(u64::MAX);

    /// Returns the value of a single square on the bitboard.
    pub fn get(self, square: Square) -> bool {
        self.0 & (1u64 << u8::from(square)) != 0
    }

    /// Returns the least significant bit of the bitboard.
    pub fn lsb(self) -> Option<Square> {
        if self.0 == 0 {
            return None;
        }

        Some((self.0.trailing_zeros() as u8).into())
    }

    /// Returns the most significant bit of the bitboard.
    pub fn msb(self) -> Option<Square> {
        if self.0 == 0 {
            return None;
        }

        Some((63 - self.0.leading_zeros() as u8).into())
    }

    /// Returns the number of set bits in the bitboard.
    pub fn popcnt(self) -> u32 {
        self.0.count_ones()
    }

    /// Returns whether the bitboard is empty.
    pub fn is_empty(self) -> bool {
        self == Bitboard::EMPTY
    }

    const BETWEEN: Lazy<[Bitboard; Square::COUNT * Square::COUNT]> = Lazy::new(|| {
        let directions: [Box<dyn Fn(Square) -> CoordinatesResult<Square>>; 4] = [
            Box::new(|square| square.right(1)),
            Box::new(|square| square.up(1)),
            Box::new(|square| square.right(1).and_then(|square| square.up(1))),
            Box::new(|square| square.right(1).and_then(|square| square.down(1))),
        ];

        let mut between = [Bitboard::EMPTY; Square::COUNT * Square::COUNT];
        for from in Square::ALL {
            for direction in directions.iter() {
                let mut bb = Bitboard::EMPTY;
                let mut next = direction(from);
                while let Ok(to) = next {
                    between[usize::from(from) * Square::COUNT + usize::from(to)] = bb;
                    between[usize::from(to) * Square::COUNT + usize::from(from)] = bb;
                    bb |= to;
                    next = direction(to);
                }
            }
        }
        between
    });

    /// Returns a bitboard with all squares between two squares.
    pub fn between(from: Square, to: Square) -> Bitboard {
        Self::BETWEEN[usize::from(from) * Square::COUNT + usize::from(to)]
    }
}

impl From<Square> for Bitboard {
    fn from(square: Square) -> Self {
        // TODO : Check if it would be more efficient to have a lookup table
        Bitboard(1u64 << u8::from(square))
    }
}

impl From<File> for Bitboard {
    fn from(file: File) -> Self {
        // TODO : Check if it would be more efficient to have a lookup table
        Bitboard(0x0101010101010101 << u8::from(file))
    }
}

impl From<Rank> for Bitboard {
    fn from(rank: Rank) -> Self {
        // TODO : Check if it would be more efficient to have a lookup table
        Bitboard(0xff << (8 * u8::from(rank)))
    }
}

impl std::ops::BitAnd for Bitboard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 & rhs.0)
    }
}

impl std::ops::BitOr for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 | rhs.0)
    }
}

impl std::ops::BitOr<Square> for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: Square) -> Self::Output {
        self | Bitboard::from(rhs)
    }
}

impl std::ops::BitOr<Square> for Square {
    type Output = Bitboard;
    fn bitor(self, rhs: Square) -> Self::Output {
        Bitboard::from(self) | Bitboard::from(rhs)
    }
}

impl std::ops::BitXor for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 ^ rhs.0)
    }
}

impl std::ops::BitXor<Square> for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: Square) -> Self::Output {
        self ^ Bitboard::from(rhs)
    }
}

impl std::ops::Not for Bitboard {
    type Output = Self;
    fn not(self) -> Self::Output {
        Bitboard(!self.0)
    }
}

impl std::ops::BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl std::ops::BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl std::ops::BitOrAssign<Square> for Bitboard {
    fn bitor_assign(&mut self, rhs: Square) {
        *self |= Bitboard::from(rhs);
    }
}

impl std::ops::BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl std::ops::BitXorAssign<Square> for Bitboard {
    fn bitxor_assign(&mut self, rhs: Square) {
        *self ^= Bitboard::from(rhs);
    }
}

impl std::ops::Shl<u32> for Bitboard {
    type Output = Self;
    fn shl(self, rhs: u32) -> Self::Output {
        Bitboard(self.0 << rhs)
    }
}

impl std::ops::Shr<u32> for Bitboard {
    type Output = Self;
    fn shr(self, rhs: u32) -> Self::Output {
        Bitboard(self.0 >> rhs)
    }
}

impl std::ops::ShlAssign<u32> for Bitboard {
    fn shl_assign(&mut self, rhs: u32) {
        self.0 <<= rhs;
    }
}

impl std::ops::ShrAssign<u32> for Bitboard {
    fn shr_assign(&mut self, rhs: u32) {
        self.0 >>= rhs;
    }
}

impl IntoIterator for Bitboard {
    type Item = Square;
    type IntoIter = BitboardIterator;

    fn into_iter(self) -> Self::IntoIter {
        BitboardIterator(self.0)
    }
}

/// An iterator over the set bits in a bitboard.
pub struct BitboardIterator(u64);

impl Iterator for BitboardIterator {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        if self.0 == 0 {
            return None;
        }

        let square: Square = (self.0.trailing_zeros() as u8).into();
        self.0 &= self.0 - 1;
        Some(square)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bitboard_from_square() {
        assert_eq!(Bitboard(0x0000000000000001), Square::A1.into());
        assert_eq!(Bitboard(0x0000000000000100), Square::A2.into());
        assert_eq!(Bitboard(0x0000000000000080), Square::H1.into());
        assert_eq!(Bitboard(0x0100000000000000), Square::A8.into());
        assert_eq!(Bitboard(0x8000000000000000), Square::H8.into());
    }

    #[test]
    fn test_bitboard_get() {
        let bb_e2 = Bitboard::from(Square::E2);
        for square in Square::ALL {
            assert!(Bitboard::ALL.get(square));
            assert!(!Bitboard::EMPTY.get(square));
            assert_eq!(bb_e2.get(square), square == Square::E2);
        }
    }

    #[test]
    fn test_bitor_and_bitorassign_to_set_bit() {
        let mut bb = Bitboard::EMPTY;
        bb = bb | Square::E2;
        assert_eq!(bb, Bitboard(0x0000000000001000));

        bb |= Square::E4;
        assert_eq!(bb, Bitboard(0x0000000010001000));
    }

    #[test]
    fn test_bitor_squares_to_create_bitboard() {
        let bb = Square::A1 | Square::H1 | Square::A8 | Square::H8;
        assert_eq!(bb, Bitboard(0x8100000000000081));
    }

    #[test]
    fn test_bitxor_and_bitxorassign_to_toggle_bit() {
        let mut bb = Bitboard::ALL;
        bb = bb ^ Square::E2;
        assert_eq!(bb, Bitboard(0xffffffffffffefff));

        bb ^= Square::E2;
        assert_eq!(bb, Bitboard(0xffffffffffffffff));
    }

    #[test]
    fn test_bitboard_iterator() {
        let bb = Bitboard(0x8100000000000081);
        let squares: Vec<Square> = bb.into_iter().collect();
        assert_eq!(squares, vec![Square::A1, Square::H1, Square::A8, Square::H8]);
    }

    #[test]
    fn test_bitboard_lsb() {
        assert_eq!(Bitboard(0x0042000000004200).lsb(), Some(Square::B2));
        assert_eq!(Bitboard(0x8100000000000081).lsb(), Some(Square::A1));
        assert_eq!(Bitboard(0xffffffffffffffff).lsb(), Some(Square::A1));
        assert_eq!(Bitboard(0x0000000010000000).lsb(), Some(Square::E4));
        assert_eq!(Bitboard(0x0000000000000000).lsb(), None);
    }

    #[test]
    fn test_bitboard_msb() {
        assert_eq!(Bitboard(0x0042000000004200).msb(), Some(Square::G7));
        assert_eq!(Bitboard(0x8100000000000081).msb(), Some(Square::H8));
        assert_eq!(Bitboard(0xffffffffffffffff).msb(), Some(Square::H8));
        assert_eq!(Bitboard(0x0000000010000000).msb(), Some(Square::E4));
        assert_eq!(Bitboard(0x0000000000000000).msb(), None);
    }

    #[test]
    fn test_bitboard_popcnt() {
        assert_eq!(Bitboard(0x0042000000004200).popcnt(), 4);
        assert_eq!(Bitboard(0x8100000000000081).popcnt(), 4);
        assert_eq!(Bitboard(0xffffffffffffffff).popcnt(), 64);
        assert_eq!(Bitboard(0x0000000010000000).popcnt(), 1);
        assert_eq!(Bitboard(0x0000000000000000).popcnt(), 0);
    }

    #[test]
    fn test_between() {
        assert_eq!(Bitboard::between(Square::A1, Square::A1), Bitboard::EMPTY);
        assert_eq!(Bitboard::between(Square::A1, Square::A2), Bitboard::EMPTY);
        assert_eq!(Bitboard::between(Square::A1, Square::C1), Bitboard::from(Square::B1));
        assert_eq!(
            Bitboard::between(Square::H7, Square::B7),
            Square::C7 | Square::D7 | Square::E7 | Square::F7 | Square::G7
        );
        assert_eq!(Bitboard::between(Square::D5, Square::D2), Square::D3 | Square::D4);
        assert_eq!(Bitboard::between(Square::D5, Square::H1), Square::E4 | Square::F3 | Square::G2);
        assert_eq!(
            Bitboard::between(Square::A1, Square::H8),
            Square::B2 | Square::C3 | Square::D4 | Square::E5 | Square::F6 | Square::G7
        );
        assert_eq!(Bitboard::between(Square::F7, Square::A2), Square::E6 | Square::D5 | Square::C4 | Square::B3);
    }
}
