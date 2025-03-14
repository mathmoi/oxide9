use std::{
    arch::x86_64::_pdep_u64,
    fmt::{Debug, Formatter},
    sync::OnceLock,
};

use super::{
    coordinates::{Antidiagonal, CoordinatesResult, Diagonal},
    File, Rank, Square,
};

/// A bitboard is a 64-bit integer that represents the state of a chess board. Each bit represents
/// a square on the board.
#[derive(Clone, Copy, PartialEq, Eq)]
pub struct Bitboard(u64);

static BITBOARD_BETWEEN: OnceLock<[Bitboard; Square::COUNT * Square::COUNT]> = OnceLock::new();

impl Bitboard {
    /// Creates a new bitboard with the given value.
    pub fn new(value: u64) -> Self {
        Bitboard(value)
    }

    /// Represents an empty bitboard.
    pub const EMPTY: Bitboard = Bitboard(0);

    /// Represents a filled bitboard.
    pub const ALL: Bitboard = Bitboard(u64::MAX);

    /// Returns the value of a single square on the bitboard.
    pub fn get(self, square: Square) -> bool {
        self.0 & (1u64 << u8::from(square)) != 0
    }

    /// Returns the least significant bit (LSB) of the bitboard as a `Square`.
    ///
    /// This function finds the position of the first set bit (1) when scanning from the least significant bit (A1 in
    /// chess notation) to the most significant bit (H8). It utilizes the `trailing_zeros` method to efficiently
    /// identify this position.
    ///
    /// # Returns
    ///
    /// - `Some(Square)` containing the position of the least significant set bit
    /// - `None` if the bitboard is empty (has no bits set)
    ///
    /// # Examples
    ///
    /// ```
    /// use oxide9::chess::{Bitboard, Square};
    ///
    /// let board = Bitboard::new(0b10100);
    /// assert_eq!(board.lsb(), Some(Square::from(2))); // Third bit from right is set
    ///
    /// assert_eq!(Bitboard::EMPTY.lsb(), None);
    /// ```
    pub fn lsb(self) -> Option<Square> {
        if self.0 == 0 {
            return None;
        }

        Some((self.0.trailing_zeros() as u8).into())
    }

    /// Returns the most significant bit (MSB) of the bitboard as a `Square`.
    ///
    /// This function finds the position of the highest set bit (1) when scanning from the most significant bit (H8 in
    /// chess notation) to the least significant bit (A1). It uses the `leading_zeros` method to efficiently identify
    /// this position.
    ///
    /// # Returns
    ///
    /// - `Some(Square)` containing the position of the most significant set bit
    /// - `None` if the bitboard is empty (has no bits set)
    ///
    /// # Examples
    ///
    /// ```
    /// use oxide9::chess::{Bitboard, Square};
    ///
    /// let board = Bitboard::new(0b10100);
    /// assert_eq!(board.msb(), Some(Square::from(4))); // Fifth bit from right is set
    ///
    /// assert_eq!(Bitboard::EMPTY.msb(), None);
    /// ```
    pub fn msb(self) -> Option<Square> {
        if self.0 == 0 {
            return None;
        }

        Some((63 - self.0.leading_zeros() as u8).into())
    }

    /// Counts the number of set bits (1s) in the bitboard.
    ///
    /// This function uses Rust's native `count_ones` method on the underlying `u64` value to calculate the population
    /// count (also known as Hamming weight).
    ///
    /// # Returns
    ///
    /// A `u32` representing the number of bits set to 1 in the bitboard.
    ///
    /// # Examples
    ///
    /// ```
    /// use oxide9::chess::{Bitboard, Square};
    ///
    /// let board = Bitboard::new(0b10110101);
    /// assert_eq!(board.popcnt(), 5); // There are 5 bits set to 1
    ///
    /// assert_eq!(Bitboard::EMPTY.popcnt(), 0);
    ///
    /// let full_square = Bitboard::new(0xFF); // 8 bits set
    /// assert_eq!(full_square.popcnt(), 8);
    /// ```
    pub fn popcnt(self) -> u32 {
        self.0.count_ones()
    }

    /// Performs parallel bits deposit (PDEP) using this bitboard as a mask.
    ///
    /// This function uses the BMI2 instruction set's PDEP operation, which takes bits from `bits` in contiguous form
    /// and deposits them into positions specified by set bits (1s) in `self`, with all other bits in the result set to
    /// 0.
    ///
    /// # Arguments
    ///
    /// * `bits` - A `u64` containing the source bits to be deposited
    ///
    /// # Returns
    ///
    /// A `Bitboard` with bits from `bits` distributed according to the pattern in `self`.
    ///
    /// # Safety
    ///
    /// This function uses an unsafe intrinsic that requires the CPU to support the BMI2 instruction set. Ensure your
    /// target architecture supports BMI2 or that you've implemented runtime feature detection before calling this
    /// function.
    ///
    /// # Examples
    ///
    /// ```
    /// use oxide9::chess::Bitboard;
    ///
    /// let mask = Bitboard::new(0b11101100);
    /// let bits = 0b1011;
    /// let result = mask.pdep(bits); // Returns Bitboard(0b10101100)
    /// ```
    pub fn pdep(self, bits: u64) -> Bitboard {
        Bitboard(unsafe { _pdep_u64(bits, self.0) })
    }

    /// Performs parallel bits extraction (PEXT) on this bitboard using the given mask.
    ///
    /// This function uses the BMI2 instruction set's PEXT operation, which extracts bits from `self` at positions where
    /// corresponding bits in `mask` are set to 1, and compacts them into a contiguous sequence of bits at the least
    /// significant positions in the result.
    ///
    /// # Arguments
    ///
    /// * `mask` - A `Bitboard` whose set bits (1s) indicate which bits to extract from `self`
    ///
    /// # Returns
    ///
    /// A `u64` containing the extracted bits in contiguous form.
    ///
    /// # Safety
    ///
    /// This function uses an unsafe intrinsic that requires the CPU to support the BMI2 instruction set. Ensure your
    /// target architecture supports BMI2 or that you've implemented runtime feature detection before calling this
    /// function.
    ///
    /// # Examples
    ///
    /// ```
    /// use oxide9::chess::Bitboard;
    ///
    /// let board = Bitboard::new(0b10110101);
    /// let mask = Bitboard::new(0b11101100);
    /// let result = board.pext(mask); // Returns 0b1011
    /// ```
    pub fn pext(self, mask: Bitboard) -> u64 {
        unsafe { std::arch::x86_64::_pext_u64(self.0, mask.0) }
    }

    /// Checks if the bitboard has no bits set (is completely empty).
    ///
    /// # Returns
    /// `true` if the bitboard has no bits set, `false` otherwise.
    pub fn is_empty(self) -> bool {
        self.0 == 0
    }

    /// Determines if the bitboard has more than one bit set to 1.
    ///
    /// This function efficiently checks whether a bitboard contains at least two set bits, without needing to count all
    /// bits.
    ///
    /// # Returns
    /// `true` if the bitboard has two or more bits set to 1, `false` if it has zero or one bit set.
    ///
    /// # Note
    /// Uses a bit manipulation trick: for any number n, (n & (n-1)) clears the least significant set bit. If the result
    /// is non-zero, there must be at least one more set bit.
    pub fn has_more_than_one(self) -> bool {
        (self.0 & (self.0 - 1)) != 0
    }

    /// Returns a bitboard with all squares between two squares, including the to square.
    ///
    /// This function accesses a pre-computed lookup table to efficiently retrieve a bitboard containing all squares on
    /// the path between `from` and `to` (excluding from and including to). The function works for orthogonal
    /// (rank/file) and diagonal paths. If the squares are not on the same rank, file, or diagonal, the function returns
    /// a bitboard with only the to square set.
    pub fn between(from: Square, to: Square) -> Bitboard {
        let lookup = BITBOARD_BETWEEN.get_or_init(|| {
            let directions: [Box<dyn Fn(Square) -> CoordinatesResult<Square>>; 8] = [
                Box::new(|square| square.right(1)),
                Box::new(|square| square.left(1)),
                Box::new(|square| square.up(1)),
                Box::new(|square| square.down(1)),
                Box::new(|square| square.right(1).and_then(|square| square.up(1))),
                Box::new(|square| square.right(1).and_then(|square| square.down(1))),
                Box::new(|square| square.left(1).and_then(|square| square.up(1))),
                Box::new(|square| square.left(1).and_then(|square| square.down(1))),
            ];

            let mut between = [Bitboard::EMPTY; Square::COUNT * Square::COUNT];
            for from in Square::ALL {
                for direction in directions.iter() {
                    let mut bb = Bitboard::EMPTY;
                    let mut next = direction(from);
                    while let Ok(to) = next {
                        bb |= to;
                        between[usize::from(from) * Square::COUNT + usize::from(to)] = bb;
                        next = direction(to);
                    }
                }
            }
            between
        });

        lookup[usize::from(from) * Square::COUNT + usize::from(to)]
    }
}

impl Debug for Bitboard {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for rank in Rank::ALL.iter().rev() {
            for file in File::ALL {
                let square = Square::new(file, *rank);
                if self.get(square) {
                    write!(f, "1")?;
                } else {
                    write!(f, "0")?;
                }
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

//======================================================================================================================
// Conversions to bitboards
//======================================================================================================================

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

impl From<Diagonal> for Bitboard {
    fn from(value: Diagonal) -> Self {
        const LOOKUP: [Bitboard; Diagonal::COUNT] = [
            Bitboard(0x0100000000000000),
            Bitboard(0x0201000000000000),
            Bitboard(0x0402010000000000),
            Bitboard(0x0804020100000000),
            Bitboard(0x1008040201000000),
            Bitboard(0x2010080402010000),
            Bitboard(0x4020100804020100),
            Bitboard(0x8040201008040201),
            Bitboard(0x0080402010080402),
            Bitboard(0x0000804020100804),
            Bitboard(0x0000008040201008),
            Bitboard(0x0000000080402010),
            Bitboard(0x0000000000804020),
            Bitboard(0x0000000000008040),
            Bitboard(0x0000000000000080),
        ];
        LOOKUP[usize::from(value)]
    }
}

impl From<Antidiagonal> for Bitboard {
    fn from(value: Antidiagonal) -> Self {
        const LOOKUP: [Bitboard; Antidiagonal::COUNT] = [
            Bitboard(0x0000000000000001),
            Bitboard(0x0000000000000102),
            Bitboard(0x0000000000010204),
            Bitboard(0x0000000001020408),
            Bitboard(0x0000000102040810),
            Bitboard(0x0000010204081020),
            Bitboard(0x0001020408102040),
            Bitboard(0x0102040810204080),
            Bitboard(0x0204081020408000),
            Bitboard(0x0408102040800000),
            Bitboard(0x0810204080000000),
            Bitboard(0x1020408000000000),
            Bitboard(0x2040800000000000),
            Bitboard(0x4080000000000000),
            Bitboard(0x8000000000000000),
        ];
        LOOKUP[usize::from(value)]
    }
}

//======================================================================================================================
// Bitwise operations for bitboards
//======================================================================================================================

impl std::ops::BitAnd for Bitboard {
    type Output = Self;
    fn bitand(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 & rhs.0)
    }
}

impl std::ops::BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl std::ops::BitOr for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 | rhs.0)
    }
}

impl std::ops::BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl std::ops::BitXor for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: Self) -> Self::Output {
        Bitboard(self.0 ^ rhs.0)
    }
}

impl std::ops::BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl std::ops::Not for Bitboard {
    type Output = Self;
    fn not(self) -> Self::Output {
        Bitboard(!self.0)
    }
}

impl std::ops::Shl<u32> for Bitboard {
    type Output = Self;
    fn shl(self, rhs: u32) -> Self::Output {
        Bitboard(self.0 << rhs)
    }
}

impl std::ops::ShlAssign<u32> for Bitboard {
    fn shl_assign(&mut self, rhs: u32) {
        self.0 <<= rhs;
    }
}

impl std::ops::Shr<u32> for Bitboard {
    type Output = Self;
    fn shr(self, rhs: u32) -> Self::Output {
        Bitboard(self.0 >> rhs)
    }
}

impl std::ops::ShrAssign<u32> for Bitboard {
    fn shr_assign(&mut self, rhs: u32) {
        self.0 >>= rhs;
    }
}

//======================================================================================================================
// Bitwise operations between bitboards and squares
//======================================================================================================================

impl std::ops::BitOr<Square> for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: Square) -> Self::Output {
        self | Bitboard::from(rhs)
    }
}

impl std::ops::BitOr<Bitboard> for Square {
    type Output = Bitboard;
    fn bitor(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) | rhs
    }
}

impl std::ops::BitOrAssign<Square> for Bitboard {
    fn bitor_assign(&mut self, rhs: Square) {
        *self |= Bitboard::from(rhs);
    }
}

impl std::ops::BitXor<Square> for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: Square) -> Self::Output {
        self ^ Bitboard::from(rhs)
    }
}

impl std::ops::BitXor<Bitboard> for Square {
    type Output = Bitboard;
    fn bitxor(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) ^ rhs
    }
}

impl std::ops::BitXorAssign<Square> for Bitboard {
    fn bitxor_assign(&mut self, rhs: Square) {
        *self ^= Bitboard::from(rhs);
    }
}

impl std::ops::BitAnd<Square> for Bitboard {
    type Output = Bitboard;
    fn bitand(self, rhs: Square) -> Self::Output {
        self & Bitboard::from(rhs)
    }
}

impl std::ops::BitAnd<Bitboard> for Square {
    type Output = Bitboard;
    fn bitand(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) & rhs
    }
}

impl std::ops::BitAndAssign<Square> for Bitboard {
    fn bitand_assign(&mut self, rhs: Square) {
        *self &= Bitboard::from(rhs);
    }
}

impl std::ops::BitOr<Square> for Square {
    type Output = Bitboard;
    fn bitor(self, rhs: Square) -> Self::Output {
        Bitboard::from(self) | Bitboard::from(rhs)
    }
}

//======================================================================================================================
// Bitwise operations between bitboards and files
//======================================================================================================================

impl std::ops::BitOr<File> for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: File) -> Self::Output {
        self | Bitboard::from(rhs)
    }
}

impl std::ops::BitOr<Bitboard> for File {
    type Output = Bitboard;
    fn bitor(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) | rhs
    }
}

impl std::ops::BitOrAssign<File> for Bitboard {
    fn bitor_assign(&mut self, rhs: File) {
        *self |= Bitboard::from(rhs);
    }
}

impl std::ops::BitXor<File> for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: File) -> Self::Output {
        self ^ Bitboard::from(rhs)
    }
}

impl std::ops::BitXor<Bitboard> for File {
    type Output = Bitboard;
    fn bitxor(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) ^ rhs
    }
}

impl std::ops::BitXorAssign<File> for Bitboard {
    fn bitxor_assign(&mut self, rhs: File) {
        *self ^= Bitboard::from(rhs);
    }
}

impl std::ops::BitAnd<File> for Bitboard {
    type Output = Bitboard;
    fn bitand(self, rhs: File) -> Self::Output {
        self & Bitboard::from(rhs)
    }
}

impl std::ops::BitAnd<Bitboard> for File {
    type Output = Bitboard;
    fn bitand(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) & rhs
    }
}

impl std::ops::BitAndAssign<File> for Bitboard {
    fn bitand_assign(&mut self, rhs: File) {
        *self &= Bitboard::from(rhs);
    }
}

impl std::ops::BitOr<File> for File {
    type Output = Bitboard;
    fn bitor(self, rhs: File) -> Self::Output {
        Bitboard::from(self) | Bitboard::from(rhs)
    }
}

//======================================================================================================================
// Bitwise operations between bitboards and ranks
//======================================================================================================================

impl std::ops::BitOr<Rank> for Bitboard {
    type Output = Self;
    fn bitor(self, rhs: Rank) -> Self::Output {
        self | Bitboard::from(rhs)
    }
}

impl std::ops::BitOr<Bitboard> for Rank {
    type Output = Bitboard;
    fn bitor(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) | rhs
    }
}

impl std::ops::BitOrAssign<Rank> for Bitboard {
    fn bitor_assign(&mut self, rhs: Rank) {
        *self |= Bitboard::from(rhs);
    }
}

impl std::ops::BitXor<Rank> for Bitboard {
    type Output = Self;
    fn bitxor(self, rhs: Rank) -> Self::Output {
        self ^ Bitboard::from(rhs)
    }
}

impl std::ops::BitXor<Bitboard> for Rank {
    type Output = Bitboard;
    fn bitxor(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) ^ rhs
    }
}

impl std::ops::BitXorAssign<Rank> for Bitboard {
    fn bitxor_assign(&mut self, rhs: Rank) {
        *self ^= Bitboard::from(rhs);
    }
}

impl std::ops::BitAnd<Rank> for Bitboard {
    type Output = Bitboard;
    fn bitand(self, rhs: Rank) -> Self::Output {
        self & Bitboard::from(rhs)
    }
}

impl std::ops::BitAnd<Bitboard> for Rank {
    type Output = Bitboard;
    fn bitand(self, rhs: Bitboard) -> Self::Output {
        Bitboard::from(self) & rhs
    }
}

impl std::ops::BitAndAssign<Rank> for Bitboard {
    fn bitand_assign(&mut self, rhs: Rank) {
        *self &= Bitboard::from(rhs);
    }
}

impl std::ops::BitOr<Rank> for Rank {
    type Output = Bitboard;
    fn bitor(self, rhs: Rank) -> Self::Output {
        Bitboard::from(self) | Bitboard::from(rhs)
    }
}

//======================================================================================================================
// Iteration over the set bits in a bitboard
//======================================================================================================================

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

impl IntoIterator for Bitboard {
    type Item = Square;
    type IntoIter = BitboardIterator;

    fn into_iter(self) -> Self::IntoIter {
        BitboardIterator(self.0)
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
        assert_eq!(Bitboard::between(Square::A1, Square::A2), Bitboard::from(Square::A2));
        assert_eq!(Bitboard::between(Square::A1, Square::C1), Square::B1 | Square::C1);
        assert_eq!(
            Bitboard::between(Square::H7, Square::B7),
            Square::B7 | Square::C7 | Square::D7 | Square::E7 | Square::F7 | Square::G7
        );
        assert_eq!(Bitboard::between(Square::D5, Square::D2), Square::D2 | Square::D3 | Square::D4);
        assert_eq!(Bitboard::between(Square::D5, Square::H1), Square::E4 | Square::F3 | Square::G2 | Square::H1);
        assert_eq!(
            Bitboard::between(Square::A1, Square::H8),
            Square::B2 | Square::C3 | Square::D4 | Square::E5 | Square::F6 | Square::G7 | Square::H8
        );
        assert_eq!(
            Bitboard::between(Square::F7, Square::A2),
            Square::E6 | Square::D5 | Square::C4 | Square::B3 | Square::A2
        );
    }

    #[test]
    fn test_bitwise_between_rank_and_bitboard() {
        assert_eq!(Bitboard(0x00ff00000000ff00), Rank::R2 | Rank::R7);
        assert_eq!(Bitboard(0x00000000000000ff), Bitboard::EMPTY | Rank::R1);
        assert_eq!(Bitboard(0x4040404fb040404), Bitboard(0x404040404040404) ^ Rank::R4);
    }

    #[test]
    fn test_bitwise_between_file_and_bitboard() {
        assert_eq!(Bitboard(0x8181818181818181), File::A | File::H);
        assert_eq!(Bitboard(0x101010101010101), Bitboard::EMPTY | File::A);
        assert_eq!(Bitboard(0x7f7f7f7f7f7f7f7f), Bitboard::ALL ^ File::H);
    }
}
