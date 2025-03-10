use std::convert::From;
use std::fmt::{self, Display};

use super::Color;

/// Error type for the coordinates module.
#[derive(Debug, PartialEq)]
pub enum CoordinatesError {
    MoveOffBoard,
    InvalidCharacter,
    InvalidString,
}

/// Result type for the coordinates module
pub type CoordinatesResult<T> = Result<T, CoordinatesError>;

//======================================================================================================================
// File (as in a file on a chess board)
//======================================================================================================================

/// Represents a file (column) on a chess board.
///
/// Files are labeled from A to H, going from left to right when viewing the board from White's
/// perspective.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq, PartialOrd)]
pub enum File {
    A = 0,
    B = 1,
    C = 2,
    D = 3,
    E = 4,
    F = 5,
    G = 6,
    H = 7,
}

impl File {
    /// Represents all files on a chess board.
    pub const ALL: [File; 8] = [File::A, File::B, File::C, File::D, File::E, File::F, File::G, File::H];

    /// Returns a new file that is moved right by the specified number of files without checking for
    /// bounds.
    ///
    /// # Safety
    ///
    /// Caller must ensure that moving right by `count` steps will result in a valid file. This method
    /// will cause undefined behavior if we move off the board.
    pub unsafe fn right_unchecked(self, count: i8) -> File {
        let min_file_value = u8::from(File::A) as i8;
        let max_file_value = u8::from(File::H) as i8;
        let new_file_value = u8::from(self) as i8 + count;
        debug_assert!(
            min_file_value <= new_file_value && new_file_value <= max_file_value,
            "The new file value must be a valid value for a file"
        );
        File::from((u8::from(self) as i8 + count) as u8)
    }

    /// Moves a file right by the specified number of files.
    /// Returns Ok(File) with the new file if the move is valid, MoveOffBoard if the move would
    /// go off the board.
    pub fn right(self, count: i8) -> CoordinatesResult<File> {
        let min_file_value = u8::from(File::A) as i8;
        let max_file_value = u8::from(File::H) as i8;
        let new_file_value = u8::from(self) as i8 + count;
        if new_file_value < min_file_value || max_file_value < new_file_value {
            return Err(CoordinatesError::MoveOffBoard);
        }
        Ok(unsafe { self.right_unchecked(count) })
    }

    /// Returns a new file that is moved left by the specified number of files without checking for
    /// bounds.
    ///
    /// # Safety
    ///
    /// Caller must ensure that moving left by `count` steps will result in a valid file. This method
    /// will cause undefined behavior if we move off the board.
    pub unsafe fn left_unchecked(self, count: i8) -> File {
        self.right_unchecked(-count)
    }

    /// Moves a file left by the specified number of files.
    /// Returns Ok(File) with the new file if the move is valid, MoveOffBoard if the move would
    /// go off the board.
    pub fn left(self, count: i8) -> CoordinatesResult<File> {
        self.right(-count)
    }
}

impl Display for File {
    /// Formats the file as a single character.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", (u8::from(*self) + b'a') as char)
    }
}

impl From<u8> for File {
    /// Converts a `u8` value to a `File`.
    fn from(value: u8) -> Self {
        assert!(value <= File::H.into());
        unsafe { std::mem::transmute(value) }
    }
}

impl From<File> for u8 {
    /// Converts a `File` to a `u8` value.
    fn from(file: File) -> Self {
        file as u8
    }
}

impl From<File> for char {
    /// Converts a `File` to a `char` value.
    fn from(file: File) -> Self {
        (u8::from(file) + b'a') as char
    }
}

impl TryFrom<char> for File {
    type Error = CoordinatesError;

    /// Converts a `char` value to a `File`.
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            'a' => Ok(File::A),
            'b' => Ok(File::B),
            'c' => Ok(File::C),
            'd' => Ok(File::D),
            'e' => Ok(File::E),
            'f' => Ok(File::F),
            'g' => Ok(File::G),
            'h' => Ok(File::H),
            'A' => Ok(File::A),
            'B' => Ok(File::B),
            'C' => Ok(File::C),
            'D' => Ok(File::D),
            'E' => Ok(File::E),
            'F' => Ok(File::F),
            'G' => Ok(File::G),
            'H' => Ok(File::H),
            _ => Err(CoordinatesError::InvalidCharacter),
        }
    }
}

//======================================================================================================================
// Rank (as in a rank on a chess board)
//======================================================================================================================

/// Represents a rank (row) on a chess board.
///
/// Ranks are labeled from 1 to 8, going from the bottom to the top when viewing the board from
/// White's perspective.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Rank {
    R1 = 0,
    R2 = 1,
    R3 = 2,
    R4 = 3,
    R5 = 4,
    R6 = 5,
    R7 = 6,
    R8 = 7,
}

impl Rank {
    /// Represents all ranks on a chess board.
    pub const ALL: [Rank; 8] = [Rank::R1, Rank::R2, Rank::R3, Rank::R4, Rank::R5, Rank::R6, Rank::R7, Rank::R8];

    /// Returns the rank relative to the specified color. If the color is white, the rank is
    /// returned, if the color is black, the rank is flipped.
    pub fn relative_to_color(self, color: Color) -> Rank {
        match color {
            Color::White => self,
            Color::Black => unsafe { Rank::R8.down_unchecked(u8::from(self) as i8) },
        }
    }

    /// Returns a new rank that is moved up by the specified number of ranks without checking for
    /// bounds.
    ///
    /// # Safety
    ///
    /// Caller must ensure that moving up by `count` steps will result in a valid rank. This method
    /// will cause undefined behavior if we move off the board.
    pub unsafe fn up_unchecked(self, count: i8) -> Rank {
        let min_rank_value = u8::from(Rank::R1) as i8;
        let max_rank_value = u8::from(Rank::R8) as i8;
        let new_rank_value = u8::from(self) as i8 + count;
        debug_assert!(
            min_rank_value <= new_rank_value && new_rank_value <= max_rank_value,
            "The new rank value must be a valid value for a rank"
        );
        Rank::from((u8::from(self) as i8 + count) as u8)
    }

    /// Moves a rank up by the specified number of ranks.
    /// Returns Ok(Rank) with the new rank if the move is valid, MoveOffBoard if the move would
    /// go off the board.
    pub fn up(self, count: i8) -> CoordinatesResult<Rank> {
        let min_rank_value = u8::from(Rank::R1) as i8;
        let max_rank_value = u8::from(Rank::R8) as i8;
        let new_rank_value = u8::from(self) as i8 + count;
        if new_rank_value < min_rank_value || max_rank_value < new_rank_value {
            return Err(CoordinatesError::MoveOffBoard);
        }
        Ok(unsafe { self.up_unchecked(count) })
    }

    /// Returns a new rank that is moved down by the specified number of ranks without checking for
    /// bounds.
    ///
    /// # Safety
    ///
    /// Caller must ensure that moving down by `count` steps will result in a valid rank. This method
    /// will cause undefined behavior if we move off the board.
    pub unsafe fn down_unchecked(self, count: i8) -> Rank {
        self.up_unchecked(-count)
    }

    /// Moves a rank down by the specified number of ranks.
    /// Returns Ok(Rank) with the new rank if the move is valid, MoveOffBoard if the move would
    /// go off the board.
    pub fn down(self, count: i8) -> CoordinatesResult<Rank> {
        self.up(-count)
    }
}

impl Display for Rank {
    /// Formats the rank as a single character.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", (u8::from(*self) + b'1') as char)
    }
}

impl From<u8> for Rank {
    /// Converts a `u8` value to a `Rank`.
    fn from(value: u8) -> Self {
        assert!(value <= Rank::R8.into());
        unsafe { std::mem::transmute(value) }
    }
}

impl From<Rank> for u8 {
    /// Converts a `Rank` to a `u8` value.
    fn from(file: Rank) -> Self {
        file as u8
    }
}

impl TryFrom<char> for Rank {
    type Error = CoordinatesError;

    /// Converts a `char` value to a `Rank`.
    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value {
            '1' => Ok(Rank::R1),
            '2' => Ok(Rank::R2),
            '3' => Ok(Rank::R3),
            '4' => Ok(Rank::R4),
            '5' => Ok(Rank::R5),
            '6' => Ok(Rank::R6),
            '7' => Ok(Rank::R7),
            '8' => Ok(Rank::R8),
            _ => Err(CoordinatesError::InvalidCharacter),
        }
    }
}

//======================================================================================================================
// Diagonals & Antidiagonals
//======================================================================================================================

/// Represents a diagonal on a chess board.
///
/// # Chess Board Diagonals
///
/// Diagonals run from the bottom left to the top right (positive diagonals, e.g. A1 to H8) on a standard 8x8 chess
/// board. There are a total of 15 positive diagonals:
///
/// - The main diagonal runs from A1 to H8 and contains 8 squares
/// - The diagonals parallel to the main diagonal contain fewer squares:
///   - The diagonal from A2 to G8 (or B1 to H7) contains 7 squares
///   - The diagonal from A3 to F8 (or C1 to H6) contains 6 squares
///   - ...and so on
/// - The shortest diagonals at the corners contain only 1 square (A8 and H1)
///
/// Diagonals are numbered 0-14, where:
/// - Diagonal 0 is the single square A8
/// - Diagonal 7 is the main diagonal from A1 to H8
/// - Diagonal 14 is the single square H1
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Diagonal(u8);

impl Diagonal {
    /// Returns the total number of diagonals on a chess board.
    pub const COUNT: usize = 15;
}

impl From<Diagonal> for u8 {
    /// Converts a `Diagonal` to a `u8` value.
    fn from(diagonal: Diagonal) -> Self {
        diagonal.0
    }
}

impl From<Diagonal> for usize {
    /// Converts a `Diagonal` to a `usize` value.
    fn from(diagonal: Diagonal) -> Self {
        diagonal.0 as usize
    }
}

/// Represents an antidiagonal on a chess board.
///
/// # Chess Board Antidiagonals
///
/// Antidiagonals run from the top left to bottom right (negative diagonals, e.g. A8 to H1) on a standard 8x8 chess
/// board. There are a total of 15 antidiagonals:
///
/// - The main antidiagonal runs from A8 to H1 and contains 8 squares
/// - The antidiagonals parallel to the main antidiagonal contain fewer squares:
///   - The antidiagonal from A7 to G1 (or B8 to H2) contains 7 squares
///   - The antidiagonal from A6 to F1 (or C8 to H3) contains 6 squares
///   - ...and so on
/// - The shortest antidiagonals at the corners contain only 1 square (A1 and H8)
///
/// Antidiagonals are numbered 0-14, where:
/// - Antidiagonal 0 is the single square A1
/// - Antidiagonal 7 is the main antidiagonal from A8 to H1
/// - Antidiagonal 14 is the single square H8
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Antidiagonal(u8);

impl Antidiagonal {
    /// Returns the total number of diagonals on a chess board.
    pub const COUNT: usize = 15;
}

impl From<Antidiagonal> for u8 {
    /// Converts an `Antidiagonal` to a `u8` value.
    fn from(antidiagonal: Antidiagonal) -> Self {
        antidiagonal.0
    }
}

impl From<Antidiagonal> for usize {
    /// Converts an `Antidiagonal` to a `usize` value.
    fn from(antidiagonal: Antidiagonal) -> Self {
        antidiagonal.0 as usize
    }
}

//======================================================================================================================
// Square (as in a square on a chess board)
//======================================================================================================================

/// Represents a square on a chess board.
///
/// Squares are indexed from 0 to 63, starting from A1 and ending at H8 with A2 being at index 1.
/// In other words, the file value is stored in the lower 3 bits and the rank value is stored in the
/// next 3 bits. The last two bits are unused and always 0.
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct Square(u8);

#[allow(dead_code)]
impl Square {
    /// The total number of squares on a chess board.
    pub const COUNT: usize = 64;

    // Constants for all squares on the board
    pub const A1: Square = Square(0);
    pub const B1: Square = Square(1);
    pub const C1: Square = Square(2);
    pub const D1: Square = Square(3);
    pub const E1: Square = Square(4);
    pub const F1: Square = Square(5);
    pub const G1: Square = Square(6);
    pub const H1: Square = Square(7);
    pub const A2: Square = Square(8);
    pub const B2: Square = Square(9);
    pub const C2: Square = Square(10);
    pub const D2: Square = Square(11);
    pub const E2: Square = Square(12);
    pub const F2: Square = Square(13);
    pub const G2: Square = Square(14);
    pub const H2: Square = Square(15);
    pub const A3: Square = Square(16);
    pub const B3: Square = Square(17);
    pub const C3: Square = Square(18);
    pub const D3: Square = Square(19);
    pub const E3: Square = Square(20);
    pub const F3: Square = Square(21);
    pub const G3: Square = Square(22);
    pub const H3: Square = Square(23);
    pub const A4: Square = Square(24);
    pub const B4: Square = Square(25);
    pub const C4: Square = Square(26);
    pub const D4: Square = Square(27);
    pub const E4: Square = Square(28);
    pub const F4: Square = Square(29);
    pub const G4: Square = Square(30);
    pub const H4: Square = Square(31);
    pub const A5: Square = Square(32);
    pub const B5: Square = Square(33);
    pub const C5: Square = Square(34);
    pub const D5: Square = Square(35);
    pub const E5: Square = Square(36);
    pub const F5: Square = Square(37);
    pub const G5: Square = Square(38);
    pub const H5: Square = Square(39);
    pub const A6: Square = Square(40);
    pub const B6: Square = Square(41);
    pub const C6: Square = Square(42);
    pub const D6: Square = Square(43);
    pub const E6: Square = Square(44);
    pub const F6: Square = Square(45);
    pub const G6: Square = Square(46);
    pub const H6: Square = Square(47);
    pub const A7: Square = Square(48);
    pub const B7: Square = Square(49);
    pub const C7: Square = Square(50);
    pub const D7: Square = Square(51);
    pub const E7: Square = Square(52);
    pub const F7: Square = Square(53);
    pub const G7: Square = Square(54);
    pub const H7: Square = Square(55);
    pub const A8: Square = Square(56);
    pub const B8: Square = Square(57);
    pub const C8: Square = Square(58);
    pub const D8: Square = Square(59);
    pub const E8: Square = Square(60);
    pub const F8: Square = Square(61);
    pub const G8: Square = Square(62);
    pub const H8: Square = Square(63);

    #[rustfmt::skip]
    pub const ALL: [Square; 64] = [
        Square::A1,Square::B1,Square::C1,Square::D1,Square::E1,Square::F1,Square::G1,Square::H1,
        Square::A2,Square::B2,Square::C2,Square::D2,Square::E2,Square::F2,Square::G2,Square::H2,
        Square::A3,Square::B3,Square::C3,Square::D3,Square::E3,Square::F3,Square::G3,Square::H3,
        Square::A4,Square::B4,Square::C4,Square::D4,Square::E4,Square::F4,Square::G4,Square::H4,
        Square::A5,Square::B5,Square::C5,Square::D5,Square::E5,Square::F5,Square::G5,Square::H5,
        Square::A6,Square::B6,Square::C6,Square::D6,Square::E6,Square::F6,Square::G6,Square::H6,
        Square::A7,Square::B7,Square::C7,Square::D7,Square::E7,Square::F7,Square::G7,Square::H7,
        Square::A8,Square::B8,Square::C8,Square::D8,Square::E8,Square::F8,Square::G8,Square::H8,
    ];

    /// Creates a new square from a file and a rank.
    pub fn new(file: File, rank: Rank) -> Square {
        Square((u8::from(rank) << 3) | u8::from(file))
    }

    /// Returns the rank of the square.
    pub fn rank(self) -> Rank {
        (self.0 >> 3).into()
    }

    /// Returns the file of the square.
    pub fn file(self) -> File {
        (self.0 & 0b111).into()
    }

    /// Returns the diagonal of the square.
    pub fn diagonal(self) -> Diagonal {
        Diagonal(7u8 + u8::from(self.file()) - u8::from(self.rank()))
    }

    /// Returns the antidiagonal of the square.
    pub fn antidiagonal(self) -> Antidiagonal {
        Antidiagonal(u8::from(self.file()) + u8::from(self.rank()))
    }

    /// Returns a new square that is moved up by the specified number of ranks without checking for
    /// bounds.
    ///
    /// # Safety
    ///
    /// Caller must ensure that moving the rank up by `count` steps will result in a square that is
    /// still on the chessboard. This method will cause undefined behavior if the resulting square
    /// would be off
    /// the board.
    pub unsafe fn up_unchecked(self, count: i8) -> Square {
        let min_rank_value = u8::from(Rank::R1) as i8;
        let max_rank_value = u8::from(Rank::R8) as i8;
        let new_rank_value = u8::from(self.rank()) as i8 + count;
        debug_assert!(
            min_rank_value <= new_rank_value && new_rank_value <= max_rank_value,
            "The new rank value must be a valid value for a rank"
        );
        Square((self.0 as i8 + count * 8) as u8)
    }

    /// Moves a square up by the specified number of ranks.
    /// Returns Ok(Square) with the new square if the move is valid, MoveOffBoard if the move would
    /// go off the board.
    pub fn up(self, count: i8) -> CoordinatesResult<Square> {
        let min_rank_value = u8::from(Rank::R1) as i8;
        let max_rank_value = u8::from(Rank::R8) as i8;
        let new_rank_value = u8::from(self.rank()) as i8 + count;
        if new_rank_value < min_rank_value || max_rank_value < new_rank_value {
            return Err(CoordinatesError::MoveOffBoard);
        }
        Ok(unsafe { self.up_unchecked(count) })
    }

    /// Returns a new square that is moved down by the specified number of ranks without checking
    /// for bounds.
    ///
    /// # Safety
    ///
    /// Caller must ensure that moving the rank down by `count` step will result in a square that is
    /// still on the chessboard. This method will cause undefined behavior if we move the square off
    /// the board.
    pub unsafe fn down_unchecked(self, count: i8) -> Square {
        self.up_unchecked(-count)
    }

    /// Moves a square down by the specified number of ranks.
    /// Returns Ok(Square) with the new square if the move is valid, MoveOffBoard if the move would
    /// go off the board.
    pub fn down(self, count: i8) -> CoordinatesResult<Square> {
        self.up(-count)
    }

    /// Returns a new square that is moved left by the specified number of files without checking
    /// for bounds.
    ///
    /// # Safety
    ///
    /// Caller must ensure that moving the file left by `count` step will result in a square that is
    /// still on the chessboard. This method will cause undefined behavior if we move the square off
    /// the board.
    pub unsafe fn left_unchecked(self, count: i8) -> Square {
        let min_file_value = u8::from(File::A) as i8;
        let max_file_value = u8::from(File::H) as i8;
        let new_file_value = u8::from(self.file()) as i8 - count;
        debug_assert!(
            min_file_value <= new_file_value && new_file_value <= max_file_value,
            "The new file value must be a valid value for a file"
        );
        Square((self.0 as i8 - count) as u8)
    }

    /// Moves a square left by the specified number of files.
    /// Returns Ok(Square) with the new square if the move is valid, MoveOffBoard if the move would
    /// go off the board.
    pub fn left(self, count: i8) -> CoordinatesResult<Square> {
        let min_file_value = u8::from(File::A) as i8;
        let max_file_value = u8::from(File::H) as i8;
        let new_file_value = u8::from(self.file()) as i8 - count;
        if new_file_value < min_file_value || max_file_value < new_file_value {
            return Err(CoordinatesError::MoveOffBoard);
        }
        Ok(unsafe { self.left_unchecked(count) })
    }

    /// Returns a new square that is moved right by the specified number of files without checking
    /// for bounds.
    ///
    /// # Safety
    ///
    /// Caller must ensure that moving the file right by `count` step will result in a square that
    /// is still on the chessboard. This method will cause undefined behavior if we move the square
    /// off the board.
    pub unsafe fn right_unchecked(self, count: i8) -> Square {
        self.left_unchecked(-count)
    }

    /// Moves a square right by the specified number of files.
    /// Returns Ok(Square) with the new square if the move is valid, MoveOffBoard if the move would
    /// go off the board.
    pub fn right(self, count: i8) -> CoordinatesResult<Square> {
        self.left(-count)
    }
}

impl Display for Square {
    /// Formats the square as a two-character string.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
    }
}

impl fmt::Debug for Square {
    /// Formats the piece as a string for debuging
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
    }
}

impl From<Square> for u8 {
    /// Converts a `Square` to a `u8` value.
    fn from(square: Square) -> Self {
        square.0
    }
}

impl From<Square> for usize {
    /// Converts a `Square` to a `u8` value.
    fn from(square: Square) -> Self {
        square.0 as usize
    }
}

impl From<u8> for Square {
    /// Converts a `u8` value to a `Square`.
    fn from(value: u8) -> Self {
        assert!(value < 64);
        Square(value)
    }
}

impl TryFrom<&str> for Square {
    type Error = CoordinatesError;

    /// Converts a `&str` value to a `Square`.
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        if value.len() != 2 {
            return Err(CoordinatesError::InvalidString);
        }
        let file = File::try_from(value.chars().nth(0).expect("The size of value was checked"))?;
        let rank = Rank::try_from(value.chars().nth(1).expect("The size of value was checked"))?;
        Ok(Square::new(file, rank))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod file_tests {
        use super::*;

        #[test]
        fn test_file_display() {
            assert_eq!(format!("{}", File::A), "a");
            assert_eq!(format!("{}", File::H), "h");
        }

        #[test]
        fn test_file_conversion() {
            assert_eq!(u8::from(File::A), 0);
            assert_eq!(u8::from(File::H), 7);
            assert_eq!(File::from(0), File::A);
            assert_eq!(File::from(7), File::H);
        }

        #[test]
        fn test_file_conversion_to_char() {
            assert_eq!(char::from(File::A), 'a');
            assert_eq!(char::from(File::B), 'b');
            assert_eq!(char::from(File::C), 'c');
            assert_eq!(char::from(File::D), 'd');
            assert_eq!(char::from(File::E), 'e');
            assert_eq!(char::from(File::F), 'f');
            assert_eq!(char::from(File::G), 'g');
            assert_eq!(char::from(File::H), 'h');
        }

        #[test]
        fn test_invalid_conversion_do_panic() {
            assert!(std::panic::catch_unwind(|| File::from(8)).is_err());
        }

        #[test]
        fn test_right_unchecked() {
            let file1 = File::A;
            let result1 = unsafe { file1.right_unchecked(3) };
            assert_eq!(File::D, result1);

            let file2 = File::G;
            let result2 = unsafe { file2.right_unchecked(-4) };
            assert_eq!(File::C, result2);
        }

        #[test]
        fn test_right() {
            let file1 = File::C;
            let result1 = file1.right(2);
            assert_eq!(Ok(File::E), result1);

            let file2 = File::F;
            let result2 = file2.right(-3);
            assert_eq!(Ok(File::C), result2);

            let file3 = File::G;
            let result3 = file3.right(2);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result3);

            let file4 = File::B;
            let result4 = file4.right(-2);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result4);
        }

        #[test]
        fn test_left_unchecked() {
            let file1 = File::H;
            let result1 = unsafe { file1.left_unchecked(3) };
            assert_eq!(File::E, result1);

            let file2 = File::C;
            let result2 = unsafe { file2.left_unchecked(-3) };
            assert_eq!(File::F, result2);
        }

        #[test]
        fn test_left() {
            let file1 = File::D;
            let result1 = file1.left(2);
            assert_eq!(Ok(File::B), result1);

            let file2 = File::C;
            let result2 = file2.left(-3);
            assert_eq!(Ok(File::F), result2);

            let file3 = File::B;
            let result3 = file3.left(3);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result3);

            let file4 = File::E;
            let result4 = file4.left(-4);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result4);
        }

        #[test]
        fn test_try_from_char() {
            assert_eq!(File::try_from('a'), Ok(File::A));
            assert_eq!(File::try_from('b'), Ok(File::B));
            assert_eq!(File::try_from('c'), Ok(File::C));
            assert_eq!(File::try_from('d'), Ok(File::D));
            assert_eq!(File::try_from('e'), Ok(File::E));
            assert_eq!(File::try_from('f'), Ok(File::F));
            assert_eq!(File::try_from('g'), Ok(File::G));
            assert_eq!(File::try_from('h'), Ok(File::H));
            assert_eq!(File::try_from('i'), Err(CoordinatesError::InvalidCharacter));
        }
    }

    mod rank_tests {
        use super::*;

        #[test]
        fn test_rank_display() {
            assert_eq!(format!("{}", Rank::R1), "1");
            assert_eq!(format!("{}", Rank::R8), "8");
        }

        #[test]
        fn test_rank_conversion() {
            assert_eq!(u8::from(Rank::R1), 0);
            assert_eq!(u8::from(Rank::R8), 7);
            assert_eq!(Rank::from(0), Rank::R1);
            assert_eq!(Rank::from(7), Rank::R8);
        }

        #[test]
        fn test_invalid_conversion_do_panic() {
            assert!(std::panic::catch_unwind(|| Rank::from(8)).is_err());
        }

        #[test]
        fn test_up_unchecked() {
            let rank1 = Rank::R4;
            let result1 = unsafe { rank1.up_unchecked(2) };
            assert_eq!(Rank::R6, result1);

            let rank2 = Rank::R7;
            let result2 = unsafe { rank2.up_unchecked(-4) };
            assert_eq!(Rank::R3, result2);
        }

        #[test]
        fn test_up() {
            let rank1 = Rank::R4;
            let result1 = rank1.up(2);
            assert_eq!(Ok(Rank::R6), result1);

            let rank2 = Rank::R7;
            let result2 = rank2.up(-4);
            assert_eq!(Ok(Rank::R3), result2);

            let rank3 = Rank::R6;
            let result3 = rank3.up(3);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result3);

            let rank4 = Rank::R2;
            let result4 = rank4.up(-2);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result4);
        }

        #[test]
        fn test_down_unchecked() {
            let rank1 = Rank::R5;
            let result1 = unsafe { rank1.down_unchecked(2) };
            assert_eq!(Rank::R3, result1);

            let rank2 = Rank::R2;
            let result2 = unsafe { rank2.down_unchecked(-4) };
            assert_eq!(Rank::R6, result2);
        }

        #[test]
        fn test_down() {
            let rank1 = Rank::R5;
            let result1 = rank1.down(2);
            assert_eq!(Ok(Rank::R3), result1);

            let rank2 = Rank::R2;
            let result2 = rank2.down(-4);
            assert_eq!(Ok(Rank::R6), result2);

            let rank3 = Rank::R2;
            let result3 = rank3.down(3);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result3);

            let rank4 = Rank::R6;
            let result4 = rank4.down(-3);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result4);
        }

        #[test]
        fn test_try_from_char() {
            assert_eq!(Rank::try_from('1'), Ok(Rank::R1));
            assert_eq!(Rank::try_from('2'), Ok(Rank::R2));
            assert_eq!(Rank::try_from('3'), Ok(Rank::R3));
            assert_eq!(Rank::try_from('4'), Ok(Rank::R4));
            assert_eq!(Rank::try_from('5'), Ok(Rank::R5));
            assert_eq!(Rank::try_from('6'), Ok(Rank::R6));
            assert_eq!(Rank::try_from('7'), Ok(Rank::R7));
            assert_eq!(Rank::try_from('8'), Ok(Rank::R8));
            assert_eq!(Rank::try_from('9'), Err(CoordinatesError::InvalidCharacter));
        }

        #[test]
        fn test_relative_to_color() {
            // Check white relative ranks
            assert_eq!(Rank::R1.relative_to_color(Color::White), Rank::R1);
            assert_eq!(Rank::R2.relative_to_color(Color::White), Rank::R2);
            assert_eq!(Rank::R3.relative_to_color(Color::White), Rank::R3);
            assert_eq!(Rank::R4.relative_to_color(Color::White), Rank::R4);
            assert_eq!(Rank::R5.relative_to_color(Color::White), Rank::R5);
            assert_eq!(Rank::R6.relative_to_color(Color::White), Rank::R6);
            assert_eq!(Rank::R7.relative_to_color(Color::White), Rank::R7);
            assert_eq!(Rank::R8.relative_to_color(Color::White), Rank::R8);

            // Check black relative ranks (should be flipped)
            assert_eq!(Rank::R1.relative_to_color(Color::Black), Rank::R8);
            assert_eq!(Rank::R2.relative_to_color(Color::Black), Rank::R7);
            assert_eq!(Rank::R3.relative_to_color(Color::Black), Rank::R6);
            assert_eq!(Rank::R4.relative_to_color(Color::Black), Rank::R5);
            assert_eq!(Rank::R5.relative_to_color(Color::Black), Rank::R4);
            assert_eq!(Rank::R6.relative_to_color(Color::Black), Rank::R3);
            assert_eq!(Rank::R7.relative_to_color(Color::Black), Rank::R2);
            assert_eq!(Rank::R8.relative_to_color(Color::Black), Rank::R1);
        }
    }

    mod square_tests {
        use super::*;

        #[test]
        fn test_square_edge_cases() {
            assert_eq!(File::A, Square::A1.file());
            assert_eq!(Rank::R1, Square::A1.rank());
            assert_eq!(File::H, Square::H1.file());
            assert_eq!(Rank::R1, Square::H1.rank());
            assert_eq!(File::A, Square::A8.file());
            assert_eq!(Rank::R8, Square::A8.rank());
            assert_eq!(File::H, Square::H8.file());
            assert_eq!(Rank::R8, Square::H8.rank());
        }

        #[test]
        fn test_square_diagonals() {
            assert_eq!(Diagonal(0), Square::A8.diagonal());
            assert_eq!(Diagonal(7), Square::A1.diagonal());
            assert_eq!(Diagonal(7), Square::H8.diagonal());
            assert_eq!(Diagonal(14), Square::H1.diagonal());
        }

        #[test]
        fn test_square_antidiagonals() {
            assert_eq!(Antidiagonal(0), Square::A1.antidiagonal());
            assert_eq!(Antidiagonal(7), Square::A8.antidiagonal());
            assert_eq!(Antidiagonal(7), Square::H1.antidiagonal());
            assert_eq!(Antidiagonal(14), Square::H8.antidiagonal());
        }

        #[test]
        fn test_square_creation() {
            let e5 = Square::new(File::E, Rank::R5);
            assert_eq!(File::E, e5.file());
            assert_eq!(Rank::R5, e5.rank());
        }

        #[test]
        fn test_square_display() {
            assert_eq!(format!("{}", Square::A1), "a1");
            assert_eq!(format!("{}", Square::H8), "h8");
        }

        #[test]
        fn test_square_to_and_from_u8() {
            for square in Square::ALL.iter() {
                assert_eq!(*square, Square::from(u8::from(*square)));
            }
        }

        #[test]
        fn test_up_unchecked() {
            let sq1 = Square::E5;
            let result1 = unsafe { sq1.up_unchecked(1) };
            assert_eq!(Square::E6, result1);

            let sq2 = Square::G7;
            let result2 = unsafe { sq2.up_unchecked(-6) };
            assert_eq!(Square::G1, result2);
        }

        #[test]
        fn test_up() {
            let sq1 = Square::E5;
            let result1 = sq1.up(1);
            assert_eq!(Ok(Square::E6), result1);

            let sq2 = Square::G7;
            let result2 = sq2.up(-6);
            assert_eq!(Ok(Square::G1), result2);

            let sq3 = Square::B3;
            let result3 = sq3.up(6);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result3);
        }

        #[test]
        fn test_down_unchecked() {
            let sq1 = Square::E5;
            let result1 = unsafe { sq1.down_unchecked(1) };
            assert_eq!(Square::E4, result1);

            let sq2 = Square::G1;
            let result2 = unsafe { sq2.down_unchecked(-6) };
            assert_eq!(Square::G7, result2);
        }

        #[test]
        fn test_down() {
            let sq1 = Square::E5;
            let result1 = sq1.down(1);
            assert_eq!(Ok(Square::E4), result1);

            let sq2 = Square::G1;
            let result2 = sq2.down(-6);
            assert_eq!(Ok(Square::G7), result2);

            let sq3 = Square::B3;
            let result3 = sq3.down(4);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result3);
        }

        #[test]
        fn test_left_unchecked() {
            let sq1 = Square::E5;
            let result1 = unsafe { sq1.left_unchecked(1) };
            assert_eq!(Square::D5, result1);

            let sq2 = Square::D1;
            let result2 = unsafe { sq2.left_unchecked(-2) };
            assert_eq!(Square::F1, result2);
        }

        #[test]
        fn test_left() {
            let sq1 = Square::E5;
            let result1 = sq1.left(1);
            assert_eq!(Ok(Square::D5), result1);

            let sq2 = Square::G1;
            let result2 = sq2.left(-1);
            assert_eq!(Ok(Square::H1), result2);

            let sq3 = Square::A3;
            let result3 = sq3.left(1);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result3);
        }

        #[test]
        fn test_right_unchecked() {
            let sq1 = Square::E5;
            let result1 = unsafe { sq1.right_unchecked(1) };
            assert_eq!(Square::F5, result1);

            let sq2 = Square::B1;
            let result2 = unsafe { sq2.right_unchecked(-1) };
            assert_eq!(Square::A1, result2);
        }

        #[test]
        fn test_right() {
            let sq1 = Square::E5;
            let result1 = sq1.right(1);
            assert_eq!(Ok(Square::F5), result1);

            let sq2 = Square::B1;
            let result2 = sq2.right(-1);
            assert_eq!(Ok(Square::A1), result2);

            let sq3 = Square::H3;
            let result3 = sq3.right(1);
            assert_eq!(Err(CoordinatesError::MoveOffBoard), result3);
        }

        #[test]
        fn test_try_from_str() {
            assert_eq!(Square::try_from("a1"), Ok(Square::A1));
            assert_eq!(Square::try_from("b2"), Ok(Square::B2));
            assert_eq!(Square::try_from("c3"), Ok(Square::C3));
            assert_eq!(Square::try_from("d4"), Ok(Square::D4));
            assert_eq!(Square::try_from("e5"), Ok(Square::E5));
            assert_eq!(Square::try_from("f6"), Ok(Square::F6));
            assert_eq!(Square::try_from("g7"), Ok(Square::G7));
            assert_eq!(Square::try_from("h8"), Ok(Square::H8));
            assert_eq!(Square::try_from("i9"), Err(CoordinatesError::InvalidCharacter));
            assert_eq!(Square::try_from("x"), Err(CoordinatesError::InvalidString));
        }
    }
}
