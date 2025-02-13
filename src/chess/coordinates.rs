use std::convert::From;
use std::fmt::Display;

/// Represents a file (column) on a chess board.
///
/// Files are labeled from A to H, going from left to right when viewing the board from White's
/// perspective.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
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
    pub const ALL_FILES: [File; 8] = [
        File::A,
        File::B,
        File::C,
        File::D,
        File::E,
        File::F,
        File::G,
        File::H,
    ];
}

impl Display for File {
    /// Formats the file as a single character.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", (u8::from(*self) + ('a' as u8)) as char)
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
    pub const ALL_RANKS: [Rank; 8] = [
        Rank::R1,
        Rank::R2,
        Rank::R3,
        Rank::R4,
        Rank::R5,
        Rank::R6,
        Rank::R7,
        Rank::R8,
    ];
}

impl Display for Rank {
    /// Formats the rank as a single character.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", (u8::from(*self) + ('1' as u8)) as char)
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

/// Represents a square on a chess board.
///
/// Squares are indexed from 0 to 63, starting from A1 and ending at H8 with A2 being at index 1.
/// In other words, the file value is stored in the lower 3 bits and the rank value is stored in the
/// next 3 bits. The last two bits are unused and always 0.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Square(u8);

#[allow(dead_code)]
impl Square {
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
    pub const ALL_SQUARES: [Square; 64] = [
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
        Square(u8::from(rank) << 3 | u8::from(file))
    }

    /// Returns the rank of the square.
    pub fn rank(&self) -> Rank {
        (self.0 >> 3).into()
    }

    /// Returns the file of the square.
    pub fn file(&self) -> File {
        (self.0 & 0b111).into()
    }
}

impl Display for Square {
    /// Formats the square as a two-character string.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}{}", self.file(), self.rank())
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
        fn test_invalid_conversion_do_panic() {
            assert!(std::panic::catch_unwind(|| File::from(8)).is_err());
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
    }
}
