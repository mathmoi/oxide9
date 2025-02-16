use std::convert::From;
use std::fmt::Display;

/// Represents the color of a chess piece.
#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Color {
    White = 0,
    Black = 1,
}

impl Color {
    /// Represents all colors of chess pieces.
    pub const ALL_COLORS: [Color; 2] = [Color::White, Color::Black];

    /// Returns the opposite color.
    pub fn opposite(&self) -> Color {
        match self {
            Color::White => Color::Black,
            Color::Black => Color::White,
        }
    }
}

impl Display for Color {
    /// Formats the color as a string.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Color::White => write!(f, "White"),
            Color::Black => write!(f, "Black"),
        }
    }
}

impl From<Color> for u8 {
    /// Converts a `Color` to a `u8` value.
    fn from(color: Color) -> Self {
        color as u8
    }
}

impl From<u8> for Color {
    /// Converts a `u8` value to a `Color`.
    fn from(value: u8) -> Self {
        assert!(value <= Color::Black.into());
        unsafe { std::mem::transmute(value) }
    }
}

#[repr(u8)]
#[derive(Copy, Clone, Debug, PartialEq)]
pub enum PieceType {
    Knight = 0,
    Bishop = 1,
    Rook = 2,
    Queen = 3,
    King = 4,
    Pawn = 5,
}

impl PieceType {
    /// Represents all piece types.
    pub const ALL_PIECE_TYPES: [PieceType; 6] = [
        PieceType::Pawn,
        PieceType::Knight,
        PieceType::Bishop,
        PieceType::Rook,
        PieceType::Queen,
        PieceType::King,
    ];
}

#[derive(Debug, PartialEq)]
/// Represents an error that occurs when converting a character to a `PieceType`.
pub enum PieceTypeError {
    InvalidCharacter(char),
}

impl From<PieceType> for u8 {
    /// Converts a `PieceType` to a `u8` value.
    fn from(piece_type: PieceType) -> Self {
        piece_type as u8
    }
}

impl From<u8> for PieceType {
    /// Converts a `u8` value to a `PieceType`.
    fn from(value: u8) -> Self {
        assert!(value <= PieceType::Pawn.into());
        unsafe { std::mem::transmute(value) }
    }
}

impl From<PieceType> for char {
    fn from(piece_type: PieceType) -> Self {
        match piece_type {
            PieceType::Pawn => 'P',
            PieceType::Knight => 'N',
            PieceType::Bishop => 'B',
            PieceType::Rook => 'R',
            PieceType::Queen => 'Q',
            PieceType::King => 'K',
        }
    }
}

impl TryFrom<char> for PieceType {
    type Error = PieceTypeError;

    fn try_from(value: char) -> Result<Self, Self::Error> {
        match value.to_ascii_lowercase() {
            'p' | 'P' => Ok(PieceType::Pawn),
            'n' | 'N' => Ok(PieceType::Knight),
            'b' | 'B' => Ok(PieceType::Bishop),
            'r' | 'R' => Ok(PieceType::Rook),
            'q' | 'Q' => Ok(PieceType::Queen),
            'k' | 'K' => Ok(PieceType::King),
            _ => Err(PieceTypeError::InvalidCharacter(value)),
        }
    }
}

impl Display for PieceType {
    /// Formats the PieceType as a single character.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PieceType::Pawn => write!(f, "Pawn"),
            PieceType::Knight => write!(f, "Knight"),
            PieceType::Bishop => write!(f, "Bishop"),
            PieceType::Rook => write!(f, "Rook"),
            PieceType::Queen => write!(f, "Queen"),
            PieceType::King => write!(f, "King"),
        }
    }
}

/// Represents a chess piece.
///
/// A `Piece` is a combination of a `Color` and a `PieceType`. It is represented as a single byte,
/// with the lower bits representing the `Color` and the higher bits representing the `PieceType`.
/// The values 0 to 11 represent all possible combinations of `Color` and `PieceType`.
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Piece(u8);

#[allow(dead_code)]
impl Piece {
    pub const WHITE_KNIGHT: Piece = Piece(0);
    pub const WHITE_BISHOP: Piece = Piece(2);
    pub const WHITE_ROOK: Piece = Piece(4);
    pub const WHITE_QUEEN: Piece = Piece(6);
    pub const WHITE_KING: Piece = Piece(8);
    pub const WHITE_PAWN: Piece = Piece(10);
    pub const BLACK_KNIGHT: Piece = Piece(1);
    pub const BLACK_BISHOP: Piece = Piece(3);
    pub const BLACK_ROOK: Piece = Piece(5);
    pub const BLACK_QUEEN: Piece = Piece(7);
    pub const BLACK_KING: Piece = Piece(9);
    pub const BLACK_PAWN: Piece = Piece(11);

    /// Represents all possible chess pieces.
    pub const ALL_PIECES: [Piece; 12] = [
        Piece::WHITE_PAWN,
        Piece::WHITE_KNIGHT,
        Piece::WHITE_BISHOP,
        Piece::WHITE_ROOK,
        Piece::WHITE_QUEEN,
        Piece::WHITE_KING,
        Piece::BLACK_PAWN,
        Piece::BLACK_KNIGHT,
        Piece::BLACK_BISHOP,
        Piece::BLACK_ROOK,
        Piece::BLACK_QUEEN,
        Piece::BLACK_KING,
    ];

    /// Creates a new `Piece` with the given `Color` and `PieceType`.
    pub fn new(color: Color, piece_type: PieceType) -> Self {
        Piece(u8::from(piece_type) << 1 | u8::from(color))
    }

    /// Returns the Color of the piece.
    pub fn color(&self) -> Color {
        Color::from(self.0 & 1)
    }

    /// Returns the PieceType of the piece.
    pub fn piece_type(&self) -> PieceType {
        PieceType::from(self.0 >> 1)
    }
}

impl From<Piece> for u8 {
    /// Converts a `Piece` to a `u8` value.
    fn from(piece: Piece) -> Self {
        piece.0
    }
}

impl From<u8> for Piece {
    /// Converts a `u8` value to a `Piece`.
    fn from(value: u8) -> Self {
        assert!(value <= Piece::BLACK_PAWN.into());
        Piece(value)
    }
}

impl From<Piece> for char {
    /// Converts a `Piece` to a single character.
    fn from(piece: Piece) -> Self {
        match piece.color() {
            Color::White => char::from(piece.piece_type()).to_ascii_uppercase(),
            Color::Black => char::from(piece.piece_type()).to_ascii_lowercase(),
        }
    }
}

impl TryFrom<char> for Piece {
    type Error = PieceTypeError;

    /// Converts a single character to a `Piece`.
    fn try_from(value: char) -> Result<Self, Self::Error> {
        let color = match char::is_uppercase(value) {
            true => Color::White,
            false => Color::Black,
        };
        let piece_type = PieceType::try_from(value)?;
        Ok(Piece::new(color, piece_type))
    }
}

impl Display for Piece {
    /// Formats the PieceType as a single character.
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} {}", self.color(), self.piece_type())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod color_tests {
        use super::*;

        #[test]
        fn test_color_display() {
            assert_eq!(format!("{}", Color::White), "White");
            assert_eq!(format!("{}", Color::Black), "Black");
        }

        #[test]
        fn test_color_conversion() {
            assert_eq!(u8::from(Color::White), 0);
            assert_eq!(u8::from(Color::Black), 1);
            assert_eq!(Color::from(0u8), Color::White);
            assert_eq!(Color::from(1u8), Color::Black);
        }

        #[test]
        fn test_opposite() {
            assert_eq!(Color::White.opposite(), Color::Black);
            assert_eq!(Color::Black.opposite(), Color::White);
        }
    }

    mod piece_type_tests {
        use super::*;

        #[test]
        fn test_piece_type_conversion() {
            assert_eq!(u8::from(PieceType::Knight), 0);
            assert_eq!(u8::from(PieceType::Bishop), 1);
            assert_eq!(u8::from(PieceType::Rook), 2);
            assert_eq!(u8::from(PieceType::Queen), 3);
            assert_eq!(u8::from(PieceType::King), 4);
            assert_eq!(u8::from(PieceType::Pawn), 5);
            assert_eq!(PieceType::from(0), PieceType::Knight);
            assert_eq!(PieceType::from(1), PieceType::Bishop);
            assert_eq!(PieceType::from(2), PieceType::Rook);
            assert_eq!(PieceType::from(3), PieceType::Queen);
            assert_eq!(PieceType::from(4), PieceType::King);
            assert_eq!(PieceType::from(5), PieceType::Pawn);
        }

        #[test]
        fn test_piece_type_from_character() {
            assert_eq!(PieceType::try_from('p'), Ok(PieceType::Pawn));
            assert_eq!(PieceType::try_from('P'), Ok(PieceType::Pawn));
            assert_eq!(PieceType::try_from('n'), Ok(PieceType::Knight));
            assert_eq!(PieceType::try_from('N'), Ok(PieceType::Knight));
            assert_eq!(PieceType::try_from('b'), Ok(PieceType::Bishop));
            assert_eq!(PieceType::try_from('B'), Ok(PieceType::Bishop));
            assert_eq!(PieceType::try_from('r'), Ok(PieceType::Rook));
            assert_eq!(PieceType::try_from('R'), Ok(PieceType::Rook));
            assert_eq!(PieceType::try_from('q'), Ok(PieceType::Queen));
            assert_eq!(PieceType::try_from('Q'), Ok(PieceType::Queen));
            assert_eq!(PieceType::try_from('k'), Ok(PieceType::King));
            assert_eq!(PieceType::try_from('K'), Ok(PieceType::King));
            assert!(PieceType::try_from('x').is_err());
            assert!(PieceType::try_from('1').is_err());
        }

        #[test]
        fn test_character_from_piece_type() {
            assert_eq!(char::from(PieceType::Pawn), 'P');
            assert_eq!(char::from(PieceType::Knight), 'N');
            assert_eq!(char::from(PieceType::Bishop), 'B');
            assert_eq!(char::from(PieceType::Rook), 'R');
            assert_eq!(char::from(PieceType::Queen), 'Q');
            assert_eq!(char::from(PieceType::King), 'K');
        }

        #[test]
        fn test_piece_type_display() {
            assert_eq!(format!("{}", PieceType::Pawn), "Pawn");
            assert_eq!(format!("{}", PieceType::Knight), "Knight");
            assert_eq!(format!("{}", PieceType::Bishop), "Bishop");
            assert_eq!(format!("{}", PieceType::Rook), "Rook");
            assert_eq!(format!("{}", PieceType::Queen), "Queen");
            assert_eq!(format!("{}", PieceType::King), "King");
        }
    }

    mod piece_tests {
        use super::*;

        fn test_single_piece_creation(color: Color, piece_type: PieceType) {
            let piece = Piece::new(color, piece_type);
            assert_eq!(piece.color(), color);
            assert_eq!(piece.piece_type(), piece_type);
        }

        #[test]
        fn test_piece_creation() {
            for color in Color::ALL_COLORS.iter() {
                for piece_type in PieceType::ALL_PIECE_TYPES.iter() {
                    test_single_piece_creation(*color, *piece_type);
                }
            }
        }

        #[test]
        fn test_constant_pieces() {
            assert_eq!(Piece::WHITE_PAWN, Piece::new(Color::White, PieceType::Pawn));
            assert_eq!(
                Piece::WHITE_KNIGHT,
                Piece::new(Color::White, PieceType::Knight)
            );
            assert_eq!(
                Piece::WHITE_BISHOP,
                Piece::new(Color::White, PieceType::Bishop)
            );
            assert_eq!(Piece::WHITE_ROOK, Piece::new(Color::White, PieceType::Rook));
            assert_eq!(
                Piece::WHITE_QUEEN,
                Piece::new(Color::White, PieceType::Queen)
            );
            assert_eq!(Piece::WHITE_KING, Piece::new(Color::White, PieceType::King));
            assert_eq!(Piece::BLACK_PAWN, Piece::new(Color::Black, PieceType::Pawn));
            assert_eq!(
                Piece::BLACK_KNIGHT,
                Piece::new(Color::Black, PieceType::Knight)
            );
            assert_eq!(
                Piece::BLACK_BISHOP,
                Piece::new(Color::Black, PieceType::Bishop)
            );
            assert_eq!(Piece::BLACK_ROOK, Piece::new(Color::Black, PieceType::Rook));
            assert_eq!(
                Piece::BLACK_QUEEN,
                Piece::new(Color::Black, PieceType::Queen)
            );
            assert_eq!(Piece::BLACK_KING, Piece::new(Color::Black, PieceType::King));
        }

        #[test]
        fn test_display_for_piece() {
            assert_eq!(format!("{}", Piece::WHITE_PAWN), "White Pawn");
            assert_eq!(format!("{}", Piece::WHITE_KNIGHT), "White Knight");
            assert_eq!(format!("{}", Piece::WHITE_BISHOP), "White Bishop");
            assert_eq!(format!("{}", Piece::WHITE_ROOK), "White Rook");
            assert_eq!(format!("{}", Piece::WHITE_QUEEN), "White Queen");
            assert_eq!(format!("{}", Piece::WHITE_KING), "White King");
            assert_eq!(format!("{}", Piece::BLACK_PAWN), "Black Pawn");
            assert_eq!(format!("{}", Piece::BLACK_KNIGHT), "Black Knight");
            assert_eq!(format!("{}", Piece::BLACK_BISHOP), "Black Bishop");
            assert_eq!(format!("{}", Piece::BLACK_ROOK), "Black Rook");
            assert_eq!(format!("{}", Piece::BLACK_QUEEN), "Black Queen");
            assert_eq!(format!("{}", Piece::BLACK_KING), "Black King");
        }

        #[test]
        fn test_conversion_to_and_from_u8() {
            for piece in Piece::ALL_PIECES.iter() {
                let value = u8::from(*piece);
                assert_eq!(Piece::from(value), *piece);
            }
        }

        #[test]
        fn test_from_piece_to_char() {
            assert_eq!(char::from(Piece::WHITE_PAWN), 'P');
            assert_eq!(char::from(Piece::WHITE_KNIGHT), 'N');
            assert_eq!(char::from(Piece::WHITE_BISHOP), 'B');
            assert_eq!(char::from(Piece::WHITE_ROOK), 'R');
            assert_eq!(char::from(Piece::WHITE_QUEEN), 'Q');
            assert_eq!(char::from(Piece::WHITE_KING), 'K');
            assert_eq!(char::from(Piece::BLACK_PAWN), 'p');
            assert_eq!(char::from(Piece::BLACK_KNIGHT), 'n');
            assert_eq!(char::from(Piece::BLACK_BISHOP), 'b');
            assert_eq!(char::from(Piece::BLACK_ROOK), 'r');
            assert_eq!(char::from(Piece::BLACK_QUEEN), 'q');
            assert_eq!(char::from(Piece::BLACK_KING), 'k');
        }

        #[test]
        fn test_from_char_to_piece() {
            assert_eq!(Piece::try_from('P'), Ok(Piece::WHITE_PAWN));
            assert_eq!(Piece::try_from('N'), Ok(Piece::WHITE_KNIGHT));
            assert_eq!(Piece::try_from('B'), Ok(Piece::WHITE_BISHOP));
            assert_eq!(Piece::try_from('R'), Ok(Piece::WHITE_ROOK));
            assert_eq!(Piece::try_from('Q'), Ok(Piece::WHITE_QUEEN));
            assert_eq!(Piece::try_from('K'), Ok(Piece::WHITE_KING));
            assert_eq!(Piece::try_from('p'), Ok(Piece::BLACK_PAWN));
            assert_eq!(Piece::try_from('n'), Ok(Piece::BLACK_KNIGHT));
            assert_eq!(Piece::try_from('b'), Ok(Piece::BLACK_BISHOP));
            assert_eq!(Piece::try_from('r'), Ok(Piece::BLACK_ROOK));
            assert_eq!(Piece::try_from('q'), Ok(Piece::BLACK_QUEEN));
            assert_eq!(Piece::try_from('k'), Ok(Piece::BLACK_KING));
            assert!(Piece::try_from('x').is_err());
        }
    }
}
