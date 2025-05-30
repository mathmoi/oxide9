use bitflags::bitflags;

use crate::eval::Eval;

use super::{
    coordinates::Square,
    piece::{Color, Piece},
};

/// This is an enum that represents the different sides of the board that a castling move can be
/// made.
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum CastlingSide {
    Kingside = 0,
    Queenside = 1,
}

impl CastlingSide {
    /// The number of castling sides.
    pub const COUNT: usize = 2;

    /// An array of all the castling sides.
    pub const ALL: [CastlingSide; Self::COUNT] = [CastlingSide::Kingside, CastlingSide::Queenside];

    pub const KINGSIDE_VALUE: u8 = 0;
    pub const QUEENSIDE_VALUE: u8 = 1;
}

impl From<CastlingSide> for usize {
    fn from(side: CastlingSide) -> usize {
        match side {
            CastlingSide::Kingside => 0,
            CastlingSide::Queenside => 1,
        }
    }
}

impl From<CastlingSide> for u8 {
    fn from(side: CastlingSide) -> u8 {
        match side {
            CastlingSide::Kingside => 0,
            CastlingSide::Queenside => 1,
        }
    }
}

impl From<u8> for CastlingSide {
    fn from(value: u8) -> Self {
        match value {
            0 => CastlingSide::Kingside,
            1 => CastlingSide::Queenside,
            _ => unreachable!(),
        }
    }
}

// This is a bitflags struct that represents the different types of castling moves that can be made.
bitflags! {
    #[derive(Debug, Clone, PartialEq, Eq, Copy, Hash)]
    pub struct CastlingRight: u8 {
        const WHITE_KINGSIDE = 0b0001;
        const WHITE_QUEENSIDE = 0b0010;
        const BLACK_KINGSIDE = 0b0100;
        const BLACK_QUEENSIDE = 0b1000;

        const KINGSIDE = Self::WHITE_KINGSIDE.bits() | Self::BLACK_KINGSIDE.bits();
        const QUEENSIDE = Self::WHITE_QUEENSIDE.bits() | Self::BLACK_QUEENSIDE.bits();

        const WHITE = Self::WHITE_KINGSIDE.bits() | Self::WHITE_QUEENSIDE.bits();
        const BLACK = Self::BLACK_KINGSIDE.bits() | Self::BLACK_QUEENSIDE.bits();

        const ALL = Self::WHITE.bits() | Self::BLACK.bits();
    }
}

impl CastlingRight {
    /// The number of possible combinations of castling rights.
    pub const COUNT: usize = 16;

    /// Creates a new castling right from a color and a side.
    pub fn new(color: Color, side: CastlingSide) -> Self {
        match (color, side) {
            (Color::White, CastlingSide::Kingside) => CastlingRight::WHITE_KINGSIDE,
            (Color::White, CastlingSide::Queenside) => CastlingRight::WHITE_QUEENSIDE,
            (Color::Black, CastlingSide::Kingside) => CastlingRight::BLACK_KINGSIDE,
            (Color::Black, CastlingSide::Queenside) => CastlingRight::BLACK_QUEENSIDE,
        }
    }
}

// This is an enum that represents the different types of moves that a piece can make.
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
pub enum MoveType {
    Basic,
    Capture(Piece),
    Promotion(Piece),
    CapturePromotion { capture: Piece, promotion: Piece },
    TwoSquarePawnPush,
    EnPassant,
    Castling(CastlingSide),
}

impl From<MoveType> for u8 {
    fn from(move_type: MoveType) -> u8 {
        match move_type {
            MoveType::Basic => 0,
            MoveType::Capture(_) => 1,
            MoveType::Promotion(_) => 2,
            MoveType::CapturePromotion { .. } => 3,
            MoveType::TwoSquarePawnPush => 4,
            MoveType::EnPassant => 5,
            MoveType::Castling(_) => 6,
        }
    }
}

// This is a struct that represents a move in a chess game.
#[derive(Debug, PartialEq, Eq, Clone, Copy, Hash)]
#[repr(align(8))]
pub struct Move {
    from_square: Square,
    to_square: Square,
    piece: Piece,
    move_type: MoveType,
    eval: Eval,
}

impl Move {
    /// This is a constructor for a new move that is basic (i.e., not a capture, promotion, etc.).
    pub fn new(from_square: Square, to_square: Square, piece: Piece) -> Self {
        Self { from_square, to_square, piece, move_type: MoveType::Basic, eval: Eval::default() }
    }

    /// This is a constructor for a new move that is a capture.
    pub fn new_capture(from_square: Square, to_square: Square, piece: Piece, capture: Piece) -> Self {
        Self { from_square, to_square, piece, move_type: MoveType::Capture(capture), eval: Eval::default() }
    }

    /// Creates a new move that is a promotion.
    pub fn new_promotion(from_square: Square, to_square: Square, piece: Piece, promotion: Piece) -> Self {
        Self { from_square, to_square, piece, move_type: MoveType::Promotion(promotion), eval: Eval::default() }
    }

    /// Creates a new move that is both a capture and a promotion.
    pub fn new_capture_promotion(
        from_square: Square,
        to_square: Square,
        piece: Piece,
        capture: Piece,
        promotion: Piece,
    ) -> Self {
        Self {
            from_square,
            to_square,
            piece,
            move_type: MoveType::CapturePromotion { capture, promotion },
            eval: Eval::default(),
        }
    }

    /// Creates a new move that is a two-square pawn push.
    pub fn new_two_square_pawn_push(from_square: Square, to_square: Square, piece: Piece) -> Self {
        Self { from_square, to_square, piece, move_type: MoveType::TwoSquarePawnPush, eval: Eval::default() }
    }

    /// Creates a new move that is a capture of a pawn en passant.
    pub fn new_en_passant(from_square: Square, to_square: Square, piece: Piece) -> Self {
        Self { from_square, to_square, piece, move_type: MoveType::EnPassant, eval: Eval::default() }
    }

    /// Creates a new move that is a castling move.
    pub fn new_castling(from_square: Square, to_square: Square, piece: Piece, side: CastlingSide) -> Self {
        Self { from_square, to_square, piece, move_type: MoveType::Castling(side), eval: Eval::default() }
    }

    /// Sets the evaluation of the move.
    pub fn set_eval(&mut self, eval: Eval) {
        self.eval = eval;
    }

    /// Returns the evaluation of the move.
    pub fn eval(self) -> Eval {
        self.eval
    }

    /// Returns the source square of the move.
    pub fn from_square(self) -> Square {
        self.from_square
    }

    /// Returns the destination square of the move.
    pub fn to_square(self) -> Square {
        self.to_square
    }

    /// Returns the piece that is moving.
    pub fn piece(self) -> Piece {
        self.piece
    }

    /// Returns the type of move.
    pub fn move_type(self) -> MoveType {
        self.move_type
    }

    /// Converts a chess move to UCI (Universal Chess Interface) string notation.
    ///
    /// The UCI protocol represents moves as a string with:
    /// - The origin square (e.g., "e2")
    /// - The destination square (e.g., "e4")
    /// - An optional promotion piece character (e.g., "q" for queen promotion)
    ///
    /// # Returns
    /// A String representing the move in UCI format (e.g., "e2e4", "e7e8q"). For regular moves and captures, only the
    /// squares are included. For promotions, the promotion piece character is appended.
    ///
    /// # Examples
    /// - A basic move from e2 to e4 returns "e2e4"
    /// - A queen promotion from e7 to e8 returns "e7e8q"
    pub fn to_uci_string(self) -> String {
        let promotion = match self.move_type {
            MoveType::Promotion(promotion) => char::from(promotion.piece_type()).to_lowercase().to_string(),
            MoveType::CapturePromotion { promotion, .. } => {
                char::from(promotion.piece_type()).to_lowercase().to_string()
            }
            _ => "".to_string(),
        };
        format!("{}{}{}", self.from_square, self.to_square, promotion)
    }

    /// Converts a `Move` to a `u32` value.
    ///
    /// The `u32` value is packed as follows:
    /// Bits     Content
    /// -------  ---------
    ///  0 - 2   Move type
    ///  3 - 9   From square
    /// 10 - 15  To square
    /// 16 - 19  Moved piece
    /// 20 - 27  Payload (depends on the move type, at most 8 bits are necessary)
    pub fn pack(maybe_move: Option<Self>) -> u32 {
        match maybe_move {
            Some(mv) => {
                let from = u8::from(mv.from_square()) as u32;
                let to = u8::from(mv.to_square()) as u32;
                let piece = u8::from(mv.piece()) as u32;
                let move_type = u8::from(mv.move_type()) as u32;
                let payload = match mv.move_type() {
                    MoveType::Capture(capture) => u8::from(capture) as u32,
                    MoveType::Promotion(promotion) => u8::from(promotion) as u32,
                    MoveType::CapturePromotion { capture, promotion } => {
                        u8::from(capture) as u32 | ((u8::from(promotion) as u32) << 4)
                    }
                    MoveType::Castling(side) => u8::from(side) as u32,
                    _ => 0,
                };

                move_type | (from << 3) | (to << 10) | (piece << 16) | (payload << 20)
            }
            None => 0,
        }
    }

    /// Converts a `u32` value back to a `Move`.
    pub fn unpack(value: u32) -> Option<Self> {
        if value == 0 {
            return None;
        }

        let from = Square::from(((value >> 3) & 0b111111) as u8);
        let to = Square::from(((value >> 10) & 0b111111) as u8);
        let piece = Piece::from(((value >> 16) & 0b1111) as u8);
        let move_type = match value & 0b111 {
            0 => MoveType::Basic,
            1 => MoveType::Capture(Piece::from((value >> 20) as u8)),
            2 => MoveType::Promotion(Piece::from((value >> 20) as u8)),
            3 => MoveType::CapturePromotion {
                capture: Piece::from(((value >> 20) & 0b1111) as u8),
                promotion: Piece::from((value >> 24) as u8),
            },
            4 => MoveType::TwoSquarePawnPush,
            5 => MoveType::EnPassant,
            6 => MoveType::Castling(CastlingSide::from((value >> 20) as u8)),
            _ => unreachable!(),
        };

        Some(Self { from_square: from, to_square: to, piece, move_type, eval: Eval::default() })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod move_type_tests {
        use super::*;

        #[test]
        fn test_move_type_into_u8() {
            assert_eq!(u8::from(MoveType::Basic), 0);
            assert_eq!(u8::from(MoveType::Capture(Piece::WHITE_PAWN)), 1);
            assert_eq!(u8::from(MoveType::Promotion(Piece::WHITE_QUEEN)), 2);
            assert_eq!(
                u8::from(MoveType::CapturePromotion { capture: Piece::BLACK_KNIGHT, promotion: Piece::WHITE_QUEEN }),
                3
            );
            assert_eq!(u8::from(MoveType::TwoSquarePawnPush), 4);
            assert_eq!(u8::from(MoveType::EnPassant), 5);
            assert_eq!(u8::from(MoveType::Castling(CastlingSide::Kingside)), 6);
        }
    }

    mod move_tests {
        use super::*;

        #[test]
        fn test_size_of_move() {
            assert!(std::mem::size_of::<Move>() <= 8);
            assert!(std::mem::size_of::<Option<Move>>() <= 8);
        }

        #[test]
        fn test_new_quiet_move() {
            let from = Square::D2;
            let to = Square::D3;
            let piece = Piece::WHITE_PAWN;
            let quiet_move = Move::new(from, to, piece);
            assert_eq!(quiet_move.from_square(), from);
            assert_eq!(quiet_move.to_square(), to);
            assert_eq!(quiet_move.piece(), piece);
            assert_eq!(quiet_move.move_type(), MoveType::Basic);
        }

        #[test]
        fn test_new_capture_move() {
            let from = Square::D4;
            let to = Square::E7;
            let piece = Piece::WHITE_KNIGHT;
            let capture = Piece::BLACK_PAWN;
            let capture_move = Move::new_capture(from, to, piece, capture);
            assert_eq!(capture_move.from_square(), from);
            assert_eq!(capture_move.to_square(), to);
            assert_eq!(capture_move.piece(), piece);
            assert_eq!(capture_move.move_type(), MoveType::Capture(capture));
        }

        #[test]
        fn test_new_promotion_move() {
            let from = Square::A7;
            let to = Square::A8;
            let piece = Piece::WHITE_PAWN;
            let promotion = Piece::WHITE_QUEEN;
            let promotion_move = Move::new_promotion(from, to, piece, promotion);
            assert_eq!(promotion_move.from_square(), from);
            assert_eq!(promotion_move.to_square(), to);
            assert_eq!(promotion_move.piece(), piece);
            assert_eq!(promotion_move.move_type(), MoveType::Promotion(promotion));
        }

        #[test]
        fn test_new_capture_promotion_move() {
            let from = Square::H7;
            let to = Square::G8;
            let piece = Piece::WHITE_PAWN;
            let capture = Piece::BLACK_KNIGHT;
            let promotion = Piece::WHITE_QUEEN;
            let capture_promotion_move = Move::new_capture_promotion(from, to, piece, capture, promotion);
            assert_eq!(capture_promotion_move.from_square(), from);
            assert_eq!(capture_promotion_move.to_square(), to);
            assert_eq!(capture_promotion_move.piece(), piece);
            assert_eq!(capture_promotion_move.move_type(), MoveType::CapturePromotion { capture, promotion });
        }

        #[test]
        fn test_new_two_square_pawn_push_move() {
            let from = Square::E2;
            let to = Square::E4;
            let piece = Piece::WHITE_PAWN;
            let two_square_pawn_push_move = Move::new_two_square_pawn_push(from, to, piece);
            assert_eq!(two_square_pawn_push_move.from_square(), from);
            assert_eq!(two_square_pawn_push_move.to_square(), to);
            assert_eq!(two_square_pawn_push_move.piece(), piece);
            assert_eq!(two_square_pawn_push_move.move_type(), MoveType::TwoSquarePawnPush);
        }

        #[test]
        fn test_new_en_passant_move() {
            let from = Square::D5;
            let to = Square::E6;
            let piece = Piece::WHITE_PAWN;
            let en_passant_move = Move::new_en_passant(from, to, piece);
            assert_eq!(en_passant_move.from_square(), from);
            assert_eq!(en_passant_move.to_square(), to);
            assert_eq!(en_passant_move.piece(), piece);
            assert_eq!(en_passant_move.move_type(), MoveType::EnPassant);
        }

        #[test]
        fn test_new_castling_move() {
            let from = Square::E1;
            let to = Square::G1;
            let piece = Piece::WHITE_KING;
            let side = CastlingSide::Kingside;
            let castling_move = Move::new_castling(from, to, piece, side);
            assert_eq!(castling_move.from_square(), from);
            assert_eq!(castling_move.to_square(), to);
            assert_eq!(castling_move.piece(), piece);
            assert_eq!(castling_move.move_type(), MoveType::Castling(side));
        }

        #[test]
        fn test_getters() {
            let from = Square::E2;
            let to = Square::E4;
            let piece = Piece::WHITE_PAWN;
            let capture = Piece::BLACK_PAWN;
            let move_type = MoveType::Capture(capture);

            let chess_move = Move { from_square: from, to_square: to, piece, move_type, eval: Eval::default() };

            assert_eq!(chess_move.from_square(), from);
            assert_eq!(chess_move.to_square(), to);
            assert_eq!(chess_move.piece(), piece);
            assert_eq!(chess_move.move_type(), move_type);
        }

        #[test]
        fn test_pack_and_unpack() {
            let basic = Move::new(Square::A1, Square::A2, Piece::WHITE_ROOK);
            let capture = Move::new_capture(Square::A1, Square::B2, Piece::WHITE_QUEEN, Piece::BLACK_KNIGHT);
            let promotion = Move::new_promotion(Square::A7, Square::A8, Piece::WHITE_PAWN, Piece::WHITE_QUEEN);
            let capture_promotion = Move::new_capture_promotion(
                Square::H7,
                Square::G8,
                Piece::WHITE_PAWN,
                Piece::BLACK_KNIGHT,
                Piece::WHITE_QUEEN,
            );
            let two_square_pawn_push = Move::new_two_square_pawn_push(Square::E2, Square::E4, Piece::WHITE_PAWN);
            let en_passant = Move::new_en_passant(Square::D5, Square::E6, Piece::WHITE_PAWN);
            let kingside_castling =
                Move::new_castling(Square::E1, Square::G1, Piece::WHITE_KING, CastlingSide::Kingside);
            let queenside_castling =
                Move::new_castling(Square::E1, Square::C1, Piece::WHITE_KING, CastlingSide::Queenside);

            assert_eq!(Some(basic), Move::unpack(Move::pack(Some(basic))));
            assert_eq!(Some(capture), Move::unpack(Move::pack(Some(capture))));
            assert_eq!(Some(promotion), Move::unpack(Move::pack(Some(promotion))));
            assert_eq!(Some(capture_promotion), Move::unpack(Move::pack(Some(capture_promotion))));
            assert_eq!(Some(two_square_pawn_push), Move::unpack(Move::pack(Some(two_square_pawn_push))));
            assert_eq!(Some(en_passant), Move::unpack(Move::pack(Some(en_passant))));
            assert_eq!(Some(kingside_castling), Move::unpack(Move::pack(Some(kingside_castling))));
            assert_eq!(Some(queenside_castling), Move::unpack(Move::pack(Some(queenside_castling))));
            assert_eq!(None, Move::unpack(Move::pack(None)));
        }
    }
}
