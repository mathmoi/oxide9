use std::ops::Index;

use super::{r#move::Castling, Bitboard, Color, File, Piece, Rank, Square};

/// Error type for parsing a FEN (Forsyth-Edwards Notation) string.
#[derive(Debug)]
pub enum FenError {
    InvalidPiecePlacement,
    InvalidActiveColor,
    InvalidCastlingAvailability,
    InvalidEnPassantSquare,
    InvalidHalfmoveClock,
    InvalidFullmoveNumber,
    MissingField,
}

pub struct Position {
    side_to_move: Color,
    board: [Option<Piece>; Square::COUNT],
    bb_color: [Bitboard; Color::COUNT],
    bb_piece: [Bitboard; Piece::COUNT],
    castling_availability: Castling,
    en_passant_square: Option<Square>,
    halfmove_clock: u16,
    fullmove_number: u16,
}

impl Position {
    fn read_piece_placement(&mut self, piece_placement: &str) -> Result<(), FenError> {
        let mut file = Some(File::A);
        let mut rank = Some(Rank::R8);
        for c in piece_placement.chars() {
            if let Ok(piece) = Piece::try_from(c) {
                let rank_value = rank.ok_or(FenError::InvalidPiecePlacement)?;
                let file_value = file.ok_or(FenError::InvalidPiecePlacement)?;
                let square = Square::new(file_value, rank_value);
                self.put_piece(piece, square);
                file = file_value.right(1).ok();
            } else if let Some(number) = c.to_digit(10) {
                file = file
                    .ok_or(FenError::InvalidPiecePlacement)?
                    .right(number as i8)
                    .ok();
            } else if c == '/' {
                rank = rank.ok_or(FenError::InvalidPiecePlacement)?.down(1).ok();
                file = Some(File::A);
            } else {
                return Err(FenError::InvalidPiecePlacement);
            }
        }
        Ok(())
    }

    fn read_active_color(&mut self, active_color: &str) -> Result<(), FenError> {
        self.side_to_move = match active_color {
            "w" => Color::White,
            "b" => Color::Black,
            _ => return Err(FenError::InvalidActiveColor),
        };
        Ok(())
    }

    fn read_castling(&mut self, castling_availability: &str) -> Result<(), FenError> {
        for c in castling_availability.chars() {
            match c {
                'K' => self.castling_availability |= Castling::WHITE_KINGSIDE,
                'Q' => self.castling_availability |= Castling::WHITE_QUEENSIDE,
                'k' => self.castling_availability |= Castling::BLACK_KINGSIDE,
                'q' => self.castling_availability |= Castling::BLACK_QUEENSIDE,
                '-' => break,
                _ => return Err(FenError::InvalidCastlingAvailability),
            }
        }
        Ok(())
    }

    fn read_en_passant_square(&mut self, en_passant_square: &str) -> Result<(), FenError> {
        self.en_passant_square = match en_passant_square {
            "-" => None,
            _ => Some(
                Square::try_from(en_passant_square)
                    .map_err(|_| FenError::InvalidEnPassantSquare)?,
            ),
        };
        Ok(())
    }

    /// Creates a new chess position from a FEN (Forsyth-Edwards Notation) string.
    ///
    /// A FEN string contains 6 fields separated by spaces:
    ///
    /// 1. Piece placement: Each rank is described from 8 to 1, separated by '/'. Letters represent
    ///    pieces (P=pawn, N=knight, B=bishop, R=rook, Q=queen, K=king). Uppercase is white,
    ///    lowercase is black. Numbers represent empty squares.
    ///    Example: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
    ///
    /// 2. Active color: "w" means White moves next, "b" means Black moves next.
    ///
    /// 3. Castling availability: Combination of "K"(white kingside), "Q"(white queenside),
    ///    "k"(black kingside), "q"(black queenside), or "-" if no castling is possible.
    ///
    /// 4. En passant target square: The square where a pawn can be captured en passant, in
    ///    algebraic notation (e.g., "e3"), or "-" if not available.
    ///
    /// 5. Halfmove clock: Number of halfmoves since the last pawn advance or piece capture. Used
    ///    for the fifty-move rule.
    ///
    /// 6. Fullmove number: The number of complete moves. Starts at 1 and increments after Black's
    ///    move.
    ///
    /// # Arguments
    ///
    /// * fen - A string containing the FEN representation of a chess position.
    ///         FEN is a standard notation to describe a particular board position of a chess
    ///         game.
    ///
    /// # Examples
    ///
    /// ```
    /// // Initial chess position
    /// let pos = oxide9::chess::Position::new_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1");
    /// ```
    ///
    /// # See also
    /// [The PGN specifications](https://ia902908.us.archive.org/26/items/pgn-standard-1994-03-12/PGN_standard_1994-03-12.txt)
    /// that defines the FEN format at section 16.1.
    pub fn new_from_fen(fen: &str) -> Result<Self, FenError> {
        let mut position = Position::default();

        let mut fields = fen.split_whitespace();
        position.read_piece_placement(fields.next().ok_or(FenError::MissingField)?)?;
        position.read_active_color(fields.next().ok_or(FenError::MissingField)?)?;
        position.read_castling(fields.next().ok_or(FenError::MissingField)?)?;
        position.read_en_passant_square(fields.next().ok_or(FenError::MissingField)?)?;

        position.halfmove_clock = fields
            .next()
            .ok_or(FenError::MissingField)?
            .parse()
            .map_err(|_| FenError::InvalidHalfmoveClock)?;

        position.fullmove_number = fields
            .next()
            .ok_or(FenError::MissingField)?
            .parse()
            .map_err(|_| FenError::InvalidFullmoveNumber)?;

        Ok(position)
    }

    /// Creates a new chess position with the initial board setup.
    ///
    /// # Examples
    ///
    /// ```
    /// // Create a new Position with the initial chess position
    /// let pos = oxide9::chess::Position::new();
    /// ```
    pub fn new() -> Self {
        const INITIAL_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        Self::new_from_fen(INITIAL_POSITION).expect("This can not fail because the INITIAL_POSITION fen will always be successfully parsed.")
    }

    /// Returns the bitboard representing the positions of all pieces of a specific color.
    pub fn bb_color(&self, color: Color) -> Bitboard {
        self.bb_color[usize::from(color)]
    }

    /// Returns the bitboard representing the positions of all pieces of a specific type.
    pub fn bb_piece(&self, piece: Piece) -> Bitboard {
        self.bb_piece[usize::from(piece)]
    }

    /// Returns the color of the side to move.
    pub fn side_to_move(&self) -> Color {
        self.side_to_move
    }

    /// Returns the castling availability of the position.
    pub fn castling_availability(&self) -> Castling {
        self.castling_availability
    }

    /// Returns the en passant square of the position.
    pub fn en_passant_square(&self) -> Option<Square> {
        self.en_passant_square
    }

    /// Puts a piece on a specific square.
    pub fn put_piece(&mut self, piece: Piece, square: Square) {
        self.board[usize::from(square)] = Some(piece);
        self.bb_color[usize::from(piece.color())] |= square;
        self.bb_piece[usize::from(piece)] |= Bitboard::from(square);
    }

    /// Removes a piece from a specific square.
    pub fn remove_piece(&mut self, square: Square) {
        let piece = self.board[usize::from(square)]
            .expect("It is not possible to remove a piece from an empty square.");
        self.board[usize::from(square)] = None;
        self.bb_color[usize::from(piece.color())] ^= Bitboard::from(square);
        self.bb_piece[usize::from(piece)] ^= Bitboard::from(square);
    }

    /// Moves a lnown chess piece from one square to another. The piece must be present on the
    /// `from` square and the `to` square must be empty.
    pub fn move_piece(&mut self, piece: Piece, from: Square, to: Square) {
        debug_assert_eq!(self.board[usize::from(from)], Some(piece));
        debug_assert_eq!(self.board[usize::from(to)], None);

        self.board[usize::from(from)] = None;
        self.board[usize::from(to)] = Some(piece);
        let bb = from | to;
        self.bb_color[usize::from(piece.color())] ^= bb;
        self.bb_piece[usize::from(piece)] ^= bb;
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            side_to_move: Color::White,
            board: [None; Square::COUNT],
            bb_color: [Bitboard::EMPTY; Color::COUNT],
            bb_piece: [Bitboard::EMPTY; Piece::COUNT],
            castling_availability: Castling::empty(),
            en_passant_square: None,
            halfmove_clock: 0,
            fullmove_number: 1,
        }
    }
}

impl Index<Square> for Position {
    type Output = Option<Piece>;
    fn index(&self, index: Square) -> &Self::Output {
        &self.board[u8::from(index) as usize]
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_new_initial_position() {
        let position = Position::new();

        assert_eq!(position.side_to_move, Color::White);

        assert_eq!(position[Square::A1], Some(Piece::WHITE_ROOK));
        assert_eq!(position[Square::B1], Some(Piece::WHITE_KNIGHT));
        assert_eq!(position[Square::C1], Some(Piece::WHITE_BISHOP));
        assert_eq!(position[Square::D1], Some(Piece::WHITE_QUEEN));
        assert_eq!(position[Square::E1], Some(Piece::WHITE_KING));
        assert_eq!(position[Square::F1], Some(Piece::WHITE_BISHOP));
        assert_eq!(position[Square::G1], Some(Piece::WHITE_KNIGHT));
        assert_eq!(position[Square::H1], Some(Piece::WHITE_ROOK));

        for file in File::ALL_FILES {
            assert_eq!(
                position[Square::new(file, Rank::R2)],
                Some(Piece::WHITE_PAWN)
            );
        }

        for rank in Rank::ALL_RANKS[2..6].iter() {
            for file in File::ALL_FILES {
                assert_eq!(position[Square::new(file, *rank)], None);
            }
        }

        for file in File::ALL_FILES {
            assert_eq!(
                position[Square::new(file, Rank::R7)],
                Some(Piece::BLACK_PAWN)
            );
        }

        assert_eq!(position[Square::A8], Some(Piece::BLACK_ROOK));
        assert_eq!(position[Square::B8], Some(Piece::BLACK_KNIGHT));
        assert_eq!(position[Square::C8], Some(Piece::BLACK_BISHOP));
        assert_eq!(position[Square::D8], Some(Piece::BLACK_QUEEN));
        assert_eq!(position[Square::E8], Some(Piece::BLACK_KING));
        assert_eq!(position[Square::F8], Some(Piece::BLACK_BISHOP));
        assert_eq!(position[Square::G8], Some(Piece::BLACK_KNIGHT));
        assert_eq!(position[Square::H8], Some(Piece::BLACK_ROOK));

        assert_eq!(Color::White, position.side_to_move());

        assert_eq!(position.castling_availability(), Castling::all());

        assert_eq!(position.en_passant_square(), None);

        assert_eq!(position.halfmove_clock, 0);
        assert_eq!(position.fullmove_number, 1);
    }

    #[test]
    fn test_new_from_fen_black_to_play() {
        let position =
            Position::new_from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1")
                .unwrap();

        assert_eq!(position.side_to_move, Color::Black);
    }

    #[test]
    fn test_new_from_fen_no_castling_rights() {
        let position =
            Position::new_from_fen("1nbqkbn1/rppppppr/p6p/8/8/P6P/RPPPPPPR/1NBQKBN1 w - - 4 5")
                .unwrap();

        assert_eq!(position.castling_availability(), Castling::empty());
    }

    #[test]
    fn test_new_from_fen_partial_castling_rights() {
        let position =
            Position::new_from_fen("1nbqkbnr/rppppppp/p7/8/8/7P/PPPPPPPR/RNBQKBN1 w Qk - 2 3")
                .unwrap();

        assert_eq!(
            position.castling_availability(),
            Castling::BLACK_KINGSIDE | Castling::WHITE_QUEENSIDE
        );
    }

    #[test]
    fn test_new_from_fen_en_passant_square() {
        let position =
            Position::new_from_fen("rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3")
                .unwrap();

        assert_eq!(position.en_passant_square(), Some(Square::D6));
    }

    #[test]
    fn test_position_default() {
        let position = Position::default();
        assert_eq!(position.side_to_move, Color::White);
        assert!(position.board.iter().all(|square| square.is_none()));
        assert!(position.bb_color.iter().all(|bb| bb.is_empty()));
        assert!(position.bb_piece.iter().all(|bb| bb.is_empty()));
    }

    #[test]
    fn test_put_piece() {
        let mut position = Position::default();

        position.put_piece(Piece::WHITE_QUEEN, Square::E4);

        assert_eq!(position[Square::E4], Some(Piece::WHITE_QUEEN));
        assert_eq!(position.bb_color(Color::White), Bitboard::from(Square::E4));
        assert_eq!(position.bb_piece(Piece::WHITE_QUEEN), Square::E4.into());

        position.put_piece(Piece::BLACK_KNIGHT, Square::H8);
        assert_eq!(position[Square::H8], Some(Piece::BLACK_KNIGHT));
        assert_eq!(position.bb_color(Color::Black), Bitboard::from(Square::H8));
        assert_eq!(position.bb_piece(Piece::BLACK_KNIGHT), Square::H8.into());
    }

    #[test]
    fn test_remove_piece() {
        let mut position = Position::default();
        position.put_piece(Piece::BLACK_PAWN, Square::B5);

        position.remove_piece(Square::B5);

        assert_eq!(position[Square::B5], None);
        assert_eq!(position.bb_color(Color::Black), Bitboard::EMPTY);
        assert_eq!(position.bb_piece(Piece::BLACK_PAWN), Bitboard::EMPTY);
    }

    #[test]
    fn test_move_piece() {
        let mut position = Position::default();
        position.put_piece(Piece::WHITE_PAWN, Square::A2);

        position.move_piece(Piece::WHITE_PAWN, Square::A2, Square::A3);

        assert_eq!(position[Square::A2], None);
        assert_eq!(position[Square::A3], Some(Piece::WHITE_PAWN));
        assert_eq!(position.bb_color(Color::White), Square::A3.into());
        assert_eq!(position.bb_piece(Piece::WHITE_PAWN), Square::A3.into());
    }
}
