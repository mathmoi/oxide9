use std::{mem::MaybeUninit, ops::Index};

use crate::{
    eval::{get_piece_square_value, get_piece_type_game_phase, EvalPair},
    zobrist::{zobrist_black_to_move, zobrist_castling, zobrist_en_passant, zobrist_piece_square, Zobrist},
};

use super::{
    bitboard::Bitboard,
    coordinates::{File, Rank, Square},
    move_gen::attacks::{attacks_from, attacks_from_bishops, attacks_from_pawns, attacks_from_rooks},
    piece::{Color, Piece, PieceType},
    r#move::{CastlingRight, CastlingSide, Move, MoveType},
};

/// Error type for parsing a FEN (Forsyth-Edwards Notation) string.
#[derive(Debug)] // TODO : Should use thiserror
pub enum FenError {
    InvalidPiecePlacement,
    InvalidActiveColor,
    InvalidCastlingAvailability,
    InvalidEnPassantSquare,
    InvalidHalfmoveClock,
    InvalidFullmoveNumber,
    MissingField,
}

//======================================================================================================================
// OccupancyFilter implementation (used as input parameter for the occupied method of the Position struct)
//======================================================================================================================

/// Defines filtering criteria for retrieving occupied squares from a chess position.
///
/// This enum provides different ways to filter which occupied squares should be included when querying a position's
/// occupancy. It allows for retrieving all occupied squares or filtering by various combinations of piece
/// characteristics.
///
/// # Variants
/// * `All` - Selects all occupied squares regardless of the pieces on them
/// * `ByColor(Color)` - Selects only squares occupied by pieces of the specified color (e.g., all white pieces)
/// * `ByType(PieceType)` - Selects only squares occupied by pieces of the specified type (e.g., all knights, regardless
///   of color)
/// * `ByPiece(Piece)` - Selects only squares occupied by the specific piece (a combination of both color and type,
///   e.g., white queen)
/// * `ByColorAndType(Color, PieceType)` - Functionally equivalent to `ByPiece` but constructed from separate color and
///   type parameters instead of a pre-constructed Piece (e.g., white knights)
/// * `ByColorAndTwoTypes(Color, PieceType, PieceType)` - Selects squares occupied by pieces of the specified color that
///   match either of the two piece types
///
/// # Usage Context
/// This enum is typically passed to a position's `occupied()` method to filter which squares should be included in the
/// returned bitboard. The filtering happens at query time rather than requiring separate bitboards to be maintained for
/// each possible filter combination.
///
/// These filters enable efficient and expressive queries about piece locations on the board, supporting common chess
/// programming patterns like finding all attacking pieces of a certain type or evaluating mobility of specific piece
/// combinations.
pub enum OccupancyFilter {
    All,
    ByColor(Color),
    ByType(PieceType),
    ByPiece(Piece),
    ByColorAndType(Color, PieceType),
    ByColorAndTwoTypes(Color, PieceType, PieceType),
}

impl From<Color> for OccupancyFilter {
    fn from(color: Color) -> Self {
        Self::ByColor(color)
    }
}

impl From<PieceType> for OccupancyFilter {
    fn from(piece_type: PieceType) -> Self {
        Self::ByType(piece_type)
    }
}

impl From<Piece> for OccupancyFilter {
    fn from(piece: Piece) -> Self {
        Self::ByPiece(piece)
    }
}

impl From<(Color, PieceType)> for OccupancyFilter {
    fn from((color, piece_type): (Color, PieceType)) -> Self {
        Self::ByColorAndType(color, piece_type)
    }
}

impl From<(Color, PieceType, PieceType)> for OccupancyFilter {
    fn from((color, type1, type2): (Color, PieceType, PieceType)) -> Self {
        Self::ByColorAndTwoTypes(color, type1, type2)
    }
}

//======================================================================================================================
// Game State implementation
//======================================================================================================================

#[derive(Clone, Copy)]
pub struct GameState {
    side_to_move: Color,
    castling_rights: CastlingRight,
    en_passant_square: Option<Square>,
    halfmove_clock: u16,
    fullmove_number: u16,
    last_move: Option<Move>,
    side_to_move_blockers: Bitboard,
    psqt_eval: EvalPair,
    game_phase: u8,
    zobrist: Zobrist,
}

/// Default implementation for the GameState struct.
impl Default for GameState {
    fn default() -> Self {
        Self {
            side_to_move: Color::White,
            castling_rights: CastlingRight::empty(),
            en_passant_square: None,
            halfmove_clock: 0,
            fullmove_number: 1,
            last_move: None,
            side_to_move_blockers: Bitboard::EMPTY,
            psqt_eval: EvalPair::default(),
            game_phase: 0,
            zobrist: 0,
        }
    }
}

//======================================================================================================================
// History implementation
//======================================================================================================================

/// A stack-like data structure that maintains a history of previous game states.
///
/// This structure efficiently stores game states from previous moves to enable move undoing and position repetition
/// detection. It uses a fixed-size array to avoid heap allocations during gameplay.
///
/// # Capacity
/// The history can store up to 2048 game states, which is enough for even the longest practical chess games.
///
/// # Safety
/// Does not perform bounds checking in release builds for performance reasons.
#[derive(Clone)]
pub struct History {
    pub states: [GameState; 2048],
    pub len: usize,
}

impl Default for History {
    /// Creates a new empty history with uninitialized state storage.
    ///
    /// # Returns
    /// A History with count set to 0 and an uninitialized array of game states.
    ///
    /// # Safety
    /// Uses unsafe code to avoid initializing the entire array for performance reasons. The count field ensures we only
    /// access initialized elements.
    fn default() -> Self {
        Self {
            states: unsafe {
                let block = MaybeUninit::uninit();
                block.assume_init()
            },
            len: 0,
        }
    }
}

impl History {
    /// Adds a game state to the history.
    ///
    /// # Parameters
    /// * `state` - The game state to add to the history
    ///
    /// # Panics
    /// In debug builds, panics if the history is full (count >= 2048). In release builds, this check is omitted for
    /// performance.
    pub fn push(&mut self, state: GameState) {
        debug_assert!(self.len < self.states.len());

        self.states[self.len] = state;
        self.len += 1;
    }

    /// Removes and returns the most recent game state from the history.
    ///
    /// # Returns
    /// The most recently added game state.
    ///
    /// # Panics
    /// In debug builds, panics if the history is empty (count == 0). In release builds, this check is omitted for
    /// performance.
    pub fn pop(&mut self) -> GameState {
        debug_assert!(self.len > 0);

        self.len -= 1;
        self.states[self.len]
    }

    /// Returns the current length of the history.
    pub fn len(&self) -> usize {
        self.len
    }
}

impl Index<usize> for History {
    type Output = GameState;

    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(index < self.len);
        &self.states[index]
    }
}

//======================================================================================================================
// Position implementation
//======================================================================================================================

/// A chess position.
#[derive(Clone)]
pub struct Position {
    board: [Option<Piece>; Square::COUNT],
    bb_color: [Bitboard; Color::COUNT],
    bb_piece: [Bitboard; Piece::COUNT],
    castling_rook_file: [File; CastlingSide::COUNT],
    castling_path: [Bitboard; CastlingSide::COUNT],
    castling_rights_mask: [CastlingRight; Square::COUNT],
    state: GameState,
    history: History,
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
                file = file.ok_or(FenError::InvalidPiecePlacement)?.right(number as i8).ok();
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
        self.set_side_to_move(match active_color {
            "w" => Color::White,
            "b" => Color::Black,
            _ => return Err(FenError::InvalidActiveColor),
        });
        Ok(())
    }

    fn read_castling(&mut self, castling_availability: &str) -> Result<(), FenError> {
        let mut file_set: [Option<File>; CastlingSide::COUNT] = [None; CastlingSide::COUNT];

        for c in castling_availability.chars() {
            let color;
            let king_file;
            let king_to_file;
            let rook_to_file;
            let castling_side;

            match c {
                'K' | 'Q' | 'k' | 'q' | 'A'..='H' | 'a'..='h' => {
                    color = if c.is_uppercase() { Color::White } else { Color::Black };
                    king_file = self.king_square(color).file();
                }
                '-' => break,
                _ => return Err(FenError::InvalidCastlingAvailability),
            }

            let castling_file = match c {
                'K' | 'k' => (self.occupied((color, PieceType::Rook))
                    & Bitboard::from(Rank::R1.relative_to_color(color)))
                .msb()
                .ok_or(FenError::InvalidCastlingAvailability)?
                .file(),
                'Q' | 'q' => (self.occupied((color, PieceType::Rook))
                    & Bitboard::from(Rank::R1.relative_to_color(color)))
                .lsb()
                .ok_or(FenError::InvalidCastlingAvailability)?
                .file(),
                'A'..='H' | 'a'..='h' => File::try_from(c).map_err(|_| FenError::InvalidCastlingAvailability)?,
                _ => unreachable!(),
            };

            if castling_file < king_file {
                castling_side = CastlingSide::Queenside;
                king_to_file = File::C;
                rook_to_file = File::D;
            } else {
                castling_side = CastlingSide::Kingside;
                king_to_file = File::G;
                rook_to_file = File::F;
            };

            // Checked if the file was already set and is different.
            if let Some(file) = file_set[usize::from(castling_side)] {
                if file != castling_file {
                    return Err(FenError::InvalidCastlingAvailability);
                }
            }

            let right = CastlingRight::new(color, castling_side);
            self.state.castling_rights |= right;
            self.castling_rook_file[usize::from(castling_side)] = castling_file;

            // Compute the mask of the path of the king and rook involved in castling.
            let king_from = Square::new(king_file, Rank::R1);
            let king_to = Square::new(king_to_file, Rank::R1);
            let rook_from = Square::new(castling_file, Rank::R1);
            let rook_to = Square::new(rook_to_file, Rank::R1);
            let mut mask = Bitboard::between(king_from, king_to) | Bitboard::between(rook_from, rook_to);
            mask &= !(king_from | rook_from);
            mask |= mask << 56;
            self.castling_path[usize::from(castling_side)] = mask;

            // Update the castling rights mask for the squares involved in castling.
            let rank = Rank::R1.relative_to_color(color);
            self.castling_rights_mask[usize::from(Square::new(king_file, rank))] |= right;
            self.castling_rights_mask[usize::from(Square::new(castling_file, rank))] |= right;

            file_set[usize::from(castling_side)] = Some(castling_file);
        }

        self.state.zobrist ^= zobrist_castling(self.state.castling_rights);

        Ok(())
    }

    fn read_en_passant_square(&mut self, en_passant_square: &str) -> Result<(), FenError> {
        match en_passant_square {
            "-" => {}
            _ => self.set_en_passant(Some(
                Square::try_from(en_passant_square).map_err(|_| FenError::InvalidEnPassantSquare)?,
            )),
        };
        Ok(())
    }

    /// Creates a new chess position from a FEN (Forsyth-Edwards Notation) string.
    ///
    /// A FEN string contains 6 fields separated by spaces:
    ///
    /// 1. Piece placement: Each rank is described from 8 to 1, separated by '/'. Letters represent pieces (P=pawn,
    ///    N=knight, B=bishop, R=rook, Q=queen, K=king). Uppercase is white, lowercase is black. Numbers represent empty
    ///    squares. Example: "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"
    ///
    /// 2. Active color: "w" means White moves next, "b" means Black moves next.
    ///
    /// 3. Castling availability: Combination of "K"(white kingside), "Q"(white queenside), "k"(black kingside),
    ///    "q"(black queenside), or "-" if no castling is possible.
    ///
    /// 4. En passant target square: The square where a pawn can be captured en passant, in algebraic notation (e.g.,
    ///    "e3"), or "-" if not available.
    ///
    /// 5. Halfmove clock: Number of halfmoves since the last pawn advance or piece capture. Used for the fifty-move
    ///    rule.
    ///
    /// 6. Fullmove number: The number of complete moves. Starts at 1 and increments after Black's move.
    ///
    /// # Arguments
    ///
    /// * fen - A string containing the FEN representation of a chess position. FEN is a standard notation to describe a
    ///         particular board position of a chess game.
    ///
    /// # See also
    /// [The PGN
    /// specifications](https://ia902908.us.archive.org/26/items/pgn-standard-1994-03-12/PGN_standard_1994-03-12.txt)
    /// that defines the FEN format at section 16.1.
    pub fn new_from_fen(fen: &str) -> Result<Self, FenError> {
        let mut position = Position::default();

        let mut fields = fen.split_whitespace();
        position.read_piece_placement(fields.next().ok_or(FenError::MissingField)?)?;
        position.read_active_color(fields.next().ok_or(FenError::MissingField)?)?;
        position.read_castling(fields.next().unwrap_or("-"))?;
        position.read_en_passant_square(fields.next().unwrap_or("-"))?;

        position.state.halfmove_clock =
            fields.next().unwrap_or("0").parse().map_err(|_| FenError::InvalidHalfmoveClock)?;

        position.state.fullmove_number =
            fields.next().unwrap_or("1").parse().map_err(|_| FenError::InvalidFullmoveNumber)?;

        position.state.side_to_move_blockers = position.blockers(position.side_to_move());

        Ok(position)
    }

    /// Creates a new chess position with the standard initial board setup.
    ///
    /// This constructor initializes the chess board to the standard starting position, with all pieces in their
    /// traditional starting squares, white to move, and full castling rights available.
    ///
    /// # Returns
    /// A new Position instance configured with the standard chess starting position
    /// (RNBQKBNR/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1).
    ///
    /// # Implementation Details
    /// The function uses Forsyth-Edwards Notation (FEN) parsing internally but is guaranteed to never fail since it
    /// uses the valid standard starting position string. This is more convenient than `new_from_fen()` when you
    /// specifically want the standard chess starting position.
    pub fn new() -> Self {
        const INITIAL_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";
        Self::new_from_fen(INITIAL_POSITION)
            .expect("This can not fail because the INITIAL_POSITION fen will always be successfully parsed.")
    }

    /// Returns the FEN (Forsyth-Edwards Notation) representation of the position.
    fn write_piece_placement(&self) -> String {
        let mut result = String::with_capacity(70);
        for rank in Rank::ALL.iter().rev() {
            let mut empty_count = 0;
            for file in File::ALL {
                let square = Square::new(file, *rank);
                if let Some(piece) = self[square] {
                    if empty_count > 0 {
                        result.push_str(&empty_count.to_string());
                        empty_count = 0;
                    }
                    result.push(piece.into());
                } else {
                    empty_count += 1;
                }
            }
            if empty_count > 0 {
                result.push_str(&empty_count.to_string());
            }
            if rank != &Rank::R1 {
                result.push('/');
            }
        }
        result
    }

    fn write_castling(&self) -> String {
        if self.castling_availability().is_empty() {
            return String::from("-");
        }

        let mut result = String::with_capacity(4);
        for color in Color::ALL {
            for side in CastlingSide::ALL {
                let right = CastlingRight::new(color, side);
                if !self.castling_availability().contains(right) {
                    continue;
                }

                let candidate_rooks =
                    self.occupied((color, PieceType::Rook)) & Bitboard::from(Rank::R1.relative_to_color(color));
                let outter_most_rook =
                    if side == CastlingSide::Queenside { candidate_rooks.lsb() } else { candidate_rooks.msb() }
                        .expect("There should be a candidate rook for castling.");

                let castling_char = if self.castling_file(side) == outter_most_rook.file() {
                    match side {
                        CastlingSide::Queenside => 'q',
                        CastlingSide::Kingside => 'k',
                    }
                } else {
                    self.castling_file(side).into()
                };

                result.push(if color == Color::White { castling_char.to_ascii_uppercase() } else { castling_char });
            }
        }
        result
    }

    fn write_en_passant(&self) -> String {
        // We only add the en passant square if there is a pawn that can capture it.
        if let Some(en_passant_sq) = self.en_passant_square() {
            let pawn = Piece::new(self.side_to_move(), PieceType::Pawn);
            let direction = match self.side_to_move() {
                Color::White => 1,
                Color::Black => -1,
            };

            if en_passant_sq
                .down(direction)
                .and_then(|sq| sq.left(1))
                .is_ok_and(|sq| self[sq].is_some_and(|piece| piece == pawn))
                | en_passant_sq
                    .down(direction)
                    .and_then(|sq| sq.right(1))
                    .is_ok_and(|sq| self[sq].is_some_and(|piece| piece == pawn))
            {
                return format!("{}", en_passant_sq);
            }
        }
        String::from("-")
    }

    /// Returns the FEN (Forsyth-Edwards Notation) representation of the position.
    pub fn to_fen(&self) -> String {
        format!(
            "{} {} {} {} {} {}",
            self.write_piece_placement(),
            char::from(self.side_to_move()),
            self.write_castling(),
            self.write_en_passant(),
            self.state.halfmove_clock,
            self.state.fullmove_number
        )
    }

    /// Generates a compact string representation of the current chess position.
    ///
    /// Creates a human-readable text visualization of the board with rank numbers on the left edge and file letters on
    /// the bottom. The board is displayed from white's perspective (rank 1 at bottom, rank 8 at top).
    ///
    /// # Returns
    /// A formatted string representing the board where:
    /// - Rank numbers (1-8) are shown on the left edge
    /// - File letters (a-h) are shown on the bottom edge
    /// - Pieces are represented by their standard characters (P, N, B, R, Q, K for white; p, n, b, r, q, k for black)
    /// - Empty squares are represented by dots (.)
    ///
    /// # Example Output
    ///
    /// 8  r n b q k b n r
    /// 7  p p p p p p p p
    /// 6  . . . . . . . .
    /// 5  . . . . . . . .
    /// 4  . . . . . . . .
    /// 3  . . . . . . . .
    /// 2  P P P P P P P P
    /// 1  R N B Q K B N R
    ///    a b c d e f g h
    pub fn to_compact_string(&self) -> String {
        let mut board = String::with_capacity(171);
        for rank in Rank::ALL.iter().rev() {
            board.push_str(&format!("{}  ", rank));
            for file in File::ALL {
                let sq = Square::new(file, *rank);
                match self[sq] {
                    Some(piece) => board.push(piece.into()),
                    None => board.push('.'),
                }
                if file != File::H {
                    board.push(' ');
                } else {
                    board.push('\n');
                }
            }
        }
        board.push_str("   a b c d e f g h");

        board
    }

    pub fn to_string(&self) -> String {
        let mut board = String::with_capacity(2000);

        // Top border
        board.push_str(self.generate_border_string(Color::Black, '┌', '┐', '~', '┬').as_str());

        // Add all the ranks
        for rank in Rank::ALL.iter().rev() {
            board.push(char::from(*rank));
            board.push_str(" │");
            for file in File::ALL {
                let square = Square::new(file, *rank);
                if let Some(piece) = self[square] {
                    board.push(if piece.color() == Color::Black { '░' } else { '▌' });
                    board.push(piece.piece_type().into());
                    board.push(if piece.color() == Color::Black { '░' } else { '▐' });
                } else {
                    board.push(' ');
                    board.push(if u8::from(*rank) % 2 == u8::from(file) % 2 { '.' } else { ' ' });
                    board.push(' ');
                }
                if file < File::H {
                    board.push('│');
                }
            }
            board.push_str("│\n");

            if Rank::R1 < *rank {
                board.push_str("  ├───┼───┼───┼───┼───┼───┼───┼───┤\n");
            }
        }

        // Bottom border
        board.push_str(self.generate_border_string(Color::White, '└', '┘', '~', '┴').as_str());

        // En passant indicator
        if let Some(en_passant_square) = self.en_passant_square() {
            board.push_str(" ".repeat(4 + usize::from(en_passant_square.file()) * 4).as_str());
            board.push_str("▲\n");
        }

        // file letters
        board.push_str("    a   b   c   d   e   f   g   h");

        board
    }

    fn generate_border_string(
        &self,
        color: Color,
        left_corner: char,
        right_corner: char,
        castling_indicator: char,
        column_separator: char,
    ) -> String {
        let mut line = String::with_capacity(40);

        line.push_str(if self.side_to_move() == color { "=>" } else { "  " });
        line.push(left_corner);

        let queenside_castling_file = self.castling_file(CastlingSide::Queenside);
        let kingside_castling_file = self.castling_file(CastlingSide::Kingside);
        let can_castle_queenside =
            self.castling_availability().contains(CastlingRight::new(color, CastlingSide::Queenside));
        let can_castle_kingside =
            self.castling_availability().contains(CastlingRight::new(color, CastlingSide::Kingside));
        for file in File::ALL {
            line.push('─');
            if (can_castle_queenside && file == queenside_castling_file)
                || (can_castle_kingside && file == kingside_castling_file)
            {
                line.push(castling_indicator);
            } else {
                line.push('─');
            }
            line.push('─');

            if file < File::H {
                line.push(column_separator);
            }
        }
        line.push(right_corner);
        line.push('\n');

        line
    }

    /// Returns a bitboard of squares occupied by pieces matching the specified filter.
    ///
    /// This method retrieves a bitboard representing all squares that contain pieces matching the provided filter
    /// criteria. The filter determines which pieces are included based on their color, type, or specific identity.
    ///
    /// # Parameters
    /// * `filter`: Criteria for which pieces to include in the returned bitboard. Any type that can be converted into
    ///   an `OccupancyFilter` is accepted.
    ///
    /// # Returns
    /// * A bitboard with 1-bits in positions where matching pieces are located.
    ///
    /// # Performance Note
    /// This method is always inlined to ensure the match on filter types is optimized away at compile time, eliminating
    /// any runtime overhead from the filter selection.
    ///
    /// # Example Usage Contexts
    /// - Get all occupied squares with `position.occupied(OccupancyFilter::All)`
    /// - Get only white pieces with `position.occupied(Color::White)`
    /// - Get all knights with `position.occupied(PieceType::Knight)`
    /// - Get black queens with `position.occupied(Piece::BlackQueen)`
    ///
    /// # Related
    /// See `OccupancyFilter` for the full set of filtering options available.
    #[inline(always)]
    pub fn occupied<F: Into<OccupancyFilter>>(&self, filter: F) -> Bitboard {
        match filter.into() {
            OccupancyFilter::All => self.bb_color[usize::from(Color::White)] | self.bb_color[usize::from(Color::Black)],

            OccupancyFilter::ByColor(color) => self.bb_color[usize::from(color)],

            OccupancyFilter::ByType(piece_type) => {
                self.bb_piece[usize::from(Piece::new(Color::White, piece_type))]
                    | self.bb_piece[usize::from(Piece::new(Color::Black, piece_type))]
            }

            OccupancyFilter::ByPiece(piece) => self.bb_piece[usize::from(piece)],

            OccupancyFilter::ByColorAndType(color, piece_type) => {
                self.bb_piece[usize::from(Piece::new(color, piece_type))]
            }

            OccupancyFilter::ByColorAndTwoTypes(color, type1, type2) => {
                self.bb_piece[usize::from(Piece::new(color, type1))]
                    | self.bb_piece[usize::from(Piece::new(color, type2))]
            }
        }
    }

    fn set_side_to_move(&mut self, color: Color) {
        if self.state.side_to_move != color {
            self.state.zobrist ^= zobrist_black_to_move();
            self.state.side_to_move = color;
        }
    }

    fn switch_side_to_move(&mut self) {
        self.state.zobrist ^= zobrist_black_to_move();
        self.state.side_to_move = !self.state.side_to_move;
    }

    /// Returns the color of the side to move.
    pub fn side_to_move(&self) -> Color {
        self.state.side_to_move
    }

    /// Returns the castling availability of the position.
    pub fn castling_availability(&self) -> CastlingRight {
        self.state.castling_rights
    }

    /// Returns the en passant square of the position.
    pub fn en_passant_square(&self) -> Option<Square> {
        self.state.en_passant_square
    }

    /// Returns the file of the rook involved in castling on a specific side.
    pub fn castling_file(&self, side: CastlingSide) -> File {
        self.castling_rook_file[usize::from(side)]
    }

    /// Returns the path of the king and rook involved in castling on a specific side.
    pub fn castling_path(&self, side: CastlingSide) -> Bitboard {
        self.castling_path[usize::from(side)]
    }

    /// Returns the square occupied by the king of the specified color.
    ///
    /// # Parameters
    /// * `color` - The color of the king to locate
    ///
    /// # Returns
    /// The square where the king of the specified color is located.
    ///
    /// # Panics
    /// Panics if no king of the specified color is found on the board, which should never happen in a valid chess
    /// position.
    pub fn king_square(&self, color: Color) -> Square {
        // TODO: Should we keep this incrementally updated?
        self.occupied((color, PieceType::King)).lsb().expect("There should always be a king on the board.")
    }

    /// Returns the last move that led to the current position.
    ///
    /// Provides access to the move that was most recently executed on the board, which resulted in the current
    /// position.
    ///
    /// # Returns
    /// * `Some(Move)` - The most recent move if one exists
    /// * `None` - If no moves have been made yet or the position was set directly
    pub fn last_move(&self) -> Option<Move> {
        self.state.last_move
    }

    /// Returns the incrementally updated portion of the position evaluation, adjusted for the side to move.
    ///
    /// # Purpose
    /// Retrieves the current value of the incrementally maintained evaluation score from the perspective
    /// of the side to move, which primarily consists of piece-square table values.
    ///
    /// # Returns
    /// An `EvalPair` containing both middlegame and endgame evaluation components that have been incrementally updated
    /// throughout the game, with the sign adjusted to reflect the advantage of the side to move.
    ///
    /// # Important
    /// - This provides only the material and piece positioning component of evaluation
    /// - Does not include dynamic evaluation factors that must be calculated on demand
    /// - This value is efficiently maintained through incremental updates during moves
    /// - The value is automatically signed relative to the side to move (positive values favor the side to move)
    /// - Typically used as a base value for full position evaluation
    pub fn incremental_eval(&self) -> EvalPair {
        match self.side_to_move() {
            Color::White => self.state.psqt_eval,
            Color::Black => -self.state.psqt_eval,
        }
    }

    /// Returns the current game phase value of the position.
    ///
    /// # Purpose
    /// Retrieves the computed game phase value for the current board position. This value indicates how far the game
    /// has progressed from opening to endgame.
    ///
    /// # Returns
    /// A `u8` value representing the current game phase.
    ///
    /// # Important
    /// - The value is incrementally updated as pieces are added or removed from the board
    /// - Higher values typically indicate an earlier game phase (more pieces on board)
    /// - Lower values indicate progression toward endgame (fewer pieces on board)
    /// - Used to determine the blend between middlegame and endgame evaluation
    pub fn game_phase(&self) -> u8 {
        self.state.game_phase
    }

    /// Returns the current Zobrist hash of the position.
    pub fn hash(&self) -> Zobrist {
        self.state.zobrist
    }

    /// Places a chess piece on a specific square on the board.
    ///
    /// # Purpose
    /// Updates both the board representation and the game state by placing the specified piece on the given square.
    ///
    /// # Parameters
    /// * `piece` - The chess piece to place on the board
    /// * `square` - The target square where the piece will be placed
    ///
    /// # Important
    /// - The target square must be empty before placing the piece
    /// - This method properly updates all relevant game state information
    /// - For low-level placement without full state updates, use `put_piece_only` instead
    pub fn put_piece(&mut self, piece: Piece, square: Square) {
        self.put_piece_only(piece, square);

        self.state.psqt_eval += get_piece_square_value(piece, square);
        self.state.game_phase += get_piece_type_game_phase(piece);
        self.state.zobrist ^= zobrist_piece_square(piece, square);
    }

    /// Places a chess piece on a specific square on the board without updating the game state.
    ///
    /// # Purpose
    /// This is a low-level method that only updates the board representation but not the full game state. It should be
    /// used in contexts where the game state will be properly restored later (typically in unmake() operations).
    ///
    /// # Parameters
    /// * `piece` - The chess piece to place on the board
    /// * `square` - The target square where the piece will be placed
    ///
    /// # Important
    /// - The target square must be empty before placing the piece (checked with debug_assert)
    /// - After using this method, the game state must be eventually restored to maintain position consistency
    /// - For normal piece placement with full state updates, use `put_piece` instead
    fn put_piece_only(&mut self, piece: Piece, square: Square) {
        debug_assert_eq!(self.board[usize::from(square)], None);

        self.board[usize::from(square)] = Some(piece);
        self.bb_color[usize::from(piece.color())] |= square;
        self.bb_piece[usize::from(piece)] |= Bitboard::from(square);
    }

    /// Removes a piece from a specific square.
    ///
    /// # Purpose
    /// Updates both the board representation and the game state by removing the piece from the given square.
    ///
    /// # Parameters
    /// * `square` - The square from which to remove the piece
    ///
    /// # Important
    /// - The square must contain a piece (panics if the square is empty)
    /// - This method properly updates all relevant game state information
    /// - For low-level removal without full state updates, use `remove_piece_only` instead
    fn remove_piece(&mut self, square: Square) {
        let piece = self.board[usize::from(square)].expect("There should be a piece to remove");

        self.remove_piece_only(piece, square);

        self.state.psqt_eval -= get_piece_square_value(piece, square);
        self.state.game_phase -= get_piece_type_game_phase(piece);
        self.state.zobrist ^= zobrist_piece_square(piece, square);
    }

    /// Removes a piece from a specific square without updating the game state.
    ///
    /// # Purpose
    /// This is a low-level method that only updates the board representation but not the full game state. It should be
    /// used in contexts where the game state will be properly restored later.
    ///
    /// # Parameters
    /// * `square` - The square from which to remove the piece
    ///
    /// # Important
    /// - The square must contain a piece (panics with an error message if the square is empty)
    /// - After using this method, the game state must be eventually restored to maintain position consistency
    /// - This method only updates the board representation and bitboards, but not the complete game state
    fn remove_piece_only(&mut self, piece: Piece, square: Square) {
        debug_assert_eq!(self.board[usize::from(square)], Some(piece));

        self.board[usize::from(square)] = None;
        self.bb_color[usize::from(piece.color())] ^= Bitboard::from(square);
        self.bb_piece[usize::from(piece)] ^= Bitboard::from(square);
    }

    /// Moves a known chess piece from one square to another.
    ///
    /// # Purpose
    /// Updates both the board representation and the game state by moving the specified piece from the source square to
    /// the destination square.
    ///
    /// # Parameters
    /// * `piece` - The chess piece to be moved
    /// * `from` - The source square where the piece is currently located
    /// * `to` - The destination square where the piece will be moved
    ///
    /// # Important
    /// - The `from` square must contain the specified piece
    /// - The `to` square must be empty
    /// - This method properly updates all relevant game state information
    /// - For low-level movement without full state updates, use `move_piece_only` instead
    fn move_piece(&mut self, piece: Piece, from: Square, to: Square) {
        self.move_piece_only(piece, from, to);

        self.state.psqt_eval += get_piece_square_value(piece, to) - get_piece_square_value(piece, from);
        self.state.zobrist ^= zobrist_piece_square(piece, from) ^ zobrist_piece_square(piece, to);
    }

    /// Moves a known chess piece from one square to another without updating the game state.
    ///
    /// # Purpose
    /// This is a low-level method that only updates the board representation but not the full game state. It should be
    /// used in contexts where the game state will be properly restored later.
    ///
    /// # Parameters
    /// * `piece` - The chess piece to be moved
    /// * `from` - The source square where the piece is currently located
    /// * `to` - The destination square where the piece will be moved
    ///
    /// # Important
    /// - The piece specified must match the piece actually present on the `from` square (checked with debug_assert)
    /// - The `to` square must be empty (checked with debug_assert)
    /// - After using this method, the game state must be eventually restored to maintain position consistency
    /// - This method efficiently updates both board and bitboard representations in a single operation
    /// - For normal piece movement with full state updates, use `move_piece` instead
    fn move_piece_only(&mut self, piece: Piece, from: Square, to: Square) {
        debug_assert_eq!(self.board[usize::from(from)], Some(piece));
        debug_assert_eq!(self.board[usize::from(to)], None);

        self.board[usize::from(from)] = None;
        self.board[usize::from(to)] = Some(piece);
        let bb = from | to;
        self.bb_color[usize::from(piece.color())] ^= bb;
        self.bb_piece[usize::from(piece)] ^= bb;
    }

    /// Sets the en passant square for the position.
    fn set_en_passant(&mut self, square: Option<Square>) {
        self.state.zobrist ^= zobrist_en_passant(self.state.en_passant_square);
        self.state.en_passant_square = square;
        self.state.zobrist ^= zobrist_en_passant(square);
    }

    /// Returns a bitboard representing all pieces of a specific color that are attacking a specific square.
    ///
    /// This function identifies all pieces of the given color that are currently attacking the specified square. It
    /// works by calculating reverse attack patterns from the target square and checking which pieces of the specified
    /// color intersect with these patterns.
    ///
    /// # Parameters
    /// * `sq` - The target square being attacked
    /// * `occupied` - A bitboard representing occupied squares to consider for attack calculations (can be modified
    ///   from the actual board state to analyze hypothetical positions)
    /// * `color` - The color of the attacking pieces to consider
    ///
    /// # Returns
    /// A bitboard containing the positions of all pieces of the specified color that are attacking the given square.
    ///
    /// # Note
    /// The occupied parameter affects only sliding piece (rook, bishop, queen) attack calculations; knight, king and
    /// pawn attacks are determined solely by their geometry.
    pub fn attacks_to(&self, sq: Square, occupied: Bitboard, color: Color) -> Bitboard {
        let queens_rooks = self.occupied((color, PieceType::Rook, PieceType::Queen));
        let queens_bishops = self.occupied((color, PieceType::Bishop, PieceType::Queen));
        let knights = self.occupied((color, PieceType::Knight));
        let king = self.occupied((color, PieceType::King));
        let pawns = self.occupied((color, PieceType::Pawn));

        attacks_from::<{ PieceType::ROOK_VALUE }>(occupied, sq) & queens_rooks
            | attacks_from::<{ PieceType::BISHOP_VALUE }>(occupied, sq) & queens_bishops
            | attacks_from::<{ PieceType::KNIGHT_VALUE }>(Bitboard::EMPTY, sq) & knights
            | attacks_from::<{ PieceType::KING_VALUE }>(Bitboard::EMPTY, sq) & king
            | attacks_from_pawns(color, sq) & pawns
    }

    /// Determines whether a square on the chess board is under attack by pieces of a specific color.
    ///
    /// This function checks if any piece of the specified color attacks a pieces the target square according to
    /// standard chess rules. It considers all piece types (pawns, knights, bishops, rooks, queens, and kings) when
    /// determining attack vectors.
    ///
    /// # Parameters
    ///
    /// * `sq` - The target square being checked for attacks
    /// * `occupied` - A bitboard representing all occupied squares on the board. This parameter allows for hypothetical
    ///   board states by selectively including/excluding pieces from consideration (useful for checking legality of
    ///   moves like castling or detecting check).
    /// * `color` - The color of the attacking pieces to check
    ///
    /// # Returns
    ///
    /// * `bool` - Returns `true` if any piece of the specified color attacks the target square, `false` otherwise
    pub fn is_attacked(&self, sq: Square, occupied: Bitboard, color: Color) -> bool {
        let queens_rooks = self.occupied((color, PieceType::Rook, PieceType::Queen));
        if (attacks_from_rooks(sq) & queens_rooks).has_any()
            && (attacks_from::<{ PieceType::ROOK_VALUE }>(occupied, sq) & queens_rooks).has_any()
        {
            return true;
        }

        let queens_bishops = self.occupied((color, PieceType::Bishop, PieceType::Queen));
        if (attacks_from_bishops(sq) & queens_bishops).has_any()
            && (attacks_from::<{ PieceType::BISHOP_VALUE }>(occupied, sq) & queens_bishops).has_any()
        {
            return true;
        }

        let knights = self.occupied((color, PieceType::Knight));
        if (attacks_from::<{ PieceType::KNIGHT_VALUE }>(Bitboard::EMPTY, sq) & knights).has_any() {
            return true;
        }

        let king = self.occupied((color, PieceType::King));
        if (attacks_from::<{ PieceType::KING_VALUE }>(Bitboard::EMPTY, sq) & king).has_any() {
            return true;
        }

        let pawns = self.occupied((color, PieceType::Pawn));
        if (attacks_from_pawns(color, sq) & pawns).has_any() {
            return true;
        }

        false
    }

    /// Determines if the current side to move is in check.
    ///
    /// This function checks if the king of the current active player is under attack by any enemy pieces, which
    /// constitutes a check in chess.
    ///
    /// # Returns
    ///
    /// * `bool` - Returns `true` if the current side to move is in check, `false` otherwise
    ///
    /// # Panics
    ///
    /// Panics if the king of the side to move is not present on the board, which indicates an invalid board state. In
    /// standard chess, both kings must always be present.
    pub fn is_check(&self) -> bool {
        self.is_attacked(
            self.king_square(self.side_to_move()),
            self.occupied(OccupancyFilter::All),
            !self.side_to_move(),
        )
    }

    /// Determines if the current position is a draw.
    ///
    /// Checks for draws based on the 50-move rule and position repetition:
    ///
    /// - 50-move rule: Returns true if 50 moves (100 half-moves) have been made without a pawn move or a capture.
    ///
    /// - Position repetition: Returns true if the current position has appeared at least once before in the game
    ///   history (this detects repetitions earlier than the traditional threefold repetition rule to optimize search).
    ///
    /// This implementation only checks positions with the same side to move by stepping through the history in
    /// increments of 2.
    pub fn is_draw(&self) -> bool {
        // 50 moves rule
        if self.state.halfmove_clock >= 100 {
            return true;
        }

        // Threefold repetition. We actually return true if the position is repeated twice, this way repetition are
        // detected sooner during the search. There is no benefit to wait for the third repetition.
        let last = self.history.len();
        let first = last - ((self.state.halfmove_clock as usize) & !1);
        for index in (first..last).step_by(2).rev() {
            if self.history[index].zobrist == self.state.zobrist {
                return true;
            }
        }

        false
    }

    /// Returns a bitboard of all enemy pieces that are currently checking the king of the side to move.
    ///
    /// This function identifies all opposing pieces that are delivering check to the current player's king. It works by
    /// finding the position of the king for the side to move and then determining which enemy pieces are attacking that
    /// square.
    ///
    /// # Returns
    /// A bitboard containing the positions of all enemy pieces that are checking the king of the side to move.
    ///
    /// # Panics
    /// Panics if the king of the side to move is not found on the board, which should never happen in a valid chess
    /// position.
    ///
    /// # Note
    /// Multiple pieces may be giving check simultaneously (e.g., in a discovered check scenario).
    pub fn checkers(&self) -> Bitboard {
        self.attacks_to(
            self.king_square(self.side_to_move()),
            self.occupied(OccupancyFilter::All),
            !self.side_to_move(),
        )
    }

    /// Returns a bitboard of all pieces that are blocking a check on the king of the specified color.
    ///
    /// Identifies all pieces (of either color) that are currently preventing the king from being in check by standing
    /// between the king and an enemy sliding piece (rook, bishop, or queen). These pieces are "pinned" to the king and
    /// have restricted movement.
    ///
    /// # Parameters
    /// * `color` - The color of the king to analyze blockers for
    ///
    /// # Returns
    /// A bitboard containing all pieces that are currently blocking a check on the specified king.
    ///
    /// # Note
    /// Only considers absolute pins from sliding pieces. A piece is considered a blocker only if it is the single piece
    /// between the king and an attacking sliding piece. Multiple pieces between the king and a sliding piece are not
    /// considered blockers since they don't create a pin.
    fn blockers(&self, color: Color) -> Bitboard {
        let king_sq = self.king_square(color);

        let rooks = self.occupied((!color, PieceType::Rook));
        let bishops = self.occupied((!color, PieceType::Bishop));
        let queens = self.occupied((!color, PieceType::Queen));

        let mut snipers = attacks_from_rooks(king_sq) & (rooks | queens);
        snipers |= attacks_from_bishops(king_sq) & (bishops | queens);
        snipers &= self.occupied(!color);

        let occupancy = self.occupied(OccupancyFilter::All) ^ snipers;

        let mut blockers = Bitboard::EMPTY;
        for sniper_sq in snipers {
            let potential_blockers = Bitboard::between(king_sq, sniper_sq) & occupancy;

            if potential_blockers.has_one() {
                blockers |= potential_blockers;
            }
        }

        blockers
    }

    /// Determines if a pseudo-legal move is actually legal in the current position.
    ///
    /// This function verifies whether a move that follows piece movement rules (pseudo-legal) is actually playable
    /// according to chess rules, primarily by checking if it would leave the king in check.
    ///
    /// # Parameters
    ///
    /// * `mv` - The pseudo-legal move to validate
    ///
    /// # Returns
    ///
    /// * `bool` - Returns `true` if the move is legal, `false` otherwise
    ///
    /// # Important
    ///
    /// This function assumes the provided move is already pseudo-legal (follows basic movement rules for the piece).
    /// Behavior is undefined if called with an invalid or non-pseudo-legal move.
    pub fn is_legal(&self, mv: Move) -> bool {
        // TODO : Add an assert to check if the move is pseudo-legal when we implement that functionality.

        // TODO : Stockfish checs if the traveled square are attacked during castling here instead of during move
        // generation. Should we to that?

        // If it is a king move and the king moves into check the move is illegal.
        if mv.piece().piece_type() == PieceType::King {
            let bb_king = self.occupied(mv.piece());
            return !self.is_attacked(
                mv.to_square(),
                self.occupied(OccupancyFilter::All) ^ bb_king,
                !mv.piece().color(),
            );
        }

        // We have a special case for en passant captures, because it has a special behavior of removig pieces from two
        // squares.
        if mv.move_type() == MoveType::EnPassant {
            let en_passant_capture_sq = Square::new(mv.to_square().file(), mv.from_square().rank());
            let occupied =
                self.occupied(OccupancyFilter::All) ^ mv.to_square() ^ mv.from_square() ^ en_passant_capture_sq;
            let them = !self.side_to_move();
            let queens_rooks = self.occupied((them, PieceType::Rook, PieceType::Queen));
            let queens_bishops = self.occupied((them, PieceType::Bishop, PieceType::Queen));
            let king_sq = self.king_square(mv.piece().color());
            return (attacks_from::<{ PieceType::ROOK_VALUE }>(occupied, king_sq) & queens_rooks).has_none()
                && (attacks_from::<{ PieceType::BISHOP_VALUE }>(occupied, king_sq) & queens_bishops).has_none();
        }

        // If the move is not a king move, the piece must not be pinned or if it is pinned it must move along the pin.
        (self.state.side_to_move_blockers & mv.from_square()).has_none()
            || Square::are_aligned(mv.from_square(), mv.to_square(), self.king_square(self.side_to_move()))
    }

    fn make_basic(&mut self, mv: Move) {
        debug_assert!(self[mv.to_square()].is_none()); // TODO : Should be in a pseudo-legal check

        self.move_piece(mv.piece(), mv.from_square(), mv.to_square());

        if mv.piece().piece_type() == PieceType::Pawn {
            self.state.halfmove_clock = 0;
        } else {
            self.state.halfmove_clock += 1
        };
    }

    fn make_capture(&mut self, mv: Move, capture: Piece) {
        debug_assert!(self[mv.to_square()].is_some_and(|piece| piece == capture)); // TODO : Should be in a pseudo-legal check

        self.remove_piece(mv.to_square());
        self.move_piece(mv.piece(), mv.from_square(), mv.to_square());

        self.state.halfmove_clock = 0;
    }

    fn make_two_square_pawn_push(&mut self, mv: Move) {
        self.move_piece(mv.piece(), mv.from_square(), mv.to_square());

        // If there is a possibility that the pawn can be captured en passant, we set the en passant square.
        let side_to_move = self.side_to_move();
        let en_passant_square = unsafe { mv.to_square().down_unchecked(side_to_move.forward()) };
        let other_pawns = self.occupied((!side_to_move, PieceType::Pawn));
        if (attacks_from_pawns(!side_to_move, en_passant_square) & other_pawns).has_any() {
            self.set_en_passant(Some(en_passant_square));
        }

        self.state.halfmove_clock = 0;
    }

    fn make_promotion(&mut self, mv: Move, promotion: Piece) {
        debug_assert!(self[mv.from_square()].is_some_and(|piece| piece == mv.piece())); // TODO : Should be in a pseudo-legal check
        debug_assert!(self[mv.to_square()].is_none()); // TODO : Should be in a pseudo-legal check

        self.remove_piece(mv.from_square());
        self.put_piece(promotion, mv.to_square());

        self.state.halfmove_clock = 0;
    }

    fn make_capture_promotion(&mut self, mv: Move, capture: Piece, promotion: Piece) {
        debug_assert!(self[mv.from_square()].is_some_and(|piece| piece == mv.piece())); // TODO : Should be in a pseudo-legal check
        debug_assert!(self[mv.to_square()].is_some_and(|piece| piece == capture)); // TODO : Should be in a pseudo-legal check

        self.remove_piece(mv.to_square());
        self.make_promotion(mv, promotion);

        self.state.halfmove_clock = 0;
    }

    fn make_en_passant(&mut self, mv: Move) {
        debug_assert!(self[mv.from_square()].is_some_and(|piece| piece == mv.piece())); // TODO : Should be in a pseudo-legal check
        debug_assert!(self[mv.to_square()].is_none()); // TODO : Should be in a pseudo-legal check

        let capture_sq = unsafe { mv.to_square().down_unchecked(self.side_to_move().forward()) }; // Safe because prise en passant square is never on edge.
        debug_assert!(self[capture_sq].is_some_and(|piece| piece == Piece::new(!self.side_to_move(), PieceType::Pawn))); // TODO : Should be in a pseudo-legal check

        self.remove_piece(capture_sq);
        self.move_piece(mv.piece(), mv.from_square(), mv.to_square());

        self.state.halfmove_clock = 0;
    }

    fn make_castling(&mut self, mv: Move, side: CastlingSide) {
        let rook = Piece::new(self.side_to_move(), PieceType::Rook);
        let rank = mv.from_square().rank();
        let rook_from_file = self.castling_rook_file[usize::from(side)];
        let rook_from = Square::new(rook_from_file, rank);
        let rook_to_file = match side {
            CastlingSide::Queenside => File::D,
            CastlingSide::Kingside => File::F,
        };
        let rook_to = Square::new(rook_to_file, rank);

        self.remove_piece(mv.from_square());
        self.remove_piece(rook_from);
        self.put_piece(mv.piece(), mv.to_square());
        self.put_piece(rook, rook_to);

        self.state.halfmove_clock += 1
    }

    /// Makes a move on the board and updates the game state.
    ///
    /// This function applies the given move to the board, updating all relevant game state including piece positions,
    /// castling rights, en passant possibilities, and turn information. It handles all move types including basic
    /// moves, captures, promotions, en passant captures, and castling.
    ///
    /// # Parameters
    /// * `mv` - The move to be executed on the board
    ///
    /// # Panics
    /// In debug builds, panics if the provided move is illegal. In release builds, making illegal moves leads to
    /// undefined behavior.
    ///
    /// # Note
    /// This function does not verify the legality of the move in release builds. Callers must ensure moves are legal
    /// before calling this function.
    pub fn make(&mut self, mv: Move) {
        debug_assert!(self.is_legal(mv), "Tried to make an illegal move: {:?}", mv);

        self.history.push(self.state);

        self.set_en_passant(None);

        match mv.move_type() {
            MoveType::Basic => self.make_basic(mv),
            MoveType::Capture(capture) => self.make_capture(mv, capture),
            MoveType::TwoSquarePawnPush => self.make_two_square_pawn_push(mv),
            MoveType::Promotion(promotion) => self.make_promotion(mv, promotion),
            MoveType::CapturePromotion { capture, promotion } => self.make_capture_promotion(mv, capture, promotion),
            MoveType::EnPassant => self.make_en_passant(mv),
            MoveType::Castling(side) => self.make_castling(mv, side),
        }

        // Update the castling rights if it's needed.
        let rights = self.castling_rights_mask[usize::from(mv.from_square())]
            | self.castling_rights_mask[usize::from(mv.to_square())];
        if !(self.state.castling_rights & rights).is_empty() {
            self.state.zobrist ^= zobrist_castling(self.state.castling_rights);
            self.state.castling_rights &= !rights;
            self.state.zobrist ^= zobrist_castling(self.state.castling_rights);
        }

        if self.side_to_move() == Color::Black {
            self.state.fullmove_number += 1;
        }

        self.switch_side_to_move();

        self.state.last_move = Some(mv);

        // recompute the blockers for the side to move
        self.state.side_to_move_blockers = self.blockers(self.side_to_move());
    }

    fn unmake_basic(&mut self, mv: Move) {
        self.move_piece_only(mv.piece(), mv.to_square(), mv.from_square());
    }

    fn unmake_capture(&mut self, mv: Move, capture: Piece) {
        self.move_piece_only(mv.piece(), mv.to_square(), mv.from_square());
        self.put_piece_only(capture, mv.to_square());
    }

    fn unmake_promotion(&mut self, mv: Move, promotion: Piece) {
        self.remove_piece_only(promotion, mv.to_square());
        self.put_piece_only(mv.piece(), mv.from_square());
    }

    fn unmake_capture_promotion(&mut self, mv: Move, capture: Piece, promotion: Piece) {
        self.remove_piece_only(promotion, mv.to_square());
        self.put_piece_only(capture, mv.to_square());
        self.put_piece_only(mv.piece(), mv.from_square());
    }

    fn unmake_en_passant(&mut self, mv: Move) {
        self.move_piece_only(mv.piece(), mv.to_square(), mv.from_square());
        let capture_sq = unsafe { mv.to_square().down_unchecked(mv.piece().color().forward()) }; // Safe because prise en passant square is never on edge.
        self.put_piece_only(Piece::new(self.side_to_move(), PieceType::Pawn), capture_sq);
    }

    fn unmake_castling(&mut self, mv: Move, side: CastlingSide) {
        let rook = Piece::new(!self.side_to_move(), PieceType::Rook);
        let rank = mv.from_square().rank();
        let rook_from_file = self.castling_rook_file[usize::from(side)];
        let rook_from = Square::new(rook_from_file, rank);
        let rook_to_file = match side {
            CastlingSide::Queenside => File::D,
            CastlingSide::Kingside => File::F,
        };
        let rook_to = Square::new(rook_to_file, rank);

        self.remove_piece_only(mv.piece(), mv.to_square());
        self.remove_piece_only(rook, rook_to);
        self.put_piece_only(mv.piece(), mv.from_square());
        self.put_piece_only(rook, rook_from);
    }

    /// Reverts the last move made on the board and restores the previous game state.
    ///
    /// This function handles all move types (basic moves, captures, promotions, en passant, and castling) and
    /// completely restores the previous board position including piece positions, castling rights, en passant
    /// possibilities, and turn information.
    ///
    /// # Panics
    /// Panics if there is no last move to undo (state.last_move is None) or if there's no previous state in the history
    /// stack.
    ///
    /// # Note
    /// This function assumes that moves and states have been properly tracked and stored in the history. It should only
    /// be called if a previous move exists.
    pub fn unmake(&mut self) {
        let mv = self.state.last_move.expect("There should be a last move.");

        match mv.move_type() {
            MoveType::Basic => self.unmake_basic(mv),
            MoveType::Capture(capture) => self.unmake_capture(mv, capture),
            MoveType::TwoSquarePawnPush => self.unmake_basic(mv),
            MoveType::Promotion(promotion) => self.unmake_promotion(mv, promotion),
            MoveType::CapturePromotion { capture, promotion } => self.unmake_capture_promotion(mv, capture, promotion),
            MoveType::EnPassant => self.unmake_en_passant(mv),
            MoveType::Castling(side) => self.unmake_castling(mv, side),
        }

        self.state = self.history.pop();
    }
}

impl Default for Position {
    fn default() -> Self {
        Self {
            board: [None; Square::COUNT],
            bb_color: [Bitboard::EMPTY; Color::COUNT],
            bb_piece: [Bitboard::EMPTY; Piece::COUNT],
            castling_rook_file: [File::A, File::H],
            castling_path: [Square::E1 | Square::E8, Square::C1 | Square::D1 | Square::C8 | Square::D8],
            castling_rights_mask: [CastlingRight::empty(); Square::COUNT],
            state: GameState::default(),
            history: History::default(),
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
    use ctor::ctor;

    #[ctor]
    fn setup() {
        crate::initialize();
    }

    #[test]
    fn test_position_default() {
        let position = Position::default();
        assert_eq!(position.side_to_move(), Color::White);
        assert!(position.board.iter().all(|square| square.is_none()));
        assert!(position.bb_color.iter().all(|bb| bb.has_none()));
        assert!(position.bb_piece.iter().all(|bb| bb.has_none()));
    }

    #[test]
    fn test_new_initial_position() {
        let position = Position::new();

        assert_eq!(position.side_to_move(), Color::White);

        assert_eq!(position[Square::A1], Some(Piece::WHITE_ROOK));
        assert_eq!(position[Square::B1], Some(Piece::WHITE_KNIGHT));
        assert_eq!(position[Square::C1], Some(Piece::WHITE_BISHOP));
        assert_eq!(position[Square::D1], Some(Piece::WHITE_QUEEN));
        assert_eq!(position[Square::E1], Some(Piece::WHITE_KING));
        assert_eq!(position[Square::F1], Some(Piece::WHITE_BISHOP));
        assert_eq!(position[Square::G1], Some(Piece::WHITE_KNIGHT));
        assert_eq!(position[Square::H1], Some(Piece::WHITE_ROOK));

        for file in File::ALL {
            assert_eq!(position[Square::new(file, Rank::R2)], Some(Piece::WHITE_PAWN));
        }

        for rank in Rank::ALL[2..6].iter() {
            for file in File::ALL {
                assert_eq!(position[Square::new(file, *rank)], None);
            }
        }

        for file in File::ALL {
            assert_eq!(position[Square::new(file, Rank::R7)], Some(Piece::BLACK_PAWN));
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

        assert_eq!(position.castling_availability(), CastlingRight::all());

        assert_eq!(File::H, position.castling_file(CastlingSide::Kingside));
        assert_eq!(File::A, position.castling_file(CastlingSide::Queenside));

        assert_eq!(
            Bitboard::between(Square::E1, Square::G1) | Bitboard::between(Square::E8, Square::G8),
            position.castling_path(CastlingSide::Kingside)
        );
        assert_eq!(
            Bitboard::between(Square::E1, Square::B1) | Bitboard::between(Square::E8, Square::B8),
            position.castling_path(CastlingSide::Queenside)
        );

        assert_eq!(position.en_passant_square(), None);

        assert_eq!(position.state.halfmove_clock, 0);
        assert_eq!(position.state.fullmove_number, 1);
    }

    #[test]
    fn test_new_from_fen_black_to_play() {
        let position = Position::new_from_fen("rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1").unwrap();

        assert_eq!(position.state.side_to_move, Color::Black);
    }

    #[test]
    fn test_new_from_fen_no_castling_rights() {
        let position = Position::new_from_fen("1nbqkbn1/rppppppr/p6p/8/8/P6P/RPPPPPPR/1NBQKBN1 w - - 4 5").unwrap();

        assert_eq!(position.castling_availability(), CastlingRight::empty());
    }

    #[test]
    fn test_new_from_fen_partial_castling_rights() {
        let position = Position::new_from_fen("1nbqkbnr/rppppppp/p7/8/8/7P/PPPPPPPR/RNBQKBN1 w Qk - 2 3").unwrap();

        assert_eq!(position.castling_availability(), CastlingRight::BLACK_KINGSIDE | CastlingRight::WHITE_QUEENSIDE);
    }

    #[test]
    fn test_new_from_fen_chess960_castling_rights() {
        let position = Position::new_from_fen("1rk3r1/8/8/8/8/8/8/1RK3R1 w KQkq - 0 1").unwrap();

        assert_eq!(CastlingRight::all(), position.castling_availability());
        assert_eq!(File::B, position.castling_file(CastlingSide::Queenside));
        assert_eq!(File::G, position.castling_file(CastlingSide::Kingside));
    }

    #[test]
    fn test_new_from_fen_chess960_castling_outer_rooks() {
        let position = Position::new_from_fen("1rrkrr2/8/8/8/8/8/8/1RRKRR2 w KQkq - 0 1").unwrap();

        assert_eq!(CastlingRight::all(), position.castling_availability());
        assert_eq!(File::B, position.castling_file(CastlingSide::Queenside));
        assert_eq!(File::F, position.castling_file(CastlingSide::Kingside));
    }

    #[test]
    fn test_new_from_fen_chess960_castling_inner_rooks() {
        let position = Position::new_from_fen("1rrkrr2/8/8/8/8/8/8/1RRKRR2 w CEce - 0 1").unwrap();

        assert_eq!(CastlingRight::all(), position.castling_availability());
        assert_eq!(File::C, position.castling_file(CastlingSide::Queenside));
        assert_eq!(File::E, position.castling_file(CastlingSide::Kingside));
    }

    #[test]
    fn test_new_from_fen_chess960_castling_path_outside_king_rook() {
        let position = Position::new_from_fen("6k1/8/8/8/8/8/8/1R4KR w K - 0 1").unwrap();

        assert_eq!(Square::F1 | Square::F8, position.castling_path(CastlingSide::Kingside));
    }

    #[test]
    fn test_new_from_fen_en_passant_square() {
        let position = Position::new_from_fen("rnbqkbnr/1pp1pppp/p7/3pP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3").unwrap();

        assert_eq!(position.en_passant_square(), Some(Square::D6));
    }

    #[test]
    fn test_to_fen() {
        let fens = [
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1",
            "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3",
            "r3k2r/pppbqppp/2n1bn2/3pp3/3PP3/2N1BN2/PPPBQPPP/R3K2R w - - 0 7",
            "1rrkrr2/8/8/8/8/8/8/1RRKRR2 w KQkq - 0 1",
            "1rrkrr2/8/8/8/8/8/8/1RRKRR2 w ECec - 0 1",
            "r3k2R/8/8/8/8/8/8/R3K3 b Qq - 0 1",
        ];

        for fen in fens.iter() {
            let position = Position::new_from_fen(fen).unwrap();
            assert_eq!(fen, &position.to_fen());
        }
    }

    #[test]
    fn test_to_fen_without_minimals_fields() {
        let position = Position::new_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w").unwrap();

        assert_eq!(position.castling_availability(), CastlingRight::empty());
        assert_eq!(position.en_passant_square(), None);
        assert_eq!(position.state.halfmove_clock, 0);
        assert_eq!(position.state.fullmove_number, 1);
    }

    #[test]
    fn test_put_piece() {
        let mut position = Position::default();

        position.put_piece(Piece::WHITE_QUEEN, Square::E4);

        assert_eq!(position[Square::E4], Some(Piece::WHITE_QUEEN));
        assert_eq!(position.occupied(Color::White), Bitboard::from(Square::E4));
        assert_eq!(position.occupied(Piece::WHITE_QUEEN), Square::E4.into());

        position.put_piece(Piece::BLACK_KNIGHT, Square::H8);
        assert_eq!(position[Square::H8], Some(Piece::BLACK_KNIGHT));
        assert_eq!(position.occupied(Color::Black), Bitboard::from(Square::H8));
        assert_eq!(position.occupied(Piece::BLACK_KNIGHT), Square::H8.into());
    }

    #[test]
    fn test_remove_piece() {
        let mut position = Position::default();
        position.put_piece(Piece::BLACK_PAWN, Square::B5);

        position.remove_piece(Square::B5);

        assert_eq!(position[Square::B5], None);
        assert_eq!(position.occupied(Color::Black), Bitboard::EMPTY);
        assert_eq!(position.occupied(Piece::BLACK_PAWN), Bitboard::EMPTY);
    }

    #[test]
    fn test_move_piece() {
        let mut position = Position::default();
        position.put_piece(Piece::WHITE_PAWN, Square::A2);

        position.move_piece(Piece::WHITE_PAWN, Square::A2, Square::A3);

        assert_eq!(position[Square::A2], None);
        assert_eq!(position[Square::A3], Some(Piece::WHITE_PAWN));
        assert_eq!(position.occupied(Color::White), Square::A3.into());
        assert_eq!(position.occupied(Piece::WHITE_PAWN), Square::A3.into());
    }

    #[test]
    fn test_is_attacked_by_rook() {
        let position = Position::new_from_fen("4k3/8/8/8/8/8/8/1R2K3 w - - 0 1").unwrap();

        assert!(position.is_attacked(Square::B5, position.occupied(OccupancyFilter::All), Color::White));
        assert!(!position.is_attacked(Square::B5, position.occupied(OccupancyFilter::All), Color::Black));
        assert!(!position.is_attacked(Square::C2, position.occupied(OccupancyFilter::All), Color::White));
    }

    #[test]
    fn test_is_attacked_by_bishop() {
        let position = Position::new_from_fen("4k3/8/8/8/8/8/8/1B2K3 w - - 0 1").unwrap();

        assert!(position.is_attacked(Square::H7, position.occupied(OccupancyFilter::All), Color::White));
        assert!(!position.is_attacked(Square::H7, position.occupied(OccupancyFilter::All), Color::Black));
        assert!(!position.is_attacked(Square::H6, position.occupied(OccupancyFilter::All), Color::White));
    }

    #[test]
    fn test_is_attacked_by_queen() {
        let position = Position::new_from_fen("4k3/8/8/8/8/8/8/1Q2K3 w - - 0 1").unwrap();

        assert!(position.is_attacked(Square::B5, position.occupied(OccupancyFilter::All), Color::White));
        assert!(!position.is_attacked(Square::B5, position.occupied(OccupancyFilter::All), Color::Black));
        assert!(position.is_attacked(Square::C2, position.occupied(OccupancyFilter::All), Color::White));
        assert!(position.is_attacked(Square::H7, position.occupied(OccupancyFilter::All), Color::White));
        assert!(!position.is_attacked(Square::H7, position.occupied(OccupancyFilter::All), Color::Black));
        assert!(!position.is_attacked(Square::H6, position.occupied(OccupancyFilter::All), Color::White));
    }

    #[test]
    fn test_is_attacked_by_knight() {
        let position = Position::new_from_fen("4k3/8/8/3n4/8/8/8/4K3 b - - 0 1").unwrap();

        assert!(position.is_attacked(Square::E3, position.occupied(OccupancyFilter::All), Color::Black));
        assert!(!position.is_attacked(Square::D4, position.occupied(OccupancyFilter::All), Color::Black));
        assert!(!position.is_attacked(Square::E3, position.occupied(OccupancyFilter::All), Color::White));
    }

    #[test]
    fn test_is_attacked_by_king() {
        let position = Position::new_from_fen("4k3/8/8/8/8/8/8/4K3 b - - 0 1").unwrap();

        assert!(position.is_attacked(Square::E7, position.occupied(OccupancyFilter::All), Color::Black));
        assert!(!position.is_attacked(Square::G6, position.occupied(OccupancyFilter::All), Color::Black));

        assert!(!position.is_attacked(Square::E7, position.occupied(OccupancyFilter::All), Color::White));
    }

    #[test]
    fn test_is_check() {
        // Simple check
        let position = Position::new_from_fen("4k3/8/8/1B6/8/8/8/4K3 b - - 0 1").unwrap();
        assert!(position.is_check());

        // Not in check
        let position2 = Position::new_from_fen("4k3/2Q3Q1/8/2BR1RB1/8/8/8/4K3 b - - 0 1").unwrap();
        assert!(!position2.is_check());

        // Check by pawn
        let position3 = Position::new_from_fen("4k3/5P2/8/8/8/8/8/4K3 b - - 0 1").unwrap();
        assert!(position3.is_check());
    }

    #[test]
    fn test_attacks_to() {
        let position = Position::new_from_fen("3r4/b7/2n3r1/2p1pn2/q2Nk1q1/3n4/3r1b2/1K6 b - - 0 1").unwrap();

        let attacks_black = position.attacks_to(Square::D4, position.occupied(OccupancyFilter::All), Color::Black);
        assert_eq!(
            attacks_black,
            Square::A4 | Square::C5 | Square::C6 | Square::D8 | Square::E5 | Square::E4 | Square::F5 | Square::F2
        );

        let attacks_white = position.attacks_to(Square::D4, position.occupied(OccupancyFilter::All), Color::White);
        assert_eq!(attacks_white, Bitboard::EMPTY);
    }

    #[test]
    fn test_get_king_square() {
        let position = Position::new_from_fen("8/8/4k3/8/8/8/3K4/8 w - - 0 1").unwrap();

        assert_eq!(position.king_square(Color::Black), Square::E6);
        assert_eq!(position.king_square(Color::White), Square::D2);
    }

    #[test]
    fn test_blockers_lots_of_blockers() {
        let position = Position::new_from_fen("4k3/4q3/8/8/7b/2b1N1N1/3r4/1rN1KN1r w - - 0 1").unwrap();

        let blockers = position.blockers(Color::White);
        assert_eq!(blockers, Square::C1 | Square::D2 | Square::E3 | Square::F1 | Square::G3);
    }

    #[test]
    fn test_blocker_no_blockers_because_two_pieces() {
        let position = Position::new_from_fen("4k3/8/8/b7/1B6/8/3B4/4K3 w - - 0 1").unwrap();

        let blockers = position.blockers(Color::White);
        assert_eq!(blockers, Bitboard::EMPTY);
    }

    #[test]
    fn test_to_compact_string() {
        let position = Position::new_from_fen("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1").unwrap();

        assert_eq!(
            position.to_compact_string(),
            "8  r n b q k b n r\n7  p p p p p p p p\n6  . . . . . . . .\n5  . . . . . . . .\n4  . . . . . . . .\n3  . . . . . . . .\n2  P P P P P P P P\n1  R N B Q K B N R\n   a b c d e f g h"
        );
    }
}
