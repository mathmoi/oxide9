use std::{cmp::min, fmt::Display};

use crate::{
    config::get_config,
    coordinates::Square,
    piece::{Color, Piece, PieceType},
    position::Position,
};

/// A simple wrapper around a 16-bit integer that represents the evaluation of a position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Eval(i16);

impl Eval {
    const MAX_MAT_DEPTH: u16 = 2000;

    /// The minimum possible evaluation score
    pub const MIN: Eval = Eval(-i16::MAX); // We use the negative of the maximum value to represent the minimum because
                                           // we can't use i16::MIN, because it cannot be negated as a valid i16 valuue.

    /// The maximum possible evaluation score
    pub const MAX: Eval = Eval(i16::MAX);

    /// The evaluation score for a mat.
    pub const MAT: Eval = Eval(-30000);

    /// The evaluation score for a draw.
    pub const DRAW: Eval = Eval(0);

    /// Creates a new Eval instance with the given value.
    pub const fn new(value: i16) -> Self {
        debug_assert!(value >= i16::MIN && value <= i16::MAX);

        Eval(value)
    }

    /// Creates a new Eval instance representing a checkmate.
    pub const fn new_mat(depth: u16) -> Self {
        debug_assert!(depth <= Self::MAX_MAT_DEPTH);

        Eval(Self::MAT.0 + (depth as i16))
    }
}

impl Default for Eval {
    fn default() -> Self {
        Eval(0)
    }
}

impl std::ops::Add for Eval {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        Eval(self.0 + rhs.0)
    }
}

impl std::ops::Sub for Eval {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        Eval(self.0 - rhs.0)
    }
}

impl std::ops::Mul<i16> for Eval {
    type Output = Self;
    fn mul(self, rhs: i16) -> Self {
        Eval(self.0 * rhs)
    }
}

impl std::ops::Mul<Eval> for i16 {
    type Output = Eval;
    fn mul(self, rhs: Eval) -> Self::Output {
        Eval(self * rhs.0)
    }
}

impl std::ops::Neg for Eval {
    type Output = Self;
    fn neg(self) -> Self {
        Eval(-self.0)
    }
}

impl From<Eval> for i32 {
    fn from(value: Eval) -> Self {
        value.0 as i32
    }
}

impl From<i32> for Eval {
    fn from(value: i32) -> Self {
        debug_assert!(value >= i16::MIN as i32 && value <= i16::MAX as i32);

        Eval(value as i16)
    }
}

impl From<Eval> for i16 {
    fn from(value: Eval) -> Self {
        value.0 as i16
    }
}

impl From<i16> for Eval {
    fn from(value: i16) -> Self {
        Eval(value as i16)
    }
}

/// Formats the evaluation score as a floating-point number.
///
/// The internal centipawn value is converted to a decimal value and displayed with 2 decimal places. For example, an
/// evaluation of 100 centipawns is displayed as "1.00".
impl Display for Eval {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:.2}", self.0 as f32 / 100.0)
    }
}

/// A pair of evaluations for the middle game and the end game
#[derive(Clone, Copy)]
pub struct EvalPair {
    /// Evaluation for the middle game
    mg: Eval,

    /// Evaluation for the end game
    eg: Eval,
}

impl Default for EvalPair {
    fn default() -> Self {
        EvalPair { mg: Eval::default(), eg: Eval::default() }
    }
}

impl EvalPair {
    /// Creates a new EvalPair with separate evaluations for middle game and end game phases.
    ///
    /// # Parameters
    /// * `mg` - The middle game evaluation score
    /// * `eg` - The end game evaluation score
    pub fn new(mg: Eval, eg: Eval) -> Self {
        EvalPair { mg, eg }
    }

    /// Returns the middle game evaluation.
    pub fn mg(&self) -> Eval {
        self.mg
    }

    /// Returns the end game evaluation.
    pub fn eg(&self) -> Eval {
        self.eg
    }
}

impl std::ops::Add for EvalPair {
    type Output = Self;
    fn add(self, rhs: Self) -> Self {
        EvalPair { mg: self.mg + rhs.mg, eg: self.eg + rhs.eg }
    }
}

impl std::ops::AddAssign for EvalPair {
    fn add_assign(&mut self, rhs: Self) {
        self.mg = self.mg + rhs.mg;
        self.eg = self.eg + rhs.eg;
    }
}

impl std::ops::Sub for EvalPair {
    type Output = Self;
    fn sub(self, rhs: Self) -> Self {
        EvalPair { mg: self.mg - rhs.mg, eg: self.eg - rhs.eg }
    }
}

impl std::ops::SubAssign for EvalPair {
    fn sub_assign(&mut self, rhs: Self) {
        self.mg = self.mg - rhs.mg;
        self.eg = self.eg - rhs.eg;
    }
}

impl std::ops::Neg for EvalPair {
    type Output = Self;
    fn neg(self) -> Self {
        EvalPair { mg: -self.mg, eg: -self.eg }
    }
}

static mut PIECES_GAME_PHASE: [u8; Piece::COUNT] = [0; Piece::COUNT];
static mut MAX_GAME_PHASE: u8 = 0;
static mut PIECES_VALUES: [EvalPair; Piece::COUNT] = [EvalPair { mg: Eval(0), eg: Eval(0) }; Piece::COUNT];
static mut PIECE_SQUARE_TABLES: [[EvalPair; Square::COUNT]; Piece::COUNT] =
    [[EvalPair { mg: Eval(0), eg: Eval(0) }; Square::COUNT]; Piece::COUNT];

/// Initialize the evaluation module
pub fn initialize() {
    initialize_pieces_game_phase();
    initialize_pieces_values();
    initialize_piece_square_tables();
}

/// Initializes the game phase values for all chess pieces.
///
/// # Purpose
/// Sets up the game phase values for each piece based on the configuration settings. These values are used to determine
/// the current phase of the game (middlegame/endgame) which affects the evaluation.
fn initialize_pieces_game_phase() {
    let config = get_config();

    for piece in Piece::ALL {
        let piece_type_index = usize::from(piece.piece_type());
        unsafe {
            PIECES_GAME_PHASE[usize::from(piece)] = config.eval.piece_type_game_phase[piece_type_index];
        }
    }

    unsafe {
        MAX_GAME_PHASE = config.eval.piece_type_game_phase[usize::from(PieceType::Pawn)] * 16
            + config.eval.piece_type_game_phase[usize::from(PieceType::Knight)] * 4
            + config.eval.piece_type_game_phase[usize::from(PieceType::Bishop)] * 4
            + config.eval.piece_type_game_phase[usize::from(PieceType::Rook)] * 4
            + config.eval.piece_type_game_phase[usize::from(PieceType::Queen)] * 2
            + config.eval.piece_type_game_phase[usize::from(PieceType::King)] * 2
    };
}

/// Initializes the evaluation values for all chess pieces.
///
/// # Purpose
/// Sets up the material evaluation values for each piece based on the configuration settings. These values are used in
/// position evaluation to calculate material balance.
fn initialize_pieces_values() {
    let config = get_config();

    for piece in Piece::ALL {
        let sign = match piece.color() {
            Color::White => 1,
            Color::Black => -1,
        };

        let piece_type_index = usize::from(piece.piece_type());
        let mg = Eval(sign * config.eval.mg_piece_values[piece_type_index]);
        let eg = Eval(sign * config.eval.eg_piece_values[piece_type_index]);
        unsafe {
            PIECES_VALUES[usize::from(piece)] = EvalPair::new(mg, eg);
        }
    }
}

/// Initializes the piece-square tables for position evaluation.
///
/// # Purpose
/// Sets up the position-dependent values for each piece type on each square of the board. These tables are used to
/// evaluate piece positioning during different game phases.
fn initialize_piece_square_tables() {
    let config = get_config();
    let pieces = [
        (PieceType::Pawn, &config.eval.mg_pawn_table, &config.eval.eg_pawn_table),
        (PieceType::Knight, &config.eval.mg_knight_table, &config.eval.eg_knight_table),
        (PieceType::Bishop, &config.eval.mg_bishop_table, &config.eval.eg_bishop_table),
        (PieceType::Rook, &config.eval.mg_rook_table, &config.eval.eg_rook_table),
        (PieceType::Queen, &config.eval.mg_queen_table, &config.eval.eg_queen_table),
        (PieceType::King, &config.eval.mg_king_table, &config.eval.eg_king_table),
    ];

    for (piece_type, mg_table, eg_table) in &pieces {
        for square in Square::ALL {
            let sq_index: usize = square.into();
            let mg = Eval(mg_table[sq_index]);
            let eg = Eval(eg_table[sq_index]);
            let eval_pair = EvalPair::new(mg, eg);
            unsafe {
                let white_piece_index: usize = Piece::new(Color::White, *piece_type).into();
                let piece_value = PIECES_VALUES[white_piece_index];
                PIECE_SQUARE_TABLES[white_piece_index][sq_index] = eval_pair + piece_value;

                let black_piece_index: usize = Piece::new(Color::Black, *piece_type).into();
                PIECE_SQUARE_TABLES[black_piece_index][usize::from(square.relative_to_color(Color::Black))] =
                    -(eval_pair + piece_value);
            }
        }
    }
}

/// Returns the game phase contribution value of a given piece.
///
/// # Purpose
/// Retrieves the game phase value for a specific piece. This value represents how much the piece contributes to
/// determining the current phase of the game (middlegame, endgame).
///
/// # Parameters
/// * `piece` - The chess piece whose game phase value to retrieve
///
/// # Returns
/// A `u8` value representing the piece's contribution to the game phase calculation.
///
/// # Important
/// - Returns values from the precomputed PIECES_GAME_PHASE that must be initialized before calling this function
/// - These values are typically used in tapered evaluation to determine the balance between middlegame and endgame
///   evaluation
pub fn get_piece_type_game_phase(piece: Piece) -> u8 {
    unsafe { PIECES_GAME_PHASE[usize::from(piece)] }
}

/// Returns the piece-square table value for a given piece on a given square.
///
/// # Purpose
/// Retrieves the precomputed position-dependent evaluation value for a specific piece placed on a specific square.
///
/// # Parameters
/// * `piece` - The chess piece to evaluate
/// * `square` - The square where the piece is located
///
/// # Returns
/// An `EvalPair` containing both middlegame and endgame evaluation values for the piece on that square.
///
/// # Important
/// - Returns values from the precomputed PIECE_SQUARE_TABLES that must be initialized before calling this function
/// - The returned value already includes the base piece value combined with the positional bonus
pub fn get_piece_square_value(piece: Piece, square: Square) -> EvalPair {
    unsafe { PIECE_SQUARE_TABLES[usize::from(piece)][usize::from(square)] }
}

/// Evaluates the given position and returns a single evaluation score.
///
/// # Purpose
/// Calculates the overall evaluation of the current chess position by blending middlegame and endgame evaluations based
/// on the current game phase.
///
/// # Parameters
/// * `position` - The chess position to evaluate
///
/// # Returns
/// An `Eval` value representing the strength of the position from the perspective of the side to move. Positive values
/// favor the side to move, while negative values favor the opponent.
///
/// # Important
/// - Uses the incrementally updated evaluation as the base for the calculation
/// - Applies phase-dependent blending (tapered evaluation) between middlegame and endgame values
/// - The blending is determined by comparing the current game phase to MAX_GAME_PHASE
/// - The returned value is normalized by dividing by MAX_GAME_PHASE
pub fn evaluate(position: &Position) -> Eval {
    let mg_phase: i32 = min(position.game_phase(), unsafe { MAX_GAME_PHASE }) as i32;
    let eg_phase: i32 = unsafe { MAX_GAME_PHASE as i32 } - mg_phase;

    let incremental_eval = position.incremental_eval();
    Eval::from(
        (mg_phase * i32::from(incremental_eval.mg) + eg_phase * i32::from(incremental_eval.eg))
            / unsafe { MAX_GAME_PHASE as i32 },
    )
}
