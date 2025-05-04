use std::{cmp::min, fmt::Display};

use crate::{
    coordinates::Square,
    piece::{Color, Piece, PieceType},
    position::Position,
};

/// A simple wrapper around a 16-bit integer that represents the evaluation of a position.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Default)]
pub struct Eval(i16);

impl Eval {
    const MAX_MAT_DEPTH: u16 = 10000;

    /// The minimum possible evaluation score
    pub const MIN: Eval = Eval(-i16::MAX); // We use the negative of the maximum value to represent the minimum because
                                           // we can't use i16::MIN, because it cannot be negated as a valid i16 valuue.

    /// The maximum possible evaluation score
    pub const MAX: Eval = Eval(i16::MAX);

    /// The evaluation score for a mat.
    pub const MAT: Eval = Eval(30000);

    /// The evaluation score for a draw.
    pub const DRAW: Eval = Eval(0);

    /// Creates a new Eval instance with the given value.
    pub const fn new(value: i16) -> Self {
        Eval(value)
    }

    /// Creates a new Eval instance representing a checkmate.
    pub const fn new_mat(depth: u16) -> Self {
        debug_assert!(depth <= Self::MAX_MAT_DEPTH);

        Eval(Self::MAT.0 - (depth as i16))
    }

    /// Removes the given number of ply to a mate evaluation. If the evaluation is not a mat it does nothing.
    pub fn remove_ply_from_mat(self, ply: u16) -> Self {
        debug_assert!(ply <= Self::MAX_MAT_DEPTH);
        self.remove_ply_from_mat_signed(ply as i16)
    }

    /// Removes the given number of ply to a mate evaluation. If the evaluation is not a mat it does nothing.
    pub fn add_ply_to_mat(self, ply: u16) -> Self {
        debug_assert!(ply <= Self::MAX_MAT_DEPTH);
        self.remove_ply_from_mat_signed(-(ply as i16))
    }

    fn remove_ply_from_mat_signed(self, ply: i16) -> Self {
        if self.0 > Eval::MAT.0 - Self::MAX_MAT_DEPTH as i16 {
            Eval(self.0 + ply)
        } else if self.0 < -Eval::MAT.0 + Self::MAX_MAT_DEPTH as i16 {
            Eval(self.0 - ply)
        } else {
            self
        }
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

impl std::ops::Add<i16> for Eval {
    type Output = Self;
    fn add(self, rhs: i16) -> Self {
        Eval(self.0 + rhs)
    }
}

impl std::ops::Sub<i16> for Eval {
    type Output = Self;
    fn sub(self, rhs: i16) -> Self {
        Eval(self.0 - rhs)
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
        value.0
    }
}

impl From<i16> for Eval {
    fn from(value: i16) -> Self {
        Eval(value)
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
#[derive(Default, Clone, Copy, Debug)]
pub struct EvalPair {
    /// Evaluation for the middle game
    mg: Eval,

    /// Evaluation for the end game
    eg: Eval,
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

const PIECES_GAME_PHASE: [u8; Piece::COUNT] = [0, 0, 1, 1, 1, 1, 2, 2, 4, 4, 0, 0];
const MAX_GAME_PHASE: u8 = 24;
#[rustfmt::skip]
const PIECE_TYPE_VALUES: [EvalPair; PieceType::COUNT] = [
    EvalPair { mg: Eval(   82), eg: Eval(  94) },
    EvalPair { mg: Eval(  337), eg: Eval( 281) },
    EvalPair { mg: Eval(  365), eg: Eval( 297) },
    EvalPair { mg: Eval(  477), eg: Eval( 512) },
    EvalPair { mg: Eval( 1025), eg: Eval( 936) },
    EvalPair { mg: Eval(    0), eg: Eval(   0) },
];

#[rustfmt::skip]
const PIECE_TYPE_SQUARE_TABLES: [[EvalPair; Square::COUNT]; PieceType::COUNT] = [
    [
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval( -35), eg: Eval(  13) },
        EvalPair { mg: Eval(  -1), eg: Eval(   8) },
        EvalPair { mg: Eval( -20), eg: Eval(   8) },
        EvalPair { mg: Eval( -23), eg: Eval(  10) },
        EvalPair { mg: Eval( -15), eg: Eval(  13) },
        EvalPair { mg: Eval(  24), eg: Eval(   0) },
        EvalPair { mg: Eval(  38), eg: Eval(   2) },
        EvalPair { mg: Eval( -22), eg: Eval(  -7) },
        EvalPair { mg: Eval( -26), eg: Eval(   4) },
        EvalPair { mg: Eval(  -4), eg: Eval(   7) },
        EvalPair { mg: Eval(  -4), eg: Eval(  -6) },
        EvalPair { mg: Eval( -10), eg: Eval(   1) },
        EvalPair { mg: Eval(   3), eg: Eval(   0) },
        EvalPair { mg: Eval(   3), eg: Eval(  -5) },
        EvalPair { mg: Eval(  33), eg: Eval(  -1) },
        EvalPair { mg: Eval( -12), eg: Eval(  -8) },
        EvalPair { mg: Eval( -27), eg: Eval(  13) },
        EvalPair { mg: Eval(  -2), eg: Eval(   9) },
        EvalPair { mg: Eval(  -5), eg: Eval(  -3) },
        EvalPair { mg: Eval(  12), eg: Eval(  -7) },
        EvalPair { mg: Eval(  17), eg: Eval(  -7) },
        EvalPair { mg: Eval(   6), eg: Eval(  -8) },
        EvalPair { mg: Eval(  10), eg: Eval(   3) },
        EvalPair { mg: Eval( -25), eg: Eval(  -1) },
        EvalPair { mg: Eval( -14), eg: Eval(  32) },
        EvalPair { mg: Eval(  13), eg: Eval(  24) },
        EvalPair { mg: Eval(   6), eg: Eval(  13) },
        EvalPair { mg: Eval(  21), eg: Eval(   5) },
        EvalPair { mg: Eval(  23), eg: Eval(  -2) },
        EvalPair { mg: Eval(  12), eg: Eval(   4) },
        EvalPair { mg: Eval(  17), eg: Eval(  17) },
        EvalPair { mg: Eval( -23), eg: Eval(  17) },
        EvalPair { mg: Eval(  -6), eg: Eval(  94) },
        EvalPair { mg: Eval(   7), eg: Eval( 100) },
        EvalPair { mg: Eval(  26), eg: Eval(  85) },
        EvalPair { mg: Eval(  31), eg: Eval(  67) },
        EvalPair { mg: Eval(  65), eg: Eval(  56) },
        EvalPair { mg: Eval(  56), eg: Eval(  53) },
        EvalPair { mg: Eval(  25), eg: Eval(  82) },
        EvalPair { mg: Eval( -20), eg: Eval(  84) },
        EvalPair { mg: Eval(  98), eg: Eval( 178) },
        EvalPair { mg: Eval( 134), eg: Eval( 173) },
        EvalPair { mg: Eval(  61), eg: Eval( 158) },
        EvalPair { mg: Eval(  95), eg: Eval( 134) },
        EvalPair { mg: Eval(  68), eg: Eval( 147) },
        EvalPair { mg: Eval( 126), eg: Eval( 132) },
        EvalPair { mg: Eval(  34), eg: Eval( 165) },
        EvalPair { mg: Eval( -11), eg: Eval( 187) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
        EvalPair { mg: Eval(   0), eg: Eval(   0) },
    ],
    [
        EvalPair { mg: Eval(-105), eg: Eval( -29) },
        EvalPair { mg: Eval( -21), eg: Eval( -51) },
        EvalPair { mg: Eval( -58), eg: Eval( -23) },
        EvalPair { mg: Eval( -33), eg: Eval( -15) },
        EvalPair { mg: Eval( -17), eg: Eval( -22) },
        EvalPair { mg: Eval( -28), eg: Eval( -18) },
        EvalPair { mg: Eval( -19), eg: Eval( -50) },
        EvalPair { mg: Eval( -23), eg: Eval( -64) },
        EvalPair { mg: Eval( -29), eg: Eval( -42) },
        EvalPair { mg: Eval( -53), eg: Eval( -20) },
        EvalPair { mg: Eval( -12), eg: Eval( -10) },
        EvalPair { mg: Eval(  -3), eg: Eval(  -5) },
        EvalPair { mg: Eval(  -1), eg: Eval(  -2) },
        EvalPair { mg: Eval(  18), eg: Eval( -20) },
        EvalPair { mg: Eval( -14), eg: Eval( -23) },
        EvalPair { mg: Eval( -19), eg: Eval( -44) },
        EvalPair { mg: Eval( -23), eg: Eval( -23) },
        EvalPair { mg: Eval(  -9), eg: Eval(  -3) },
        EvalPair { mg: Eval(  12), eg: Eval(  -1) },
        EvalPair { mg: Eval(  10), eg: Eval(  15) },
        EvalPair { mg: Eval(  19), eg: Eval(  10) },
        EvalPair { mg: Eval(  17), eg: Eval(  -3) },
        EvalPair { mg: Eval(  25), eg: Eval( -20) },
        EvalPair { mg: Eval( -16), eg: Eval( -22) },
        EvalPair { mg: Eval( -13), eg: Eval( -18) },
        EvalPair { mg: Eval(   4), eg: Eval(  -6) },
        EvalPair { mg: Eval(  16), eg: Eval(  16) },
        EvalPair { mg: Eval(  13), eg: Eval(  25) },
        EvalPair { mg: Eval(  28), eg: Eval(  16) },
        EvalPair { mg: Eval(  19), eg: Eval(  17) },
        EvalPair { mg: Eval(  21), eg: Eval(   4) },
        EvalPair { mg: Eval(  -8), eg: Eval( -18) },
        EvalPair { mg: Eval(  -9), eg: Eval( -17) },
        EvalPair { mg: Eval(  17), eg: Eval(   3) },
        EvalPair { mg: Eval(  19), eg: Eval(  22) },
        EvalPair { mg: Eval(  53), eg: Eval(  22) },
        EvalPair { mg: Eval(  37), eg: Eval(  22) },
        EvalPair { mg: Eval(  69), eg: Eval(  11) },
        EvalPair { mg: Eval(  18), eg: Eval(   8) },
        EvalPair { mg: Eval(  22), eg: Eval( -18) },
        EvalPair { mg: Eval( -47), eg: Eval( -24) },
        EvalPair { mg: Eval(  60), eg: Eval( -20) },
        EvalPair { mg: Eval(  37), eg: Eval(  10) },
        EvalPair { mg: Eval(  65), eg: Eval(   9) },
        EvalPair { mg: Eval(  84), eg: Eval(  -1) },
        EvalPair { mg: Eval( 129), eg: Eval(  -9) },
        EvalPair { mg: Eval(  73), eg: Eval( -19) },
        EvalPair { mg: Eval(  44), eg: Eval( -41) },
        EvalPair { mg: Eval( -73), eg: Eval( -25) },
        EvalPair { mg: Eval( -41), eg: Eval(  -8) },
        EvalPair { mg: Eval(  72), eg: Eval( -25) },
        EvalPair { mg: Eval(  36), eg: Eval(  -2) },
        EvalPair { mg: Eval(  23), eg: Eval(  -9) },
        EvalPair { mg: Eval(  62), eg: Eval( -25) },
        EvalPair { mg: Eval(   7), eg: Eval( -24) },
        EvalPair { mg: Eval( -17), eg: Eval( -52) },
        EvalPair { mg: Eval(-167), eg: Eval( -58) },
        EvalPair { mg: Eval( -89), eg: Eval( -38) },
        EvalPair { mg: Eval( -34), eg: Eval( -13) },
        EvalPair { mg: Eval( -49), eg: Eval( -28) },
        EvalPair { mg: Eval(  61), eg: Eval( -31) },
        EvalPair { mg: Eval( -97), eg: Eval( -27) },
        EvalPair { mg: Eval( -15), eg: Eval( -63) },
        EvalPair { mg: Eval(-107), eg: Eval( -99) },
    ],
    [
        EvalPair { mg: Eval( -33), eg: Eval( -23) },
        EvalPair { mg: Eval(  -3), eg: Eval(  -9) },
        EvalPair { mg: Eval( -14), eg: Eval( -23) },
        EvalPair { mg: Eval( -21), eg: Eval(  -5) },
        EvalPair { mg: Eval( -13), eg: Eval(  -9) },
        EvalPair { mg: Eval( -12), eg: Eval( -16) },
        EvalPair { mg: Eval( -39), eg: Eval(  -5) },
        EvalPair { mg: Eval( -21), eg: Eval( -17) },
        EvalPair { mg: Eval(   4), eg: Eval( -14) },
        EvalPair { mg: Eval(  15), eg: Eval( -18) },
        EvalPair { mg: Eval(  16), eg: Eval(  -7) },
        EvalPair { mg: Eval(   0), eg: Eval(  -1) },
        EvalPair { mg: Eval(   7), eg: Eval(   4) },
        EvalPair { mg: Eval(  21), eg: Eval(  -9) },
        EvalPair { mg: Eval(  33), eg: Eval( -15) },
        EvalPair { mg: Eval(   1), eg: Eval( -27) },
        EvalPair { mg: Eval(   0), eg: Eval( -12) },
        EvalPair { mg: Eval(  15), eg: Eval(  -3) },
        EvalPair { mg: Eval(  15), eg: Eval(   8) },
        EvalPair { mg: Eval(  15), eg: Eval(  10) },
        EvalPair { mg: Eval(  14), eg: Eval(  13) },
        EvalPair { mg: Eval(  27), eg: Eval(   3) },
        EvalPair { mg: Eval(  18), eg: Eval(  -7) },
        EvalPair { mg: Eval(  10), eg: Eval( -15) },
        EvalPair { mg: Eval(  -6), eg: Eval(  -6) },
        EvalPair { mg: Eval(  13), eg: Eval(   3) },
        EvalPair { mg: Eval(  13), eg: Eval(  13) },
        EvalPair { mg: Eval(  26), eg: Eval(  19) },
        EvalPair { mg: Eval(  34), eg: Eval(   7) },
        EvalPair { mg: Eval(  12), eg: Eval(  10) },
        EvalPair { mg: Eval(  10), eg: Eval(  -3) },
        EvalPair { mg: Eval(   4), eg: Eval(  -9) },
        EvalPair { mg: Eval(  -4), eg: Eval(  -3) },
        EvalPair { mg: Eval(   5), eg: Eval(   9) },
        EvalPair { mg: Eval(  19), eg: Eval(  12) },
        EvalPair { mg: Eval(  50), eg: Eval(   9) },
        EvalPair { mg: Eval(  37), eg: Eval(  14) },
        EvalPair { mg: Eval(  37), eg: Eval(  10) },
        EvalPair { mg: Eval(   7), eg: Eval(   3) },
        EvalPair { mg: Eval(  -2), eg: Eval(   2) },
        EvalPair { mg: Eval( -16), eg: Eval(   2) },
        EvalPair { mg: Eval(  37), eg: Eval(  -8) },
        EvalPair { mg: Eval(  43), eg: Eval(   0) },
        EvalPair { mg: Eval(  40), eg: Eval(  -1) },
        EvalPair { mg: Eval(  35), eg: Eval(  -2) },
        EvalPair { mg: Eval(  50), eg: Eval(   6) },
        EvalPair { mg: Eval(  37), eg: Eval(   0) },
        EvalPair { mg: Eval(  -2), eg: Eval(   4) },
        EvalPair { mg: Eval( -26), eg: Eval(  -8) },
        EvalPair { mg: Eval(  16), eg: Eval(  -4) },
        EvalPair { mg: Eval( -18), eg: Eval(   7) },
        EvalPair { mg: Eval( -13), eg: Eval( -12) },
        EvalPair { mg: Eval(  30), eg: Eval(  -3) },
        EvalPair { mg: Eval(  59), eg: Eval( -13) },
        EvalPair { mg: Eval(  18), eg: Eval(  -4) },
        EvalPair { mg: Eval( -47), eg: Eval( -14) },
        EvalPair { mg: Eval( -29), eg: Eval( -14) },
        EvalPair { mg: Eval(   4), eg: Eval( -21) },
        EvalPair { mg: Eval( -82), eg: Eval( -11) },
        EvalPair { mg: Eval( -37), eg: Eval(  -8) },
        EvalPair { mg: Eval( -25), eg: Eval(  -7) },
        EvalPair { mg: Eval( -42), eg: Eval(  -9) },
        EvalPair { mg: Eval(   7), eg: Eval( -17) },
        EvalPair { mg: Eval(  -8), eg: Eval( -24) },
    ],
    [
        EvalPair { mg: Eval( -19), eg: Eval(  -9) },
        EvalPair { mg: Eval( -13), eg: Eval(   2) },
        EvalPair { mg: Eval(   1), eg: Eval(   3) },
        EvalPair { mg: Eval(  17), eg: Eval(  -1) },
        EvalPair { mg: Eval(  16), eg: Eval(  -5) },
        EvalPair { mg: Eval(   7), eg: Eval( -13) },
        EvalPair { mg: Eval( -37), eg: Eval(   4) },
        EvalPair { mg: Eval( -26), eg: Eval( -20) },
        EvalPair { mg: Eval( -44), eg: Eval(  -6) },
        EvalPair { mg: Eval( -16), eg: Eval(  -6) },
        EvalPair { mg: Eval( -20), eg: Eval(   0) },
        EvalPair { mg: Eval(  -9), eg: Eval(   2) },
        EvalPair { mg: Eval(  -1), eg: Eval(  -9) },
        EvalPair { mg: Eval(  11), eg: Eval(  -9) },
        EvalPair { mg: Eval(  -6), eg: Eval( -11) },
        EvalPair { mg: Eval( -71), eg: Eval(  -3) },
        EvalPair { mg: Eval( -45), eg: Eval(  -4) },
        EvalPair { mg: Eval( -25), eg: Eval(   0) },
        EvalPair { mg: Eval( -16), eg: Eval(  -5) },
        EvalPair { mg: Eval( -17), eg: Eval(  -1) },
        EvalPair { mg: Eval(   3), eg: Eval(  -7) },
        EvalPair { mg: Eval(   0), eg: Eval( -12) },
        EvalPair { mg: Eval(  -5), eg: Eval(  -8) },
        EvalPair { mg: Eval( -33), eg: Eval( -16) },
        EvalPair { mg: Eval( -36), eg: Eval(   3) },
        EvalPair { mg: Eval( -26), eg: Eval(   5) },
        EvalPair { mg: Eval( -12), eg: Eval(   8) },
        EvalPair { mg: Eval(  -1), eg: Eval(   4) },
        EvalPair { mg: Eval(   9), eg: Eval(  -5) },
        EvalPair { mg: Eval(  -7), eg: Eval(  -6) },
        EvalPair { mg: Eval(   6), eg: Eval(  -8) },
        EvalPair { mg: Eval( -23), eg: Eval( -11) },
        EvalPair { mg: Eval( -24), eg: Eval(   4) },
        EvalPair { mg: Eval( -11), eg: Eval(   3) },
        EvalPair { mg: Eval(   7), eg: Eval(  13) },
        EvalPair { mg: Eval(  26), eg: Eval(   1) },
        EvalPair { mg: Eval(  24), eg: Eval(   2) },
        EvalPair { mg: Eval(  35), eg: Eval(   1) },
        EvalPair { mg: Eval(  -8), eg: Eval(  -1) },
        EvalPair { mg: Eval( -20), eg: Eval(   2) },
        EvalPair { mg: Eval(  -5), eg: Eval(   7) },
        EvalPair { mg: Eval(  19), eg: Eval(   7) },
        EvalPair { mg: Eval(  26), eg: Eval(   7) },
        EvalPair { mg: Eval(  36), eg: Eval(   5) },
        EvalPair { mg: Eval(  17), eg: Eval(   4) },
        EvalPair { mg: Eval(  45), eg: Eval(  -3) },
        EvalPair { mg: Eval(  61), eg: Eval(  -5) },
        EvalPair { mg: Eval(  16), eg: Eval(  -3) },
        EvalPair { mg: Eval(  27), eg: Eval(  11) },
        EvalPair { mg: Eval(  32), eg: Eval(  13) },
        EvalPair { mg: Eval(  58), eg: Eval(  13) },
        EvalPair { mg: Eval(  62), eg: Eval(  11) },
        EvalPair { mg: Eval(  80), eg: Eval(  -3) },
        EvalPair { mg: Eval(  67), eg: Eval(   3) },
        EvalPair { mg: Eval(  26), eg: Eval(   8) },
        EvalPair { mg: Eval(  44), eg: Eval(   3) },
        EvalPair { mg: Eval(  32), eg: Eval(  13) },
        EvalPair { mg: Eval(  42), eg: Eval(  10) },
        EvalPair { mg: Eval(  32), eg: Eval(  18) },
        EvalPair { mg: Eval(  51), eg: Eval(  15) },
        EvalPair { mg: Eval(  63), eg: Eval(  12) },
        EvalPair { mg: Eval(   9), eg: Eval(  12) },
        EvalPair { mg: Eval(  31), eg: Eval(   8) },
        EvalPair { mg: Eval(  43), eg: Eval(   5) },
    ],
    [
        EvalPair { mg: Eval(  -1), eg: Eval( -33) },
        EvalPair { mg: Eval( -18), eg: Eval( -28) },
        EvalPair { mg: Eval(  -9), eg: Eval( -22) },
        EvalPair { mg: Eval(  10), eg: Eval( -43) },
        EvalPair { mg: Eval( -15), eg: Eval(  -5) },
        EvalPair { mg: Eval( -25), eg: Eval( -32) },
        EvalPair { mg: Eval( -31), eg: Eval( -20) },
        EvalPair { mg: Eval( -50), eg: Eval( -41) },
        EvalPair { mg: Eval( -35), eg: Eval( -22) },
        EvalPair { mg: Eval(  -8), eg: Eval( -23) },
        EvalPair { mg: Eval(  11), eg: Eval( -30) },
        EvalPair { mg: Eval(   2), eg: Eval( -16) },
        EvalPair { mg: Eval(   8), eg: Eval( -16) },
        EvalPair { mg: Eval(  15), eg: Eval( -23) },
        EvalPair { mg: Eval(  -3), eg: Eval( -36) },
        EvalPair { mg: Eval(   1), eg: Eval( -32) },
        EvalPair { mg: Eval( -14), eg: Eval( -16) },
        EvalPair { mg: Eval(   2), eg: Eval( -27) },
        EvalPair { mg: Eval( -11), eg: Eval(  15) },
        EvalPair { mg: Eval(  -2), eg: Eval(   6) },
        EvalPair { mg: Eval(  -5), eg: Eval(   9) },
        EvalPair { mg: Eval(   2), eg: Eval(  17) },
        EvalPair { mg: Eval(  14), eg: Eval(  10) },
        EvalPair { mg: Eval(   5), eg: Eval(   5) },
        EvalPair { mg: Eval(  -9), eg: Eval( -18) },
        EvalPair { mg: Eval( -26), eg: Eval(  28) },
        EvalPair { mg: Eval(  -9), eg: Eval(  19) },
        EvalPair { mg: Eval( -10), eg: Eval(  47) },
        EvalPair { mg: Eval(  -2), eg: Eval(  31) },
        EvalPair { mg: Eval(  -4), eg: Eval(  34) },
        EvalPair { mg: Eval(   3), eg: Eval(  39) },
        EvalPair { mg: Eval(  -3), eg: Eval(  23) },
        EvalPair { mg: Eval( -27), eg: Eval(   3) },
        EvalPair { mg: Eval( -27), eg: Eval(  22) },
        EvalPair { mg: Eval( -16), eg: Eval(  24) },
        EvalPair { mg: Eval( -16), eg: Eval(  45) },
        EvalPair { mg: Eval(  -1), eg: Eval(  57) },
        EvalPair { mg: Eval(  17), eg: Eval(  40) },
        EvalPair { mg: Eval(  -2), eg: Eval(  57) },
        EvalPair { mg: Eval(   1), eg: Eval(  36) },
        EvalPair { mg: Eval( -13), eg: Eval( -20) },
        EvalPair { mg: Eval( -17), eg: Eval(   6) },
        EvalPair { mg: Eval(   7), eg: Eval(   9) },
        EvalPair { mg: Eval(   8), eg: Eval(  49) },
        EvalPair { mg: Eval(  29), eg: Eval(  47) },
        EvalPair { mg: Eval(  56), eg: Eval(  35) },
        EvalPair { mg: Eval(  47), eg: Eval(  19) },
        EvalPair { mg: Eval(  57), eg: Eval(   9) },
        EvalPair { mg: Eval( -24), eg: Eval( -17) },
        EvalPair { mg: Eval( -39), eg: Eval(  20) },
        EvalPair { mg: Eval(  -5), eg: Eval(  32) },
        EvalPair { mg: Eval(   1), eg: Eval(  41) },
        EvalPair { mg: Eval( -16), eg: Eval(  58) },
        EvalPair { mg: Eval(  57), eg: Eval(  25) },
        EvalPair { mg: Eval(  28), eg: Eval(  30) },
        EvalPair { mg: Eval(  54), eg: Eval(   0) },
        EvalPair { mg: Eval( -28), eg: Eval(  -9) },
        EvalPair { mg: Eval(   0), eg: Eval(  22) },
        EvalPair { mg: Eval(  29), eg: Eval(  22) },
        EvalPair { mg: Eval(  12), eg: Eval(  27) },
        EvalPair { mg: Eval(  59), eg: Eval(  27) },
        EvalPair { mg: Eval(  44), eg: Eval(  19) },
        EvalPair { mg: Eval(  43), eg: Eval(  10) },
        EvalPair { mg: Eval(  45), eg: Eval(  20) },
    ],
    [
        EvalPair { mg: Eval( -15), eg: Eval( -53) },
        EvalPair { mg: Eval(  36), eg: Eval( -34) },
        EvalPair { mg: Eval(  12), eg: Eval( -21) },
        EvalPair { mg: Eval( -54), eg: Eval( -11) },
        EvalPair { mg: Eval(   8), eg: Eval( -28) },
        EvalPair { mg: Eval( -28), eg: Eval( -14) },
        EvalPair { mg: Eval(  24), eg: Eval( -24) },
        EvalPair { mg: Eval(  14), eg: Eval( -43) },
        EvalPair { mg: Eval(   1), eg: Eval( -27) },
        EvalPair { mg: Eval(   7), eg: Eval( -11) },
        EvalPair { mg: Eval(  -8), eg: Eval(   4) },
        EvalPair { mg: Eval( -64), eg: Eval(  13) },
        EvalPair { mg: Eval( -43), eg: Eval(  14) },
        EvalPair { mg: Eval( -16), eg: Eval(   4) },
        EvalPair { mg: Eval(   9), eg: Eval(  -5) },
        EvalPair { mg: Eval(   8), eg: Eval( -17) },
        EvalPair { mg: Eval( -14), eg: Eval( -19) },
        EvalPair { mg: Eval( -14), eg: Eval(  -3) },
        EvalPair { mg: Eval( -22), eg: Eval(  11) },
        EvalPair { mg: Eval( -46), eg: Eval(  21) },
        EvalPair { mg: Eval( -44), eg: Eval(  23) },
        EvalPair { mg: Eval( -30), eg: Eval(  16) },
        EvalPair { mg: Eval( -15), eg: Eval(   7) },
        EvalPair { mg: Eval( -27), eg: Eval(  -9) },
        EvalPair { mg: Eval( -49), eg: Eval( -18) },
        EvalPair { mg: Eval(  -1), eg: Eval(  -4) },
        EvalPair { mg: Eval( -27), eg: Eval(  21) },
        EvalPair { mg: Eval( -39), eg: Eval(  24) },
        EvalPair { mg: Eval( -46), eg: Eval(  27) },
        EvalPair { mg: Eval( -44), eg: Eval(  23) },
        EvalPair { mg: Eval( -33), eg: Eval(   9) },
        EvalPair { mg: Eval( -51), eg: Eval( -11) },
        EvalPair { mg: Eval( -17), eg: Eval(  -8) },
        EvalPair { mg: Eval( -20), eg: Eval(  22) },
        EvalPair { mg: Eval( -12), eg: Eval(  24) },
        EvalPair { mg: Eval( -27), eg: Eval(  27) },
        EvalPair { mg: Eval( -30), eg: Eval(  26) },
        EvalPair { mg: Eval( -25), eg: Eval(  33) },
        EvalPair { mg: Eval( -14), eg: Eval(  26) },
        EvalPair { mg: Eval( -36), eg: Eval(   3) },
        EvalPair { mg: Eval(  -9), eg: Eval(  10) },
        EvalPair { mg: Eval(  24), eg: Eval(  17) },
        EvalPair { mg: Eval(   2), eg: Eval(  23) },
        EvalPair { mg: Eval( -16), eg: Eval(  15) },
        EvalPair { mg: Eval( -20), eg: Eval(  20) },
        EvalPair { mg: Eval(   6), eg: Eval(  45) },
        EvalPair { mg: Eval(  22), eg: Eval(  44) },
        EvalPair { mg: Eval( -22), eg: Eval(  13) },
        EvalPair { mg: Eval(  29), eg: Eval( -12) },
        EvalPair { mg: Eval(  -1), eg: Eval(  17) },
        EvalPair { mg: Eval( -20), eg: Eval(  14) },
        EvalPair { mg: Eval(  -7), eg: Eval(  17) },
        EvalPair { mg: Eval(  -8), eg: Eval(  17) },
        EvalPair { mg: Eval(  -4), eg: Eval(  38) },
        EvalPair { mg: Eval( -38), eg: Eval(  23) },
        EvalPair { mg: Eval( -29), eg: Eval(  11) },
        EvalPair { mg: Eval( -65), eg: Eval( -74) },
        EvalPair { mg: Eval(  23), eg: Eval( -35) },
        EvalPair { mg: Eval(  16), eg: Eval( -18) },
        EvalPair { mg: Eval( -15), eg: Eval( -18) },
        EvalPair { mg: Eval( -56), eg: Eval( -11) },
        EvalPair { mg: Eval( -34), eg: Eval(  15) },
        EvalPair { mg: Eval(   2), eg: Eval(   4) },
        EvalPair { mg: Eval(  13), eg: Eval( -17) },
    ],
];

static mut PIECE_SQUARE_TABLES: [[EvalPair; Square::COUNT]; Piece::COUNT] =
    [[EvalPair { mg: Eval(0), eg: Eval(0) }; Square::COUNT]; Piece::COUNT];

/// Initialize the evaluation module
pub fn initialize() {
    initialize_piece_square_tables();
}

/// Initializes the piece-square tables for position evaluation.
///
/// # Purpose
/// Sets up the position-dependent values for each piece type on each square of the board. These tables are used to
/// evaluate piece positioning during different game phases.
fn initialize_piece_square_tables() {
    for piece_type in PieceType::ALL {
        for square in Square::ALL {
            let mirrored_square = square.mirror();
            let white_piece = Piece::new(Color::White, piece_type);
            let black_piece = Piece::new(Color::Black, piece_type);
            let eval = PIECE_TYPE_SQUARE_TABLES[usize::from(piece_type)][usize::from(square)]
                + PIECE_TYPE_VALUES[usize::from(piece_type)];
            unsafe {
                PIECE_SQUARE_TABLES[usize::from(white_piece)][usize::from(square)] = eval;

                PIECE_SQUARE_TABLES[usize::from(black_piece)][usize::from(mirrored_square)] = -eval;
            };
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
    PIECES_GAME_PHASE[usize::from(piece)]
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
    let mg_phase: i32 = min(position.game_phase(), MAX_GAME_PHASE) as i32;
    let eg_phase: i32 = MAX_GAME_PHASE as i32 - mg_phase;

    let incremental_eval = position.incremental_eval();
    Eval::from(
        (mg_phase * i32::from(incremental_eval.mg) + eg_phase * i32::from(incremental_eval.eg))
            / (MAX_GAME_PHASE as i32),
    )
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_remove_ply_from_mat() {
        assert_eq!(Eval::new_mat(2), Eval::new_mat(5).remove_ply_from_mat(3));
        assert_eq!(-Eval::new_mat(2), (-Eval::new_mat(5)).remove_ply_from_mat(3));
        assert_eq!(Eval::new(-30), Eval::new(-30).remove_ply_from_mat(3));
    }
}
