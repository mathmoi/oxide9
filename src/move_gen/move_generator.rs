use crate::{
    eval::{get_piece_square_value, Eval},
    piece::{Color, PieceType},
    position::Position,
    r#move::{Move, MoveType},
};

use super::{
    generation::{generate_moves, MoveGenerationType},
    move_list::MoveList,
};

enum GenerationStep {
    TTMove,

    GenerateCaptures,
    DistributeCaptures,

    GenerateQuiet,
    DistributeQuiet,

    GenerateEvasion,
    DistributeEvasion,

    Done,
}

impl GenerationStep {
    fn next(&self) -> Self {
        match self {
            GenerationStep::TTMove => GenerationStep::GenerateCaptures,
            GenerationStep::GenerateCaptures => GenerationStep::DistributeCaptures,
            GenerationStep::DistributeCaptures => GenerationStep::GenerateQuiet,
            GenerationStep::GenerateQuiet => GenerationStep::DistributeQuiet,
            GenerationStep::DistributeQuiet => GenerationStep::Done,
            GenerationStep::GenerateEvasion => GenerationStep::DistributeEvasion,
            GenerationStep::DistributeEvasion => GenerationStep::Done,
            GenerationStep::Done => GenerationStep::Done,
        }
    }
}

/// The MoveGenerator struct is responsible for generating moves for a given position.
pub struct MoveGenerator {
    position: *const Position,
    list: MoveList,
    step: GenerationStep,
    qsearch: bool,
    tt_move: Option<Move>,
}

impl MoveGenerator {
    pub fn new(position: &Position, tt_move: Option<Move>, qsearch: bool) -> Self {
        MoveGenerator {
            position: position as *const Position,
            list: MoveList::default(),
            step: GenerationStep::TTMove,
            qsearch,
            tt_move,
        }
    }
}

impl Iterator for MoveGenerator {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.step {
                GenerationStep::TTMove => {
                    self.step = if unsafe { &*self.position }.is_check() {
                        GenerationStep::GenerateEvasion
                    } else {
                        GenerationStep::GenerateCaptures
                    };
                    if let Some(mv) = self.tt_move {
                        if unsafe { &*self.position }.is_pseudo_legal(mv) {
                            return Some(mv);
                        }
                    }
                }

                GenerationStep::GenerateCaptures => {
                    generate_moves::<{ MoveGenerationType::CAPTURES_VALUE }>(
                        unsafe { &*self.position },
                        &mut self.list,
                    );

                    // Loop to evaluate mvv_lva and remove the tt_move.
                    let mut index = 0;
                    loop {
                        if index >= self.list.len() {
                            break;
                        }
                        let mv = self.list.get_mut(index);

                        if Some(*mv) == self.tt_move {
                            self.list.swap_remove(index);
                            continue;
                        }

                        mv.set_eval(mvv_lva(*mv));
                        index += 1;
                    }

                    self.step = GenerationStep::DistributeCaptures;
                }

                GenerationStep::GenerateQuiet => {
                    if self.qsearch {
                        self.step = GenerationStep::Done;
                        return None;
                    }

                    generate_moves::<{ MoveGenerationType::QUIET_VALUE }>(unsafe { &*self.position }, &mut self.list);

                    // Loop to evaluate the moves and remove the tt_move.
                    let color_factor = if unsafe { &*self.position }.side_to_move() == Color::White { 1 } else { -1 };
                    let mut index = 0;
                    loop {
                        if index >= self.list.len() {
                            break;
                        }
                        let mv = self.list.get_mut(index);

                        if Some(*mv) == self.tt_move {
                            self.list.swap_remove(index);
                            continue;
                        }

                        mv.set_eval(color_factor * evaluate_quiet_move(*mv));
                        index += 1;
                    }

                    self.step = GenerationStep::DistributeQuiet;
                }

                GenerationStep::GenerateEvasion => {
                    generate_moves::<{ MoveGenerationType::EVASIONS_VALUE }>(
                        unsafe { &*self.position },
                        &mut self.list,
                    );
                    self.step = GenerationStep::DistributeEvasion;
                }

                GenerationStep::DistributeCaptures
                | GenerationStep::DistributeQuiet
                | GenerationStep::DistributeEvasion => {
                    if self.list.is_empty() {
                        self.step = self.step.next();
                        continue;
                    }
                    let mv = self.list.pop();
                    return Some(mv);
                }

                GenerationStep::Done => {
                    return None;
                }
            }
        }
    }
}

/// Calculates the MVV-LVA (Most Valuable Victim - Least Valuable Attacker) score for a move.
///
/// This function assigns a score based on the value of the captured piece (victim) and the piece making the capture
/// (attacker). Higher scores are given to captures where a high-value piece is captured by a low-value piece.
///
/// # Parameters
/// * `mv` - The move to evaluate
///
/// # Returns
/// A score representing the move's priority in move ordering based on the MVV-LVA heuristic. Higher values indicate
/// more promising captures.
///
/// # Panics
/// Panics if called with a non-capturing move.
fn mvv_lva(mv: Move) -> Eval {
    const VALUES: [Eval; 6] =
        [Eval::new(10), Eval::new(28), Eval::new(30), Eval::new(50), Eval::new(90), Eval::new(200)];

    let victim = match mv.move_type() {
        MoveType::Capture(capture) => VALUES[usize::from(capture.piece_type())],
        MoveType::EnPassant => VALUES[usize::from(PieceType::Pawn)],
        MoveType::Promotion(promotion) => VALUES[usize::from(promotion.piece_type())],
        MoveType::CapturePromotion { capture, promotion } => {
            VALUES[usize::from(capture.piece_type())] + VALUES[usize::from(promotion.piece_type())]
        }
        _ => unreachable!(),
    };
    let attacker = VALUES[usize::from(mv.piece().piece_type())];

    victim * 128 - attacker
}

/// Evaluates a non-capturing move based on the difference in piece-square values.
///
/// Calculates the positional value change of a piece when it moves from one square to another. Higher values indicate
/// moves that improve the piece's position on the board.
///
/// # Parameters
/// * `mv` - The quiet (non-capturing) move to evaluate
///
/// # Returns
/// An evaluation score representing the positional improvement of the move. Positive values indicate the piece is
/// moving to a better square.
fn evaluate_quiet_move(mv: Move) -> Eval {
    let piece = mv.piece();
    get_piece_square_value(piece, mv.to_square()).mg() - get_piece_square_value(piece, mv.from_square()).mg()
}
