use crate::{
    eval::{get_piece_square_value, Eval},
    piece::PieceType,
    position::Position,
    r#move::{Move, MoveType},
};

use super::{
    generation::{generate_moves, MoveGenerationType},
    move_list::MoveList,
};

enum GenerationStep {
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
}

impl MoveGenerator {
    pub fn new(position: &Position, qsearch: bool) -> Self {
        let step = if position.is_check() { GenerationStep::GenerateEvasion } else { GenerationStep::GenerateCaptures };

        MoveGenerator { position: position as *const Position, list: MoveList::new(), step, qsearch }
    }
}

impl Iterator for MoveGenerator {
    type Item = Move;

    fn next(&mut self) -> Option<Self::Item> {
        loop {
            match self.step {
                GenerationStep::GenerateCaptures => {
                    generate_moves::<{ MoveGenerationType::CAPTURES_VALUE }>(
                        unsafe { &*self.position },
                        &mut self.list,
                    );
                    self.list.iter_mut().for_each(|mv| mv.set_eval(mvv_lva(*mv)));
                    self.step = GenerationStep::DistributeCaptures;
                }

                GenerationStep::GenerateQuiet => {
                    if self.qsearch {
                        self.step = GenerationStep::Done;
                        return None;
                    }

                    generate_moves::<{ MoveGenerationType::QUIET_VALUE }>(unsafe { &*self.position }, &mut self.list);
                    self.list.iter_mut().for_each(|mv| mv.set_eval(evaluate_quiet_move(*mv)));
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
                    if self.list.len() == 0 {
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
            // TODO: Is this correct?
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
