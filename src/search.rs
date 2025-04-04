use std::time::{Duration, Instant};

use crate::{
    eval::{evaluate, Eval},
    move_gen::move_generator::MoveGenerator,
    position::Position,
    r#move::Move,
};

/// The SearchStats struct holds statistics about the search process.
pub struct SearchStats {
    /// The number of moves possible at the root of the search tree.
    pub moves_at_root: u64,

    /// The number of nodes searched. This excludes quiescence nodes.
    pub nodes: u64,

    /// The number of quiescence nodes searched
    pub qnodes: u64,
}

impl Default for SearchStats {
    fn default() -> Self {
        SearchStats { moves_at_root: 0, nodes: 0, qnodes: 0 }
    }
}

/// Represents different types of search progress to be reported in the console or to the GUI.
///
/// This enum captures the various situations during a chess search where progress information needs to be displayed,
/// with appropriate data for each case.
///
/// # Variants
///
/// * `Iteration` - Regular progress update at the current search depth
///   - `depth`: Current search depth
///   - `elapsed`: Time spent searching so far
///   - `score`: Current best evaluation
///   - `nodes`: Number of nodes searched
///   - `pv`: Principal variation (sequence of best moves found)
///
/// * `NewBestMove` - A new best move has been found during the search
///   - `depth`: Search depth where the move was found
///   - `elapsed`: Time spent searching so far
///   - `score`: Evaluation of the new best move
///   - `nodes`: Number of nodes searched
///   - `pv`: Updated principal variation
///
/// * `NewMoveAtRoot` - Search has started analyzing a new move at the root position
///   - `depth`: Current search depth
///   - `elapsed`: Time spent searching so far
///   - `nodes`: Number of nodes searched
///   - `move_number`: Current move number being searched
///   - `move_count`: Total number of moves to search
///   - `mv`: The move currently being searched
pub enum ProgressType<'a> {
    Iteration { depth: u16, elapsed: Duration, score: Eval, nodes: u64, pv: &'a [Move] },
    NewBestMove { depth: u16, elapsed: Duration, score: Eval, nodes: u64, pv: &'a [Move] },
    NewMoveAtRoot { depth: u16, elapsed: Duration, nodes: u64, move_number: u64, move_count: u64, mv: Move },
}

pub type ProgressCallback = fn(progress_type: ProgressType);

/// Represents a chess position search operation.
pub struct Search<'a> {
    /// Mutable reference to the chess position being searched. While the position is mutated during search, it will be
    /// restored to its original state after the search is complete.
    position: &'a mut Position,

    /// Collection of statistics about the current search
    stats: SearchStats,

    /// Maximum depth to search
    depth: u16,

    /// Optional callback for reporting search progress
    progress: Option<ProgressCallback>,

    /// Timestamp when the search was started, it is Nonot when the search is not started yet.
    start_time: Option<Instant>,
}

impl<'a> Search<'a> {
    /// Creates a new search instance with the given position and depth.
    ///
    /// # Parameters
    /// * `position` - A mutable reference to the chess position to be searched. While the position will be mutated
    ///   during search it will be restored to its original state after the search is complete.
    /// * `depth` - The maximum depth (in half-moves) to search to
    ///
    /// # Returns
    /// A new Search instance configured with the specified position and depth
    pub fn new(position: &'a mut Position, depth: u16, progress: Option<ProgressCallback>) -> Search<'a> {
        Search { position, stats: SearchStats::default(), depth, progress, start_time: None }
    }

    /// Returns the search statistics.
    pub fn stats(&self) -> &SearchStats {
        &self.stats
    }

    /// Starts the search and returns the principal variation.
    ///
    /// This method initiates a search to the depth specified during initialization, finding the best sequence of moves
    /// (principal variation) from the current position.
    ///
    /// # Returns
    /// A vector of moves representing the principal variation (best line of play). The moves in the vector are in reverse
    /// order, with the best move to play in the current position being the last move in the vector.
    pub fn start(&mut self) -> Vec<Move> {
        self.start_time = Some(Instant::now());

        self.stats.moves_at_root = Self::count_moves(self.position);

        self.iterative_deepening()
    }

    /// Returns the number of legal moves available from a position.
    ///
    /// Generates all possible moves from the current position and counts only those that are legal according to chess
    /// rules.
    ///
    /// # Parameters
    ///
    /// * `position` - A reference to the Position to analyze
    ///
    /// # Returns
    ///
    /// * The total count of legal moves available in the given position
    fn count_moves(position: &Position) -> u64 {
        let move_generator = MoveGenerator::new(position, false);
        move_generator.filter(|mv| position.is_legal(*mv)).count() as u64
    }

    /// Performs an iterative deepening search to the specified maximum depth.
    ///
    /// Conducts a series of alpha-beta searches with increasing depth from 1 to the maximum depth specified. Each
    /// iteration builds upon knowledge gained from previous iterations, resulting in more efficient pruning.
    ///
    /// Progress is reported after each depth iteration if a progress callback is configured.
    ///
    /// # Returns
    ///
    /// * The principal variation (PV) representing the best sequence of moves found during the search
    pub fn iterative_deepening(&mut self) -> Vec<Move> {
        let mut pv = Vec::new();
        for depth in 1..=self.depth {
            let score = self.search::<true, false>(depth, Eval::MIN, Eval::MAX, &mut pv);
            if let Some(progress) = self.progress {
                let start_time = self.start_time.expect("The timer should be started");
                progress(ProgressType::Iteration {
                    depth,
                    elapsed: start_time.elapsed(),
                    score,
                    nodes: self.stats.nodes + self.stats.qnodes,
                    pv: &pv,
                });
            }
        }
        pv
    }

    // NEXT : Spliter la recherche en search_root, search et qsearch. search_root doit réutiliser la liste de coups
    // racine qui sera réordonnée par iterative_deepening.

    /// Recursively searches the position to the given depth.
    ///
    /// This method implements a basic alpha-beta search algorithm that explores
    /// the game tree to find the best move sequence from the current position.
    ///
    /// # Parameters
    /// * `depth` - Current remaining search depth in half-moves
    /// * `alpha` - The current alpha value for alpha-beta pruning
    /// * `beta` - The current beta value for alpha-beta pruning
    /// * `pv` - Mutable vector to store the principal variation (best line of moves)
    ///
    /// # Returns
    /// The evaluation score of the position after searching to the specified depth.
    /// Higher positive values indicate an advantage for the side to move.
    ///
    /// # Note
    /// This is an internal recursive method used by the `start` method.
    fn search<const ROOT: bool, const QSEARCH: bool>(
        &mut self,
        depth: u16,
        alpha: Eval,
        beta: Eval,
        pv: &mut Vec<Move>,
    ) -> Eval {
        let mut alpha = alpha; // Make alpha mutable locally

        // TODO : Is Vec really performant for pv structure?
        // TODO : Should we use a stack to store the PV and others things?

        match QSEARCH {
            true => self.stats.qnodes += 1,
            false => self.stats.nodes += 1,
        }

        // In the qsearch we evaluate the stand pat (stop capturing) option. If the stand pat is better than beta, we
        // stop the search and return beta (beta cut-off). If the stand pat is better than alpha, we update alpha with the
        // stand pat value.
        if QSEARCH {
            let stand_pat = evaluate(&self.position);
            if stand_pat >= beta {
                return beta;
            }
            if stand_pat > alpha {
                alpha = stand_pat;
            }
        }

        let move_generator = MoveGenerator::new(self.position, QSEARCH);
        let mut move_searched: u64 = 0;
        for mv in move_generator {
            if self.position.is_legal(mv) {
                if ROOT {
                    if let Some(progress) = self.progress {
                        progress(ProgressType::NewMoveAtRoot {
                            depth,
                            elapsed: self.start_time.expect("The timer should be started").elapsed(),
                            nodes: self.stats.nodes + self.stats.qnodes,
                            move_number: move_searched + 1,
                            move_count: self.stats.moves_at_root,
                            mv,
                        });
                    }
                }

                self.position.make(mv);
                let mut local_pv = Vec::new();
                let score = if QSEARCH || depth == 1 {
                    -self.search::<false, true>(0, -beta, -alpha, &mut local_pv)
                } else {
                    -self.search::<false, false>(depth - 1, -beta, -alpha, &mut local_pv)
                };
                self.position.unmake();

                if score >= beta {
                    return beta;
                }

                if score > alpha {
                    alpha = score;

                    if !QSEARCH {
                        *pv = local_pv;
                        pv.push(mv);
                    }

                    // If we are at the root of the search, we report the new best move
                    if ROOT && 1 < move_searched {
                        if let Some(progress) = self.progress {
                            progress(ProgressType::NewBestMove {
                                depth,
                                elapsed: self.start_time.expect("The timer should be started").elapsed(),
                                score,
                                nodes: self.stats.nodes + self.stats.qnodes,
                                pv,
                            });
                        }
                    }
                }
            }

            move_searched += 1;
        }

        alpha
    }
}
