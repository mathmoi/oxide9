use std::time::{Duration, Instant};

use crate::{
    eval::{evaluate, Eval},
    move_gen::{move_generator::MoveGenerator, move_list::MoveList},
    position::Position,
    r#move::Move,
    time::TimeManager,
};

/// The SearchStats struct holds statistics about the search process.
pub struct SearchStats {
    /// The number of nodes searched. This excludes quiescence nodes.
    pub nodes: u64,

    /// The number of quiescence nodes searched
    pub qnodes: u64,
}

impl Default for SearchStats {
    fn default() -> Self {
        SearchStats { nodes: 0, qnodes: 0 }
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
#[derive(Debug)]
pub enum ProgressType<'a> {
    Iteration { depth: u16, elapsed: Duration, score: Eval, nodes: u64, pv: &'a [Move] },
    NewBestMove { depth: u16, elapsed: Duration, score: Eval, nodes: u64, pv: &'a [Move] },
    NewMoveAtRoot { depth: u16, elapsed: Duration, nodes: u64, move_number: u64, move_count: u64, mv: Move },
    SearchFinished { mv: Move },
}

pub type ProgressCallback = fn(progress_type: ProgressType);

/// Represents a chess position search operation.
pub struct Search {
    /// Mutable reference to the chess position being searched. While the position is mutated during search, it will be
    /// restored to its original state after the search is complete.
    position: Position,

    /// List of moves at the root of the search tree. This is used to store the moves generated at the root level and
    /// keep the best moves at the beginning of the list between iterations.
    moves_at_root: MoveList,

    /// Collection of statistics about the current search
    stats: SearchStats,

    /// Maximum depth to search
    max_depth: u16,

    /// Callback for reporting search progress
    progress: ProgressCallback,

    /// Timestamp when the search was started, it is Nonot when the search is not started yet.
    start_time: Option<Instant>,

    /// Time manager to control the search time
    time_manager: TimeManager,
}

impl Search {
    /// Creates a new search instance with the given position and depth.
    ///
    /// # Parameters
    /// * `position` - A mutable reference to the chess position to be searched. While the position will be mutated
    ///   during search it will be restored to its original state after the search is complete.
    /// * `time_manager` - The time manager to control the search time
    /// * `max_depth` - The maximum depth (in half-moves) to search to
    ///
    /// # Returns
    /// A new Search instance configured with the specified position and depth
    pub fn new(position: Position, max_depth: u16, time_manager: TimeManager, progress: ProgressCallback) -> Search {
        let moves_at_root = Self::generate_moves_at_root(&position);
        Search {
            position,
            moves_at_root,
            stats: SearchStats::default(),
            max_depth,
            progress,
            start_time: None,
            time_manager,
        }
    }

    /// Generates all legal moves for the current position at the root level of the search tree.
    ///
    /// This method populates the internal `moves_at_root` collection with all legal moves that can be made from the
    /// current position.
    fn generate_moves_at_root(position: &Position) -> MoveList {
        let move_generator = MoveGenerator::new(position, false);
        move_generator.filter(|mv| position.is_legal(*mv)).collect()
    }

    /// Returns the search statistics.
    pub fn stats(&self) -> &SearchStats {
        &self.stats
    }

    /// Starts the search for the best moves from the current position.
    pub fn start(&mut self) {
        self.start_time = Some(Instant::now());
        self.run();
    }

    /// Executes the search process and reports the final result.
    fn run(&mut self) {
        let pv = self.iterative_deepening();
        if let Some(best_move) = pv.last() {
            (self.progress)(ProgressType::SearchFinished { mv: *best_move });
        }
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
    fn iterative_deepening(&mut self) -> Vec<Move> {
        let mut pv = Vec::new();
        for depth in 2..=self.max_depth {
            if !self.time_manager.can_start_iteration() {
                break;
            }

            self.time_manager.iteration_started();

            let score = self.search_root(depth, &mut pv);
            let start_time = self.start_time.expect("The timer should be started");
            (self.progress)(ProgressType::Iteration {
                depth,
                elapsed: start_time.elapsed(),
                score,
                nodes: self.stats.nodes + self.stats.qnodes,
                pv: &pv,
            });

            // Put the best move at the beginning of the list
            if !pv.is_empty() {
                let best_move = pv.last().expect("The PV should not be empty");
                self.moves_at_root.move_front(*best_move);
            }

            self.time_manager.iteration_finished();
        }
        pv
    }

    /// Searches the root position to a specified depth and builds the principal variation.
    ///
    /// Evaluates all legal moves at the current position and selects the best move according to alpha-beta search.
    /// Reports progress through callback functions during the search process. q
    /// # Parameters
    /// * `depth` - The maximum search depth in half-moves
    /// * `pv` - Mutable vector to store the principal variation (best line of moves)
    ///
    /// # Returns
    /// The evaluation score of the best move found. Higher positive values indicate an advantage for the side to move.
    fn search_root(&mut self, depth: u16, pv: &mut Vec<Move>) -> Eval {
        let mut alpha = Eval::MIN;
        let mut move_searched: u64 = 0;

        for mv in self.moves_at_root.clone().iter() {
            // We let the gui know that we are searching a new move at the root
            (self.progress)(ProgressType::NewMoveAtRoot {
                depth,
                elapsed: self.start_time.expect("The timer should be started").elapsed(),
                nodes: self.stats.nodes + self.stats.qnodes,
                move_number: move_searched + 1,
                move_count: self.moves_at_root.len() as u64,
                mv,
            });

            self.position.make(mv);
            let mut local_pv = Vec::new();
            let score = -self.search(depth - 1, Eval::MIN, -alpha, &mut local_pv);
            self.position.unmake();

            if score > alpha {
                alpha = score;
                *pv = local_pv;
                pv.push(mv);

                if 0 < move_searched {
                    (self.progress)(ProgressType::NewBestMove {
                        depth,
                        elapsed: self.start_time.expect("The timer should be started").elapsed(),
                        score,
                        nodes: self.stats.nodes + self.stats.qnodes,
                        pv: &pv,
                    });
                }
            }

            move_searched += 1;
        }

        alpha
    }

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
    fn search(&mut self, depth: u16, alpha: Eval, beta: Eval, pv: &mut Vec<Move>) -> Eval {
        let mut alpha = alpha; // Make alpha mutable locally

        // TODO : Is Vec really performant for pv structure?
        // TODO : Should we use a stack to store the PV and others things?

        self.stats.nodes += 1;

        let move_generator = MoveGenerator::new(&self.position, false);
        for mv in move_generator {
            if self.position.is_legal(mv) {
                self.position.make(mv);
                let mut local_pv = Vec::new();
                let score = if depth == 1 {
                    -self.qsearch(-beta, -alpha)
                } else {
                    -self.search(depth - 1, -beta, -alpha, &mut local_pv)
                };
                self.position.unmake();

                if score >= beta {
                    return beta;
                }

                if score > alpha {
                    alpha = score;
                    *pv = local_pv;
                    pv.push(mv);
                }
            }
        }

        alpha
    }

    /// Performs quiescence search to evaluate positions with tactical sequences.
    ///
    /// Quiescence search is a selective search that only explores capturing moves to reach a "quiet" position where
    /// tactical sequences are resolved. This helps avoid the horizon effect in chess engines.
    ///
    /// # Parameters
    /// * `alpha` - Lower bound of the search window
    /// * `beta` - Upper bound of the search window
    ///
    /// # Returns
    /// An evaluation score within the bounds of alpha and beta. A higher positive value indicates a better position for
    /// the side to move.
    fn qsearch(&mut self, alpha: Eval, beta: Eval) -> Eval {
        let mut alpha = alpha; // Make alpha mutable locally
        self.stats.qnodes += 1;

        // In the qsearch we evaluate the stand pat (stop capturing) option. If the stand pat is better than beta, we
        // stop the search and return beta (beta cut-off). If the stand pat is better than alpha, we update alpha with the
        // stand pat value.
        let stand_pat = evaluate(&self.position);
        if stand_pat >= beta {
            return beta;
        }
        if stand_pat > alpha {
            alpha = stand_pat;
        }

        // TODO : If we are in check we should probably search all moves? How to prevent perpetual check?
        let move_generator = MoveGenerator::new(&self.position, true);
        for mv in move_generator {
            if self.position.is_legal(mv) {
                self.position.make(mv);
                let score = -self.qsearch(-beta, -alpha);
                self.position.unmake();

                if score >= beta {
                    return beta;
                }

                if score > alpha {
                    alpha = score;
                }
            }
        }

        alpha
    }
}
