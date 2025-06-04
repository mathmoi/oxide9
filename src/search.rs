use std::{
    sync::{
        atomic::{AtomicBool, Ordering},
        Arc,
    },
    thread::{self, JoinHandle},
    time::{Duration, Instant},
};

use crate::{
    depth::Depth,
    eval::{evaluate, Eval},
    move_gen::{move_generator::MoveGenerator, move_list::MoveList},
    options::{Options, ReadOnlyOptions},
    position::Position,
    r#move::Move,
    time::TimeManager,
    tt::{EntryType, TranspositionTable},
};

struct SearchOptions {
    /// The depth to extend when the size to move is checked.
    check_extension: Depth,
}

impl Default for SearchOptions {
    fn default() -> Self {
        let options = Options::get();
        Self { check_extension: Depth::from_sixteenths(options.check_extension_sixteenths()) }
    }
}

/// The SearchStats struct holds statistics about the search process.
#[derive(Debug, Default, Clone)]
pub struct SearchStats {
    /// The number of nodes searched. This excludes quiescence nodes.
    pub total_nodes: u64,

    /// The number of quiescence nodes searched
    pub nodes: u64,

    /// The number of transposition table probes
    pub tt_probes: u64,

    /// The number of transposition table hits
    pub tt_probes_hit: u64,

    /// The of cuts due to transposition table
    pub tt_cuts: u64,

    /// The factor of used entries in the transposition table
    pub tt_load_factor: f64,
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
pub enum ProgressType {
    Iteration { depth: Depth, elapsed: Duration, score: Eval, nodes: u64, pv: Vec<Move> },
    NewBestMove { depth: Depth, elapsed: Duration, score: Eval, nodes: u64, pv: Vec<Move> },
    NewMoveAtRoot { depth: Depth, elapsed: Duration, nodes: u64, move_number: usize, move_count: u64, mv: Move },
    SearchFinished { mv: Move, elapsed: Duration, stats: SearchStats },
}

pub type ProgressCallback = fn(progress_type: ProgressType);

/// Represents a search operation that can be monitored and controlled.
///
/// Maintains a handle to an ongoing search process along with the ability to signal cancellation. This structure allows
/// for asynchronous search operations that can be interrupted when needed.
///
/// # Fields
/// * `cancelation_token` - Shared atomic flag that can signal the search to terminate
/// * `join_handle` - Handle to the thread where the search is executing
pub struct Search {
    cancelation_token: Arc<AtomicBool>,
    join_handle: JoinHandle<()>,
}

impl Search {
    /// Creates a new asynchronous search operation.
    ///
    /// Initializes a search in a separate thread based on the provided parameters. The search begins immediately upon
    /// creation and runs in the background.
    ///
    /// # Parameters
    /// * `position` - The chess position to analyze
    /// * `max_depth` - Maximum search depth in plies
    /// * `time_manager` - Controls time allocation for the search
    /// * `progress` - Callback to receive progress updates during the search
    /// * `transposition_table` - Transposition table for storing and retrieving previously evaluated positions
    ///
    /// # Returns
    /// A `Search` instance that allows monitoring and controlling the background search operation
    pub fn new<F>(
        position: Position,
        max_depth: Depth,
        time_manager: TimeManager,
        progress: Arc<F>,
        transposition_table: Arc<TranspositionTable>,
        cancelation_token: Arc<AtomicBool>,
    ) -> Self
    where
        F: Fn(ProgressType) + Send + Sync + 'static,
    {
        transposition_table.increment_generation();

        let mut search_thread = SearchThread::new(
            position,
            max_depth,
            time_manager,
            progress,
            cancelation_token.clone(),
            transposition_table,
            SearchOptions::default(),
        );

        let join_handle = thread::spawn(move || {
            search_thread.run();
        });

        Search { cancelation_token, join_handle }
    }

    /// Signals the search to stop as soon as possible.
    ///
    /// Sets the cancellation flag that will be detected by the search algorithm, allowing it to terminate at the next
    /// convenient opportunity. This method returns immediately and does not wait for the search to actually terminate.
    ///
    /// This is a non-blocking operation; to wait for the search to complete after stopping, call `join()`.
    pub fn stop(&self) {
        self.cancelation_token.store(true, Ordering::Relaxed);
    }

    /// Waits for the search thread to complete.
    ///
    /// Blocks the current thread until the search operation has fully terminated. If the search is still running, this
    /// will wait until it finishes or is stopped. This method consumes the Search object, making it unavailable after
    /// joining.
    ///
    /// # Panics
    /// Panics if the search thread has panicked or cannot be joined for any reason.
    pub fn join(self) {
        self.join_handle.join().expect("It should be possible to join the thread");
    }
}

/// Represents a search thread.
struct SearchThread<F> {
    /// Mutable reference to the chess position being searched. While the position is mutated during search, it will be
    /// restored to its original state after the search is complete.
    position: Position,

    /// List of moves at the root of the search tree. This is used to store the moves generated at the root level and
    /// keep the best moves at the beginning of the list between iterations.
    moves_at_root: MoveList,

    /// Collection of statistics about the current search
    stats: SearchStats,

    /// Maximum depth to search
    max_depth: Depth,

    /// Callback for reporting search progress
    progress: Arc<F>,

    /// Timestamp when the search was started, it is Nonot when the search is not started yet.
    start_time: Option<Instant>,

    /// Time manager to control the search time
    time_manager: TimeManager,

    /// Number of nodes searched before checking the time
    nodes_at_next_check: u64,

    /// Cancelation token that indicated if the search should be stopped. The search thread needs to check this token
    /// periodically and abort the search if it is set to true.
    cancelation_token: Arc<AtomicBool>,

    /// Indicate that the search is in the process of stopping.
    stopping: bool,

    /// Transposition table used for storing and retrieving previously evaluated positions.
    transposition_table: Arc<TranspositionTable>,

    /// Options for the search.
    options: SearchOptions,
}

impl<F> SearchThread<F>
where
    F: Fn(ProgressType) + Send + Sync + 'static,
{
    /// Creates a new search thread with the given parameters.
    ///
    /// Initializes a search thread that will analyze the provided position up to the specified depth. The search can be
    /// controlled via the cancellation token and will report progress through the provided callback.
    ///
    /// # Parameters
    /// * `position` - The chess position to analyze
    /// * `max_depth` - Maximum search depth in plies
    /// * `time_manager` - Controls time allocation for the search
    /// * `progress` - Callback function that receives search progress updates
    /// * `cancelation_token` - Shared atomic flag that can signal the search to terminate
    /// * `transposition_table` - Transposition table for storing and retrieving previously evaluated positions
    ///
    /// # Returns
    /// A configured `SearchThread` instance ready to execute the search
    pub fn new(
        position: Position,
        max_depth: Depth,
        time_manager: TimeManager,
        progress: Arc<F>,
        cancelation_token: Arc<AtomicBool>,
        transposition_table: Arc<TranspositionTable>,
        search_options: SearchOptions,
    ) -> Self
    where
        F: Fn(ProgressType) + Send + Sync + 'static,
    {
        let moves_at_root = Self::generate_moves_at_root(&position);
        SearchThread {
            position,
            moves_at_root,
            stats: SearchStats::default(),
            max_depth,
            progress,
            start_time: None,
            time_manager,
            nodes_at_next_check: 300000,
            cancelation_token,
            stopping: false,
            transposition_table,
            options: search_options,
        }
    }

    /// Generates all legal moves for the current position at the root level of the search tree.
    ///
    /// This method populates the internal `moves_at_root` collection with all legal moves that can be made from the
    /// current position.
    fn generate_moves_at_root(position: &Position) -> MoveList {
        let move_generator = MoveGenerator::new(position, None, false);
        move_generator.filter(|mv| position.is_legal(*mv)).collect()
    }

    /// Executes the search process and reports the final result.
    fn run(&mut self) {
        self.start_time = Some(Instant::now());
        let pv = self.iterative_deepening();
        if let Some(best_move) = pv.last() {
            self.stats.tt_load_factor = self.transposition_table.load_factor();
            (self.progress)(ProgressType::SearchFinished {
                mv: *best_move,
                elapsed: self.start_time.expect("The time should be started").elapsed(),
                stats: self.stats.clone(),
            });
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
        let mut depth = Depth::from_plies(2);
        while depth <= self.max_depth {
            if !self.time_manager.can_start_iteration() {
                break;
            }

            self.time_manager.iteration_started();

            let mut local_pv = Vec::new();
            let score = self.search_root(depth, &mut local_pv);

            // If we are aborting the search we break out of the loop immediately
            if self.stopping {
                break;
            }
            pv = local_pv;

            // Report the end if the iteration
            let start_time = self.start_time.expect("The timer should be started");
            (self.progress)(ProgressType::Iteration {
                depth,
                elapsed: start_time.elapsed(),
                score,
                nodes: self.stats.total_nodes,
                pv: pv.clone(),
            });

            // Put the best move at the beginning of the list
            if !pv.is_empty() {
                let best_move = pv.last().expect("The PV should not be empty");
                self.moves_at_root.move_front(*best_move);
            }

            self.time_manager.iteration_finished();

            depth += Depth::ONE_PLY;
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
    fn search_root(&mut self, depth: Depth, pv: &mut Vec<Move>) -> Eval {
        let mut alpha = Eval::MIN;

        for (move_searched, mv) in self.moves_at_root.clone().iter().enumerate() {
            // We let the gui know that we are searching a new move at the root
            (self.progress)(ProgressType::NewMoveAtRoot {
                depth,
                elapsed: self.start_time.expect("The timer should be started").elapsed(),
                nodes: self.stats.total_nodes,
                move_number: move_searched + 1,
                move_count: self.moves_at_root.len() as u64,
                mv,
            });

            self.position.make(Some(mv));
            let mut local_pv = Vec::new();
            let eval = -self.search(depth - Depth::ONE_PLY, 1, Eval::MIN, -alpha, &mut local_pv);
            self.position.unmake();

            // If we are aborting the search we return immediately
            if self.stopping {
                return Eval::default();
            }

            if eval > alpha {
                alpha = eval;
                *pv = local_pv;
                pv.push(mv);

                if 0 < move_searched {
                    (self.progress)(ProgressType::NewBestMove {
                        depth,
                        elapsed: self.start_time.expect("The timer should be started").elapsed(),
                        score: eval,
                        nodes: self.stats.total_nodes,
                        pv: pv.clone(),
                    });
                }
            }
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
    /// * `ply` - Current search depth in half-moves
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
    fn search(&mut self, depth: Depth, ply: u16, mut alpha: Eval, beta: Eval, pv: &mut Vec<Move>) -> Eval {
        // TODO : Is Vec really performant for pv structure?
        // TODO : Should we use a stack to store the PV and others things?

        self.stats.total_nodes += 1;
        self.stats.nodes += 1;

        // Here we check if we need to stop the search because of time constraints.
        if self.nodes_at_next_check <= self.stats.total_nodes {
            self.stopping = !self.time_manager.can_continue() || self.cancelation_token.load(Ordering::Relaxed);
            if self.stopping {
                return Eval::default();
            }
            self.update_nodes_at_next_check();
        }

        // In internal search nodes we probe the transposition table. If we find an acceptable exact score or a lower
        // bound better than beta we might cut the search imediately. If we find a lower bound better than alpha but not
        // better than beta we can immediately raise alpha.
        let key = self.position.hash();
        let tt_ref = self.transposition_table.probe(key);
        self.stats.tt_probes += 1;
        let tt_move = if let Some(tt_entry) = tt_ref.get(key) {
            self.stats.tt_probes_hit += 1;
            if depth <= tt_entry.depth() {
                let tt_eval = tt_entry.get_eval(ply);
                match tt_entry.entry_type() {
                    EntryType::Exact => {
                        self.stats.tt_cuts += 1;
                        return tt_eval;
                    }
                    EntryType::LowerBound => {
                        if tt_eval >= beta {
                            self.stats.tt_cuts += 1;
                            return tt_eval;
                        }
                    }
                    EntryType::UpperBound => {
                        if tt_eval <= alpha {
                            self.stats.tt_cuts += 1;
                            return tt_eval;
                        }
                    }
                }
            }
            tt_entry.mv()
        } else {
            None
        };

        // Null move pruning
        const NULL_MOVE_R: Depth = Depth::from_plies(3);
        let is_check = self.position.is_check();
        let current_eval = evaluate(&self.position);
        if depth > NULL_MOVE_R + Depth::ONE_PLY
            && !is_check
            && current_eval > beta
            && self.position.last_move().is_some()
            && self.position.has_pieces(self.position.side_to_move())
        {
            self.position.make(None);
            let mut local_pv = Vec::new();
            let null_eval = self.search(depth - NULL_MOVE_R, ply, beta - 1, beta, &mut local_pv);
            self.position.unmake();
            if null_eval >= beta {
                return beta;
            }
        }

        // Check extension
        let mut depth = depth;
        if is_check {
            depth += self.options.check_extension;
        }

        // Moves loop
        let move_generator = MoveGenerator::new(&self.position, tt_move, false);
        let mut has_legal_move = false;
        let mut best_eval = Eval::MIN;
        let mut best_move: Option<Move> = None;
        for mv in move_generator {
            if self.position.is_legal(mv) {
                has_legal_move = true;
                self.position.make(Some(mv));

                let mut local_pv = Vec::new();
                let eval = if self.position.is_draw() {
                    Eval::DRAW
                } else if depth <= Depth::ONE_PLY {
                    -self.qsearch(-beta, -alpha, ply + 1)
                } else {
                    -self.search(depth - Depth::ONE_PLY, ply + 1, -beta, -alpha, &mut local_pv)
                };

                self.position.unmake();

                // If we are aborting the search we return immediately
                if self.stopping {
                    return Eval::default();
                }

                if eval > best_eval {
                    best_eval = eval;

                    if best_eval > alpha {
                        best_move = Some(mv);

                        if best_eval >= beta {
                            break; // Beta cut-off
                        }

                        alpha = best_eval;
                        *pv = local_pv;
                        pv.push(mv);
                    }
                }
            }
        }

        // Check for checkmate or stalemate
        if !has_legal_move {
            if is_check {
                return -Eval::new_mat(ply);
            }

            return Eval::DRAW;
        }

        // Write the information in the transposition table
        let entry_type = if best_eval >= beta {
            EntryType::LowerBound
        } else if best_move.is_some() {
            EntryType::Exact
        } else {
            EntryType::UpperBound
        };

        // TODO : When we have a multithreaded search, check if the read_generation cost is impacting the performance.
        tt_ref.store(key, best_move, self.transposition_table.read_generation(), entry_type, depth, ply, best_eval);

        best_eval
    }

    fn update_nodes_at_next_check(&mut self) {
        let elapsed = self.start_time.expect("The timer should be started").elapsed();
        let nps = self.stats.total_nodes as f64 / elapsed.as_secs_f64();
        let time_before_check = self.time_manager.time_before_check();
        self.nodes_at_next_check = self.stats.total_nodes + (nps * time_before_check.as_secs_f64()) as u64;
    }

    /// Performs quiescence search to evaluate positions with tactical sequences.
    ///
    /// Quiescence search is a selective search that only explores capturing moves to reach a "quiet" position where
    /// tactical sequences are resolved. This helps avoid the horizon effect in chess engines.
    ///
    /// # Parameters
    /// * `alpha` - Lower bound of the search window
    /// * `beta` - Upper bound of the search window
    /// * `ply` - Current search depth in half-moves
    ///
    /// # Returns
    /// An evaluation score within the bounds of alpha and beta. A higher positive value indicates a better position for
    /// the side to move.
    fn qsearch(&mut self, alpha: Eval, beta: Eval, ply: u16) -> Eval {
        let mut alpha = alpha; // Make alpha mutable locally
        self.stats.total_nodes += 1;

        // In the qsearch we evaluate the stand pat (stop capturing) option. If the stand pat is better than beta, we
        // stop the search and return beta (beta cut-off). If the stand pat is better than alpha, we update alpha with the
        // stand pat value.
        let is_check = self.position.is_check();
        if !is_check {
            let stand_pat = evaluate(&self.position);
            if stand_pat >= beta {
                return beta;
            }
            if stand_pat > alpha {
                alpha = stand_pat;
            }
        }

        let mut has_legal_move = false;
        let move_generator = MoveGenerator::new(&self.position, None, true);
        for mv in move_generator {
            if self.position.is_legal(mv) {
                has_legal_move = true;

                self.position.make(Some(mv));
                let eval = -self.qsearch(-beta, -alpha, ply + 1);
                self.position.unmake();

                if eval >= beta {
                    return beta;
                }

                if eval > alpha {
                    alpha = eval;
                }
            }
        }

        // Check for checkmate or stalemate
        if is_check && !has_legal_move {
            return -Eval::new_mat(ply);
        }

        alpha
    }
}
