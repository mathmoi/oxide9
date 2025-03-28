use crate::{
    eval::{evaluate, Eval},
    move_gen::{
        generation::{generate_all_moves, generate_qsearch},
        move_list::MoveList,
    },
    position::Position,
    r#move::Move,
};

pub struct Search<'a> {
    position: &'a mut Position,
    depth: u16,
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
    pub fn new(position: &'a mut Position, depth: u16) -> Search<'a> {
        Search { position, depth }
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
        let mut pv = Vec::new();
        self.search::<false>(self.depth, Eval::MIN, Eval::MAX, &mut pv);
        pv
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
    fn search<const QSEARCH: bool>(&mut self, depth: u16, alpha: Eval, beta: Eval, pv: &mut Vec<Move>) -> Eval {
        let mut alpha = alpha; // Make alpha mutable locally

        // TODO : Is Vec really performant for pv structure?
        // TODO : Should we use a stack to store the PV and others things?

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

        let mut moves = MoveList::new();
        if QSEARCH {
            generate_qsearch(&self.position, &mut moves);
        } else {
            generate_all_moves(&self.position, &mut moves);
        }

        for mv in moves.iter() {
            if self.position.is_legal(*mv) {
                self.position.make(*mv);
                let mut local_pv = Vec::new();
                let score = if QSEARCH || depth == 1 {
                    -self.search::<true>(0, -beta, -alpha, &mut local_pv)
                } else {
                    -self.search::<false>(depth - 1, -beta, -alpha, &mut local_pv)
                };
                self.position.unmake();

                if score >= beta {
                    return beta;
                }

                if score > alpha {
                    alpha = score;
                    if !QSEARCH {
                        *pv = local_pv;
                        pv.push(*mv);
                    }
                }
            }
        }

        alpha
    }
}
