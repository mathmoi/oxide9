use crate::{
    eval::{evaluate, Eval},
    move_gen::{generation::generate_all_moves, move_list::MoveList},
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
        self.search(self.depth, &mut pv);
        pv
    }

    /// Recursively searches the position to the given depth.
    ///
    /// This method implements a basic alpha-beta search algorithm that explores
    /// the game tree to find the best move sequence from the current position.
    ///
    /// # Parameters
    /// * `depth` - Current remaining search depth in half-moves
    /// * `pv` - Mutable vector to store the principal variation (best line of moves)
    ///
    /// # Returns
    /// The evaluation score of the position after searching to the specified depth.
    /// Higher positive values indicate an advantage for the side to move.
    ///
    /// # Note
    /// This is an internal recursive method used by the `start` method.
    fn search(&mut self, depth: u16, pv: &mut Vec<Move>) -> Eval {
        // TODO : Is Vec really performant for pv structure?
        // TODO : Should we use a stack to store the PV and others things?
        if depth == 0 {
            return evaluate(&self.position);
        }

        let mut moves = MoveList::new();
        generate_all_moves(&self.position, &mut moves);

        let mut best_score = Eval::MIN;
        for mv in moves.iter() {
            if self.position.is_legal(*mv) {
                self.position.make(*mv);
                let mut local_pv = Vec::new();
                let score = -self.search(depth - 1, &mut local_pv);
                self.position.unmake();

                if score > best_score {
                    best_score = score;
                    *pv = local_pv;
                    pv.push(*mv);
                }
            }
        }

        best_score
    }
}
