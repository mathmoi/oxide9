use std::mem::MaybeUninit;

use crate::r#move::Move;

const MAX_MOVES: usize = 256;

/// Structure to store a list of chess moves efficiently.
///
/// This structure uses a fixed-size array to avoid heap allocations during move generation, which is a
/// performance-critical operation in chess engines.
///
/// # Safety
/// For performance reasons, this implementation does not perform bounds checking when adding moves. It is the caller's
/// responsibility to ensure that the list is not full before adding a move. Adding a move to a full list will cause
/// undefined behavior (likely a panic or memory corruption).
///
/// # Memory Layout
/// Uses a fixed array of size MAX_MOVES and a counter to track the number of valid moves.
#[derive(Debug, Clone)]
pub struct MoveList {
    pub moves: [Move; MAX_MOVES],
    pub count: usize,
}

impl Default for MoveList {
    /// Creates a new empty move list.
    ///
    /// Initializes a MoveList with count set to 0 and an uninitialized array of moves. This approach avoids the cost of
    /// initializing the entire fixed-size array, which is a significant performance optimization for move generation.
    ///
    /// # Returns
    /// A new MoveList with no moves.
    ///
    /// # Safety
    /// Uses unsafe code to avoid initialization overhead. The `count` field ensures that only initialized elements are
    /// accessed during normal operation. The moves array contains uninitialized memory beyond the `count` value.
    fn default() -> Self {
        Self {
            moves: unsafe {
                let block = MaybeUninit::uninit();
                block.assume_init()
            },
            count: 0,
        }
    }
}

impl MoveList {
    /// Adds a move to the list.
    ///
    /// Places the move at the current count position and increments the counter.
    ///
    /// # Parameters
    /// * `mv` - The chess move to add to the list
    ///
    /// # Safety
    /// Does not check if the list is full. Callers must ensure there is space available before calling this method
    /// (count < MAX_MOVES) to avoid out-of-bounds array access.
    ///
    /// # Performance
    /// This method is intentionally minimal with no bounds checking to maximize performance during move generation.
    pub fn push(&mut self, mv: Move) {
        debug_assert!(self.count < MAX_MOVES);

        self.moves[self.count] = mv;
        self.count += 1;
    }

    /// Removes and returns the highest-scoring move from the list.
    ///
    /// Finds the move with the highest evaluation score, swaps it with the last move in the list, and returns it after
    /// reducing the count of moves.
    ///
    /// # Returns
    /// The move with the highest evaluation score in the list.
    ///
    /// # Panics
    /// Debug builds will panic if the list is empty. In release builds, calling this method on an empty list results in
    /// undefined behavior.
    ///
    /// # Note
    /// Reduces the count of moves in the list by 1.
    pub fn pop(&mut self) -> Move {
        debug_assert!(self.count > 0);

        let mut best = self.moves[0].eval();
        let mut best_index = 0;
        for index in 1..self.count {
            if best < self.moves[index].eval() {
                best = self.moves[index].eval();
                best_index = index;
            }
        }

        let result = self.moves[best_index];
        self.moves[best_index] = self.moves[self.count - 1];
        self.count -= 1;
        result
    }

    /// Removes the move at the specified index from the list by swapping it with the last move.
    ///
    /// This operation replaces the move at the specified index with the last move in the list and then decreases the
    /// count. This is more efficient than shifting elements but does not preserve the order of moves.
    ///
    /// # Parameters
    /// * `index` - The index of the move to remove
    ///
    /// # Panics
    /// Panics in debug builds if the index is out of bounds.
    pub fn swap_remove(&mut self, index: usize) {
        debug_assert!(index < self.count);
        self.moves[index] = self.moves[self.count - 1];
        self.count -= 1;
    }

    /// Checks if the list is empty.
    ///
    /// # Returns
    /// `true` if the collection list no elements, `false` otherwise.
    pub fn is_empty(&self) -> bool {
        self.count == 0
    }

    /// Returns the number of moves currently in the list.
    ///
    /// Provides the count of valid moves that have been added to the list. This value is also the index where the next
    /// move would be inserted.
    ///
    /// # Returns
    /// The count of valid moves in the list.
    pub fn len(&self) -> usize {
        self.count
    }

    /// Returns a mutable reference to a move at a specific index.
    pub fn get_mut(&mut self, index: usize) -> &mut Move {
        debug_assert!(index < self.count);
        &mut self.moves[index]
    }

    /// Returns an iterator over the valid moves in the list.
    ///
    /// Creates an iterator that yields references to only the valid moves in the list (from index 0 to count-1),
    /// ignoring uninitialized memory beyond count.
    ///
    /// # Returns
    /// An iterator that yields references to each move in the list.
    ///
    /// # Performance
    /// Uses the standard iterator implementation with a take() operation to limit iteration to only the valid portion
    /// of the internal array.
    pub fn iter(&self) -> impl Iterator<Item = Move> + '_ {
        self.moves.iter().take(self.count).copied()
    }

    /// Returns a mutable iterator over the valid moves in the list.
    ///
    /// # Returns
    /// A mutable iterator that yields references to each valid move in the list (from index 0 to count-1).
    ///
    /// # Note
    /// Only iterates through the populated portion of the list up to the current count of moves, allowing for in-place
    /// modification of individual moves.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Move> + '_ {
        self.moves.iter_mut().take(self.count)
    }

    /// Moves a specified move to the front of the moves list.
    ///
    /// If the specified move exists in the list, it will be moved to the front, shifting all other moves one position
    /// to accommodate this change. If the move doesn't exist in the list, no action is taken.
    ///
    /// # Parameters
    /// * `mv` - The move to be brought to the front of the list
    pub fn move_front(&mut self, mv: Move) {
        let maybe_position = self.moves[1..self.count].iter().position(|x| *x == mv);
        if let Some(position) = maybe_position {
            self.moves.copy_within(0..position, 1);
            self.moves[0] = mv;
        }
    }
}

impl FromIterator<Move> for MoveList {
    /// Creates a MoveList from an iterator of moves.
    ///
    /// This method collects moves from the provided iterator into a new MoveList. It will panic if the number of moves
    /// exceeds MAX_MOVES.
    ///
    /// # Parameters
    /// * `iter` - An iterator over Move objects.
    ///
    /// # Returns
    /// A new MoveList containing all moves from the iterator.
    ///
    /// # Panics
    /// If the number of moves exceeds MAX_MOVES, this method will panic.
    fn from_iter<I: IntoIterator<Item = Move>>(iter: I) -> Self {
        let mut list = Self::default();
        for mv in iter {
            list.push(mv);
        }
        list
    }
}
