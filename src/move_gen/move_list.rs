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
pub struct MoveList {
    pub moves: [Move; MAX_MOVES],
    pub count: usize,
}

impl MoveList {
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
    pub fn new() -> Self {
        Self {
            moves: unsafe {
                let block = MaybeUninit::uninit();
                block.assume_init()
            },
            count: 0,
        }
    }

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

    /// Removes and returns the last move from the list.
    ///
    /// # Returns
    /// The last move in the list.
    ///
    /// # Panics
    /// Debug builds will panic if the list is empty. In release builds, calling this method on an empty list results in
    /// undefined behavior.
    ///
    /// # Note
    /// Reduces the count of moves in the list by 1.
    pub fn pop(&mut self) -> Move {
        debug_assert!(self.count > 0);

        self.count -= 1;
        self.moves[self.count]
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
    pub fn iter(&self) -> impl Iterator<Item = &Move> {
        self.moves.iter().take(self.count)
    }

    /// Returns a mutable iterator over the valid moves in the list.
    ///
    /// # Returns
    /// A mutable iterator that yields references to each valid move in the list (from index 0 to count-1).
    ///
    /// # Note
    /// Only iterates through the populated portion of the list up to the current count of moves, allowing for in-place
    /// modification of individual moves.
    pub fn iter_mut(&mut self) -> impl Iterator<Item = &mut Move> {
        self.moves.iter_mut().take(self.count)
    }

    /// Sorts the moves in the list in ascending order based on their evaluation values.
    ///
    /// This method uses an unstable sort to arrange moves by their evaluation scores. After sorting, moves with lower
    /// evaluation scores will appear earlier in the list.
    pub fn sort(&mut self) {
        self.moves[0..self.count].sort_unstable_by_key(|mv| mv.eval());
    }
}
