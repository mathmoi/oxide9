use std::{
    cell::UnsafeCell,
    cmp::min,
    mem,
    sync::atomic::{AtomicU8, Ordering},
};

use crate::{eval::Eval, r#move::Move, zobrist::Zobrist};

pub type Generation = u8;

/// Type of evaluation stored in the transposition table.
#[repr(u8)]
#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum EntryType {
    /// Indicate that the entry contains an exact evaluation
    Exact = 0,

    /// Indicate that the entry contains a lower bound for the evaluation.
    LowerBound = 1,

    /// Indicate that the entry contains an upper bound for the evaluation.
    UpperBound = 2,
}

#[repr(align(16))]
pub struct Entry {
    pub data: u64,
    pub key: Zobrist,
}

impl Entry {
    const MOVE_SIZE: usize = 28;
    const GENERATION_SIZE: usize = 8;
    const ENTRY_TYPE_SIZE: usize = 2;
    const DEPTH_SIZE: usize = 10;
    const EVAL_SIZE: usize = 16;

    const MOVE_OFFSET: usize = 0;
    const GENERATION_OFFSET: usize = Self::MOVE_OFFSET + Self::MOVE_SIZE;
    const ENTRY_TYPE_OFFSET: usize = Self::GENERATION_OFFSET + Self::GENERATION_SIZE;
    const DEPTH_OFFSET: usize = Self::ENTRY_TYPE_OFFSET + Self::ENTRY_TYPE_SIZE;
    const EVAL_OFFSET: usize = Self::DEPTH_OFFSET + Self::DEPTH_SIZE;

    /// Creates a new transposition table entry.
    ///
    /// This constructor creates an entry that contains a position's Zobrist hash key along with search information like
    /// best move, evaluation, and search depth.
    ///
    /// # Parameters
    /// * `key` - Zobrist hash key for the position
    /// * `mv` - Best move found for this position
    /// * `generation` - Current search generation to track entry age/freshness
    /// * `entry_type` - Type of search result (exact, alpha, beta)
    /// * `depth` - Search depth at which this position was evaluated
    /// * `ply` - Current search ply (used for mate score adjustments)
    /// * `eval` - Evaluation score for this position
    ///
    /// The information is efficiently packed into a compact binary representation to minimize memory usage in the
    /// transposition table.
    pub fn new(
        key: Zobrist,
        mv: Option<Move>,
        generation: Generation,
        entry_type: EntryType,
        depth: u16,
        ply: u16,
        eval: Eval,
    ) -> Self {
        debug_assert!(Move::pack(mv) < (1u32 << Self::MOVE_SIZE));
        debug_assert!((generation as u64) < (1u64 << Self::GENERATION_SIZE));
        debug_assert!((entry_type as u8) < (1 << Self::ENTRY_TYPE_SIZE));
        debug_assert!(depth < (1 << Self::DEPTH_SIZE));

        let data = (Move::pack(mv) as u64) << Self::MOVE_OFFSET
            | (generation as u64) << Self::GENERATION_OFFSET
            | (entry_type as u8 as u64) << Self::ENTRY_TYPE_OFFSET
            | (depth as u64) << Self::DEPTH_OFFSET
            | (i16::from(eval.remove_ply_from_mat(ply)) as u16 as u64) << Self::EVAL_OFFSET;

        Entry { key, data }
    }

    /// Retrieves the Zobrist hash key for this entry.
    pub fn key(&self) -> Zobrist {
        self.key
    }

    /// Returns the move stored in the entry.
    pub fn mv(&self) -> Option<Move> {
        Move::unpack(((self.data >> Self::MOVE_OFFSET) & ((1u64 << Self::MOVE_SIZE) - 1)) as u32)
    }

    /// Returns the generation of the entry.
    pub fn generation(&self) -> Generation {
        (self.data >> Self::GENERATION_OFFSET & ((1u64 << Self::GENERATION_SIZE) - 1)) as Generation
    }

    /// Returns the type of entry (exact, lower bound, upper bound).
    pub fn entry_type(&self) -> EntryType {
        unsafe {
            std::mem::transmute((self.data >> Self::ENTRY_TYPE_OFFSET & ((1u64 << Self::ENTRY_TYPE_SIZE) - 1)) as u8)
        }
    }

    /// Returns the search depth of the entry.
    pub fn depth(&self) -> u16 {
        (self.data >> Self::DEPTH_OFFSET & ((1u64 << Self::DEPTH_SIZE) - 1)) as u16
    }

    /// Returns the evaluation score of the entry, adjusted for the current ply.
    pub fn get_eval(&self, ply: u16) -> Eval {
        Eval::from((self.data >> Self::EVAL_OFFSET & ((1u64 << Self::EVAL_SIZE) - 1)) as u16 as i16).add_ply_to_mat(ply)
    }
}

/// A transposition table reference points to en entry in the transposition table, it can be use to read or write an
/// entry.
pub struct TTRef {
    ptr: *mut Entry,
}

/// A reference into the transposition table. This can be used to read an entry and possibly overwrite it later without
/// having to look it up again.
///
/// Maintains a direct pointer to a specific entry's location in the transposition table, avoiding additional hash
/// lookups when updating the same position.
///
/// In a future version this may internally be a reference to a bucket in the transposition table instead.
impl TTRef {
    /// Creates a new transposition table reference pointing to the given entry.
    fn new(ptr: *mut Entry) -> Self {
        TTRef { ptr }
    }

    /// Retrieves the entry this reference points to.
    pub fn get(&self, key: Zobrist) -> Option<&Entry> {
        let entry = unsafe { &*self.ptr };
        if entry.key() != key {
            return None;
        }
        Some(entry)
    }

    /// Overwrites the referenced entry with a new one.
    ///
    /// # Panics
    /// Panics in debug builds if attempting to store an entry with a zero key.
    #[allow(clippy::too_many_arguments)]
    pub fn store(
        &self,
        key: Zobrist,
        mv: Option<Move>,
        generation: Generation,
        entry_type: EntryType,
        depth: u16,
        ply: u16,
        eval: Eval,
    ) {
        debug_assert!(key != 0, "Cannot store an entry with a zero key");

        let original = unsafe { &*self.ptr };
        if original.generation() != generation || original.depth() <= depth {
            unsafe { *self.ptr = Entry::new(key, mv, generation, entry_type, depth, ply, eval) };
        }
    }
}

pub struct TranspositionTable {
    pub table: Box<[UnsafeCell<Entry>]>,
    pub mask: usize,
    pub generation: AtomicU8,
}

// Safety: This implementation is safe because:
// 1. We're using UnsafeCell for interior mutability, but we ensure that each thread
//    accesses different array elements through the hash function
// 2. Concurrent writes to the same entry might occur but that's acceptable for a
//    transposition table where occasional data races only impact performance, not correctness
unsafe impl Sync for TranspositionTable {}

impl TranspositionTable {
    /// Default size of the transposition table in megabytes.
    pub const DEFAULT_MB_SIZE: usize = 128;

    /// Creates a new transposition table with the specified size.
    ///
    /// Note that the size must be at least 1024 and a power of 2. If the size is not a power of 2, it will be rounded down to the next power of 2.
    pub fn new(size: usize) -> Self {
        debug_assert!(size >= 1024, "Transposition table size must be at least 1024");

        let capacity = 1 << (63 - (size / mem::size_of::<Entry>()).leading_zeros());
        let mut vec = Vec::with_capacity(capacity);
        vec.resize_with(capacity, || UnsafeCell::new(Entry { data: 0, key: 0 }));
        TranspositionTable { table: vec.into_boxed_slice(), mask: capacity - 1, generation: AtomicU8::new(0) }
    }

    /// Retrieves a reference to an entry in the transposition table using a Zobrist hash key.
    ///
    /// This function locates the appropriate slot in the transposition table based on the provided Zobrist key. It
    /// returns a TTRef object that provides direct access to the entry, allowing for efficient read and update
    /// operations without requiring additional hash lookups.
    ///
    /// The returned reference may point to an existing entry with a different key (hash collision), an empty entry, or
    /// a matching entry. Callers should check the entry's key to determine if it matches the requested position.
    ///
    /// # Parameters
    /// * `key` - The Zobrist hash key of the chess position
    ///
    /// # Returns
    /// A TTRef object that can be used to read or update the entry at the corresponding location
    pub fn probe(&self, key: Zobrist) -> TTRef {
        let index = (key as usize) & self.mask;
        TTRef::new(self.table[index].get())
    }

    /// Increments the generation counter for the transposition table.
    ///
    /// The generation counter is used to track the age of entries in the transposition table. This helps determine
    /// which entries should be replaced when space is needed. Typically called at the start of a new search.
    ///
    /// This operation is thread-safe and uses atomic operations to update the generation value.
    pub fn increment_generation(&self) {
        self.generation.fetch_add(1, Ordering::AcqRel);
    }

    /// Returns the current generation of the transposition table.
    ///
    /// The generation value helps identify fresh vs. stale entries in the table.
    ///
    /// # Performance Note
    /// There is a synchronization cost to reading this value due to atomic operations, so it should be read
    /// infrequently and cached by each thread.
    ///
    /// # Returns
    /// The current generation counter value
    pub fn read_generation(&self) -> Generation {
        self.generation.load(Ordering::Acquire)
    }

    /// Calculates an approximate load factor of the transposition table.
    ///
    /// The load factor is the ratio of occupied entries (with the current generation) to the total capacity. This
    /// method samples up to the first 1024 entries to estimate the overall utilization without scanning the entire
    /// table.
    ///
    /// # Returns
    /// A value between 0.0 and 1.0 representing the estimated percentage of table slots that contain current generation
    /// entries.
    pub fn load_factor(&self) -> f64 {
        let max_index = min(self.table.len(), 1024);
        let mut used_count: usize = 0;
        let generation = self.read_generation();
        for index in 0..max_index {
            let entry = unsafe { &*self.table[index].get() };
            if entry.key() != 0 && entry.generation() == generation {
                used_count += 1;
            }
        }
        used_count as f64 / max_index as f64
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    mod entry_tests {
        use super::*;
        use crate::{coordinates::Square, piece::Piece};

        #[test]
        fn test_entry_size() {
            assert_eq!(std::mem::size_of::<Entry>(), 16);
        }

        #[test]
        fn test_data_total_size() {
            let total_size = Entry::MOVE_SIZE
                + Entry::GENERATION_SIZE
                + Entry::ENTRY_TYPE_SIZE
                + Entry::DEPTH_SIZE
                + Entry::EVAL_SIZE;
            assert_eq!(total_size, 64);
        }

        #[test]
        fn test_new_and_getters() {
            let key: Zobrist = 0xDEADBEEF;
            let mv = Move::new_capture_promotion(
                Square::H7,
                Square::G8,
                Piece::WHITE_PAWN,
                Piece::BLACK_KNIGHT,
                Piece::WHITE_QUEEN,
            );
            let generation = 42;
            let entry_type = EntryType::Exact;
            let depth = 8;
            let ply = 4;
            let eval = Eval::new_mat(12);

            let entry = Entry::new(key, Some(mv), generation, entry_type, depth, ply, eval);

            assert_eq!(entry.key(), key);
            assert_eq!(entry.mv(), Some(mv));
            assert_eq!(entry.generation(), generation);
            assert_eq!(entry.entry_type(), entry_type);
            assert_eq!(entry.depth(), depth);
            assert_eq!(entry.get_eval(ply), eval);
        }
    }

    mod ttref_tests {
        use super::*;
        use crate::{coordinates::Square, piece::Piece, r#move::Move};

        #[test]
        fn test_get() {
            let key: Zobrist = 0xDEADBEEF;
            let entry = Entry { data: 0, key };
            let ttref = TTRef::new(&entry as *const _ as *mut _);

            assert_eq!(ttref.get(key).unwrap().key(), key);
        }

        #[test]
        fn test_store() {
            let key: Zobrist = 0xDEADBEEF;
            let mv = Move::new_capture_promotion(
                Square::H7,
                Square::G8,
                Piece::WHITE_PAWN,
                Piece::BLACK_KNIGHT,
                Piece::WHITE_QUEEN,
            );
            let generation = 42;
            let entry_type = EntryType::Exact;
            let depth = 8;
            let ply = 4;
            let eval = Eval::new_mat(12);

            let entry = Entry { data: 0, key: 0 };
            let ttref = TTRef::new(&entry as *const _ as *mut _);

            ttref.store(key, Some(mv), generation, entry_type, depth, ply, eval);

            assert_eq!(entry.key(), key);
            assert_eq!(entry.mv(), Some(mv));
            assert_eq!(entry.generation(), generation);
            assert_eq!(entry.entry_type(), entry_type);
            assert_eq!(entry.depth(), depth);
            assert_eq!(entry.get_eval(ply), eval);
        }
    }
}
