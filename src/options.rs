use std::sync::{LazyLock, RwLock};

/// Probides read-only access to engine options.
pub trait ReadOnlyOptions {
    /// Returns the number of moves to go estimate.
    fn moves_to_go_estimate(&self) -> u32;

    /// Returns the maximum time ratio per move.
    fn max_time_ratio_per_move(&self) -> f32;

    /// Returns the maximum over target factor.
    fn max_over_target_factor(&self) -> f32;

    /// Returns the size of check extension in sixteenths of a ply.
    fn check_extension_sixteenths(&self) -> i16;
}

/// Configuration options for the chess engine
#[derive(Debug, Clone)]
pub struct Options {
    /// The number of moves the engine estimates remain in the game when no explicit moves-to-go information is
    /// provided. Used for time allocation.
    moves_to_go_estimate: u32,

    /// The maximum ratio of allocated time that should be spent on a single move.
    max_time_ratio_per_move: f32,

    /// Factor determining how much over the target time the engine is allowed to go. Higher values give the engine more
    /// flexibility to finish critical calculations even if slightly exceeding the allocated time.
    max_over_target_factor: f32,

    /// The size of check extension in sixteenths of a ply.
    check_extension_sixteenths: i16,
}

static OPTIONS: LazyLock<RwLock<Options>> = LazyLock::new(|| RwLock::new(Options::default()));

impl Default for Options {
    /// Provides default values for engine options.
    fn default() -> Self {
        Self {
            moves_to_go_estimate: 45,
            max_time_ratio_per_move: 0.8,
            max_over_target_factor: 5.0,
            check_extension_sixteenths: 9,
        }
    }
}

impl ReadOnlyOptions for Options {
    /// Returns the number of moves the engine estimates remain in the game when no explicit moves-to-go information is
    /// provided.
    fn moves_to_go_estimate(&self) -> u32 {
        self.moves_to_go_estimate
    }

    /// Returns the maximum ratio of allocated time that should be spent on a single move.
    fn max_time_ratio_per_move(&self) -> f32 {
        self.max_time_ratio_per_move
    }

    /// Returns the factor determining how much over the target time the engine is allowed to go.
    fn max_over_target_factor(&self) -> f32 {
        self.max_over_target_factor
    }

    /// Returns the size of check extension in sixteenths of a ply.
    fn check_extension_sixteenths(&self) -> i16 {
        self.check_extension_sixteenths
    }
}

impl Options {
    /// Returns a copy of the current global engine options.
    ///
    /// Provides a thread-safe way to access the current engine configuration by returning a complete copy of the global
    /// options state. This ensures that the caller gets a consistent snapshot of all option values.
    ///
    /// # Returns
    /// * `Options` - A clone of the current engine options
    pub fn get() -> impl ReadOnlyOptions {
        OPTIONS.read().expect("Options should always be available").clone()
    }

    /// Modifies the current global engine options atomically using the provided closure.
    ///
    /// Provides a thread-safe way to update engine options by acquiring a write lock and allowing the caller to modify
    /// the options through a closure. This ensures that all modifications happen atomically within a single
    /// transaction.
    pub fn modify<F>(f: F)
    where
        F: FnOnce(&mut Options),
    {
        let mut options = OPTIONS.write().expect("Options should always be available");
        f(&mut options);
    }

    /// Sets the number of moves the engine estimates remain in the game when no explicit moves-to-go information is
    /// provided.
    pub fn set_moves_to_go_estimate(&mut self, value: u32) {
        self.moves_to_go_estimate = value;
    }

    /// Sets the maximum ratio of allocated time that should be spent on a single move.
    pub fn set_max_time_ratio_per_move(&mut self, value: f32) {
        self.max_time_ratio_per_move = value;
    }

    /// Sets the factor determining how much over the target time the engine is allowed to go.
    pub fn set_max_over_target_factor(&mut self, value: f32) {
        self.max_over_target_factor = value;
    }
}
