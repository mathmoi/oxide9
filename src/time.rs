use std::{
    cmp::min,
    time::{Duration, Instant},
};

/// Represents different chess time control formats.
///
/// Chess games can be played with various time control systems that determine how much time each player has for their
/// moves. This enum encapsulates the common time control formats used in chess.
pub enum TimeControl {
    /// Fixed time per move.
    /// The player must make each move within this duration.
    MoveTime(Duration),

    /// Traditional chess clock with a specified number of moves to reach.
    /// Player has a set amount of time to make a specified number of moves.
    Conventional { time: Duration, moves_to_go: u16 },

    /// Time control with increment after each move.
    /// Player has a base time and receives additional time after completing each move.
    Incremental { time: Duration, increment: Duration },

    /// Simple time control with no increment or move requirements.
    /// Player must complete the game within this total time allocation.
    SuddenDeath { time: Duration },

    /// No time limit.
    /// Used for analysis mode or when time control is managed externally.
    Infinite,
}

impl TimeControl {
    /// Creates a new TimeControl instance based on the provided time control parameters.
    ///
    /// This function chooses the appropriate time control variant based on the combination of parameters provided,
    /// following standard chess time control conventions.
    ///
    /// # Parameters
    /// * `time` - The main time allocation for the player
    /// * `increment` - Additional time gained after each move
    /// * `moves_to_go` - Number of moves to be completed within the time allocation
    /// * `move_time` - Fixed time per move
    /// * `infinite` - Flag for unlimited time
    ///
    /// # Returns
    /// A TimeControl enum variant that best represents the provided parameters.
    ///
    /// # Precedence
    /// Time controls are selected in the following order of precedence:
    /// 1. Infinite (if the infinite flag is true)
    /// 2. MoveTime (if move_time is specified)
    /// 3. Conventional (if time and moves_to_go are specified)
    /// 4. Incremental (if time and a non-zero increment are specified)
    /// 5. SuddenDeath (if only time is specified)
    ///
    /// # Panics
    /// Panics if no valid time control parameters are provided. Callers must ensure at least one valid time control
    /// configuration is specified.
    pub fn new(
        time: Option<Duration>,
        increment: Option<Duration>,
        moves_to_go: Option<u16>,
        move_time: Option<Duration>,
        infinite: bool,
    ) -> Self {
        if infinite {
            return TimeControl::Infinite;
        }

        if let Some(move_time) = move_time {
            return TimeControl::MoveTime(move_time);
        }

        if let Some(time) = time {
            if let Some(moves_to_go) = moves_to_go {
                return TimeControl::Conventional { time, moves_to_go };
            }

            if let Some(increment) = increment {
                if increment > Duration::ZERO {
                    return TimeControl::Incremental { time, increment };
                }
            }

            return TimeControl::SuddenDeath { time };
        }

        unreachable!()
    }
}

pub struct TimeManager {
    min: Duration,
    max: Duration,
    target: Duration,
    search_start: Instant,
    iterations_completed: u16,
    iteration_start: Instant,
    last_iteration_duration: [Duration; 2],
}

impl TimeManager {
    const SAFETY_MARGIN: Duration = Duration::from_millis(30);
    const MOVES_TO_GO_ESTIMATE: u32 = 35;
    const MAX_TIME_RATIO_PER_MOVE: f32 = 0.8;
    const MIN_ITERATIONS: u16 = 1;
    const MIN_DURATION_BETWEEN_CHECKS: Duration = Duration::from_millis(10);
    const MAX_DURATION_BETWEEN_CHECKS: Duration = Duration::from_millis(1000);

    /// Creates a new TimeManager instance with the specified time control.
    ///
    /// # Parameters
    /// * `time_control` - The time control format to be used for the game.
    ///
    /// # Returns
    /// A new TimeManager instance configured with the provided time control.
    pub fn new(time_control: TimeControl) -> Self {
        let (min, max, target) = match time_control {
            TimeControl::MoveTime(time) => {
                (time - Self::SAFETY_MARGIN, time - Self::SAFETY_MARGIN, time - Self::SAFETY_MARGIN)
            }
            TimeControl::Conventional { time, moves_to_go } => {
                let max = time.mul_f32(Self::MAX_TIME_RATIO_PER_MOVE);
                let target = min(time / moves_to_go as u32, max);
                (Duration::ZERO, max, target)
            }
            TimeControl::Incremental { time, increment } => {
                let max = time.mul_f32(Self::MAX_TIME_RATIO_PER_MOVE);
                let target = min(time / Self::MOVES_TO_GO_ESTIMATE + increment, max);
                (Duration::ZERO, max, target)
            }
            TimeControl::SuddenDeath { time } => {
                let max = time.mul_f32(Self::MAX_TIME_RATIO_PER_MOVE);
                let target = min(time / Self::MOVES_TO_GO_ESTIMATE, max);
                (Duration::ZERO, max, target)
            }
            TimeControl::Infinite => (Duration::MAX, Duration::MAX, Duration::MAX),
        };

        TimeManager {
            min,
            max,
            target,
            search_start: Instant::now(),
            iterations_completed: 0,
            iteration_start: Instant::now(),
            last_iteration_duration: [Duration::ZERO; 2],
        }
    }

    /// Records the start time of a new iteration.
    ///
    /// This method should be called at the beginning of each search iteration. It updates the internal state to track
    /// when the current iteration began.
    pub fn iteration_started(&mut self) {
        self.iteration_start = Instant::now();
    }

    /// Records the completion of the current search iteration.
    ///
    /// Updates the history of iteration durations by shifting the previous duration and storing the current iteration's
    /// duration. Also increments the count of completed iterations.
    ///
    /// This method should be called at the end of each search iteration.
    pub fn iteration_finished(&mut self) {
        self.last_iteration_duration[0] = self.last_iteration_duration[1];
        self.last_iteration_duration[1] = self.iteration_start.elapsed();
        self.iterations_completed += 1;
    }

    /// Determines whether the search can continue running.
    ///
    /// # Returns
    /// `true` if either:
    /// - The minimum search criteria have not been met yet (minimum iterations or minimum time), or
    /// - The elapsed time since search started is still below the maximum allowed time.
    ///
    /// `false` if the search has exhausted its allocated time.
    pub fn can_continue(&self) -> bool {
        self.need_to_continue() || self.search_start.elapsed() < self.max
    }

    /// Determines whether the search must continue based on minimum criteria.
    ///
    /// # Returns
    /// `true` if either:
    /// - The minimum number of iterations has not been completed yet, or
    /// - The minimum search time has not been reached.
    ///
    /// This is an internal helper method used to ensure search quality meets baseline requirements regardless of time
    /// constraints.
    fn need_to_continue(&self) -> bool {
        self.iterations_completed < Self::MIN_ITERATIONS || self.search_start.elapsed() < self.min
    }

    /// Determines whether a new search iteration should be started.
    ///
    /// # Returns
    /// `true` if:
    /// - The minimum search criteria have not been met yet, or
    /// - The search is still within time limits and:
    ///   - The target time has not been reached, and
    ///   - The estimated time to complete at least half of the next iteration fits within the remaining time before the
    ///     target.
    ///
    /// `false` if starting a new iteration would likely exceed time constraints or the search has already met its
    /// target time.
    pub fn can_start_iteration(&self) -> bool {
        if self.need_to_continue() {
            return true;
        }

        if !self.can_continue() {
            return false;
        }

        let elapsed = self.search_start.elapsed();
        if self.target <= elapsed {
            return false;
        }

        // Return true if we expect to finish at least half of the next iteration before the target time
        let time_before_target = self.target - elapsed;
        let estimate_next_iteration = Duration::from_secs_f64(
            (self.last_iteration_duration[1].as_secs_f64() / self.last_iteration_duration[0].as_secs_f64())
                * self.last_iteration_duration[1].as_secs_f64(),
        );
        estimate_next_iteration / 2 < time_before_target
    }

    /// Returns the time remaining before the engine should check for time constraints (by calling can_continue).
    pub fn time_before_check(&self) -> Duration {
        let elapsed = self.search_start.elapsed();
        if self.max <= elapsed {
            return Self::MIN_DURATION_BETWEEN_CHECKS;
        }

        ((self.max - self.search_start.elapsed()) / 2)
            .clamp(Self::MIN_DURATION_BETWEEN_CHECKS, Self::MAX_DURATION_BETWEEN_CHECKS)
    }
}
