use std::sync::Once;

pub mod analyze;
pub mod bitboard;
pub mod config;
pub mod coordinates;
pub mod eval;
pub mod r#move;
pub mod move_gen;
pub mod notation;
pub mod perft;
pub mod piece;
pub mod position;
pub mod search;
pub mod time;
pub mod tt;
pub mod uci;
pub mod zobrist;

static INIT: Once = Once::new();

/// Initialize the library, this function must be called before using any other functions.
pub fn initialize() {
    initialize_with_args(None, false);
}

pub fn initialize_with_args(perft_threads: Option<u32>, precise: bool) {
    INIT.call_once(|| {
        config::initialize(perft_threads, precise).unwrap();
        bitboard::initialize();
        move_gen::attacks::initialize();
        eval::initialize();
        zobrist::initialize();
    });
}
