use std::sync::Once;

pub mod analyze;
pub mod bitboard;
pub mod config;
pub mod coordinates;
pub mod eval;
pub mod r#move;
pub mod move_gen;
pub mod perft;
pub mod piece;
pub mod position;
pub mod search;

static INIT: Once = Once::new();

/// Initialize the library, this function must be called before using any other functions.
pub fn initialize() {
    INIT.call_once(|| {
        config::initialize().unwrap();
        bitboard::initialize();
        move_gen::attacks::initialize();
        eval::initialize();
    });
}
