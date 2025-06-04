use std::sync::Once;

pub mod analyze;
pub mod bench;
pub mod bitboard;
pub mod coordinates;
pub mod depth;
pub mod eval;
pub mod r#move;
pub mod move_gen;
pub mod notation;
pub mod options;
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
    INIT.call_once(|| {
        bitboard::initialize();
        move_gen::attacks::initialize();
        eval::initialize();
        zobrist::initialize();
    });
}
