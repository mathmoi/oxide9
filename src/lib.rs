pub mod bitboard;
pub mod coordinates;
pub mod r#move;
pub mod move_gen;
pub mod perft;
pub mod piece;
pub mod position;

/// Initialize the library, this function must be called before using any other functions.
pub fn initialize() {
    bitboard::initialize();
    move_gen::attacks::initialize();
}
