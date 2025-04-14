use rand::random;

use crate::{
    coordinates::{File, Square},
    piece::Piece,
    r#move::CastlingRight,
};

/// Type alias for the Zobrist hash value
pub type Zobrist = u64;

static mut ZOBRIST_PIECE_SQUARE: [Zobrist; Piece::COUNT * Square::COUNT] = [0; Piece::COUNT * Square::COUNT];
static mut ZOBRIST_EN_PASSANT: [Zobrist; File::COUNT] = [0; File::COUNT];
static mut ZOBRIST_BLACK_TO_MOVE: Zobrist = 0;
static mut ZOBRIST_CASTLING: [Zobrist; CastlingRight::COUNT] = [0; 16];

/// Initializes all Zobrist hash keys used throughout the engine. Must be called once before using any Zobrist hashing
/// functions.
pub fn initialize() {
    initialize_zobrist_piece_square();
    initialize_zobrist_en_passant();
    initialize_zobrist_black_to_move();
    initialize_zobrist_castling();
}

fn initialize_zobrist_piece_square() {
    for piece in Piece::ALL {
        for sq in Square::ALL {
            unsafe {
                ZOBRIST_PIECE_SQUARE[usize::from(piece) * Square::COUNT + usize::from(sq)] = random();
            }
        }
    }
}

fn initialize_zobrist_en_passant() {
    for file in File::ALL {
        unsafe {
            ZOBRIST_EN_PASSANT[usize::from(file)] = random();
        }
    }
}

fn initialize_zobrist_black_to_move() {
    unsafe {
        ZOBRIST_BLACK_TO_MOVE = random();
    }
}

fn initialize_zobrist_castling() {
    // We let the index zero to the default value (0) so that the default zobrist value of an empty board is zero.
    for index in 1..CastlingRight::COUNT {
        unsafe {
            ZOBRIST_CASTLING[index] = random();
        }
    }
}

/// Returns the Zobrist hash value for a specific piece on a specific square. This hash is used when adding or removing
/// pieces from the board.
pub fn zobrist_piece_square(piece: Piece, square: Square) -> Zobrist {
    unsafe { ZOBRIST_PIECE_SQUARE[usize::from(piece) * Square::COUNT + usize::from(square)] }
}

/// Returns the Zobrist hash value for an en passant opportunity on the given file. This hash is used to mark the
/// possibility of an en passant capture.
pub fn zobrist_en_passant(square: Option<Square>) -> Zobrist {
    match square {
        Some(square) => unsafe { ZOBRIST_EN_PASSANT[usize::from(square.file())] },
        None => 0,
    }
}

/// Returns the Zobrist hash value that toggles the side to move. This hash is XORed with the position hash when the
/// turn changes.
pub fn zobrist_black_to_move() -> Zobrist {
    unsafe { ZOBRIST_BLACK_TO_MOVE }
}

/// Returns the Zobrist hash value for a specific castling right combination. This hash is used to update the position
/// signature when castling rights change.
pub fn zobrist_castling(castling_right: CastlingRight) -> Zobrist {
    unsafe { ZOBRIST_CASTLING[usize::from(castling_right.bits())] }
}
