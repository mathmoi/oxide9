use thiserror::Error;

use crate::{
    coordinates::{CoordinatesError, File, Square},
    piece::{Piece, PieceError, PieceType},
    position::Position,
    r#move::{CastlingSide, Move},
};

/// Represents errors that can occur when parsing chess move notation.
///
/// This enum encapsulates various failure modes that might occur when attempting to parse and validate chess move
/// notation strings.
#[derive(Error, Debug)]
pub enum NotationError {
    /// Error when the source square coordinates in the notation are invalid.
    #[error("Invalid coordinate notation: {0}")]
    InvalidFromSquare(CoordinatesError),

    /// Error when the destination square coordinates in the notation are invalid.
    #[error("Invalid to notation: {0}")]
    InvalidToSquare(CoordinatesError),

    /// Error when the promotion piece notation is invalid.
    #[error("Invalid capture notation: {0}")]
    InvalidCaptureNotation(PieceError),

    /// Error when the overall notation format is incorrect.
    #[error("Invalid notation: {0}")]
    InvalidNotation(String),

    /// Error when there is no piece present at the specified source square.
    #[error("There is not a piece at the from square: {0}")]
    NoPieceAtFromSquare(Square),
}

/// Parses a chess move in coordinate notation and converts it to a Move object.
///
/// Coordinate notation represents moves as the source square followed by the destination square, optionally followed by
/// a promotion piece (e.g., "e2e4", "e7e8q").
///
/// # Parameters
/// * `position` - The current chess position
/// * `notation` - The move in coordinate notation (e.g., "e2e4", "e7e8q")
///
/// # Returns
/// * `Ok(Move)` - A valid Move object if the notation is correctly parsed
/// * `Err(NotationError)` - An error indicating why the notation could not be parsed>
///
/// # Errors
/// * `InvalidFromSquare` - If the source square notation is invalid
/// * `NoPieceAtFromSquare` - If there is no piece at the source square
/// * `InvalidCaptureNotation` - If the promotion piece notation is invalid
/// * `InvalidNotation` - If the notation format is incorrect
pub fn parse_coordinate_notation(position: &Position, notation: &str) -> Result<Move, NotationError> {
    let mut begin = 0;
    let end = notation.len() - 1;

    let from = Square::try_from(&notation[begin..begin + 2]).map_err(|e| NotationError::InvalidFromSquare(e))?;
    begin += 2;

    let piece = position[from].ok_or(NotationError::NoPieceAtFromSquare(from))?;

    let mut to = Square::try_from(&notation[begin..begin + 2]).map_err(|e| NotationError::InvalidFromSquare(e))?;
    begin += 2;

    let mut maybe_capture = position[to];

    let maybe_promotion = if begin <= end {
        let promotion = Some(Piece::new(
            piece.color(),
            PieceType::try_from(notation.chars().nth(begin).expect("The value should have enough characters"))
                .map_err(|e| NotationError::InvalidCaptureNotation(e))?,
        ));
        begin += 1;
        promotion
    } else {
        None
    };

    // If there is character remaining the notation is invalid
    if begin < end {
        return Err(NotationError::InvalidNotation(notation.to_string()));
    }

    // If the move is a pawn moving diagonally to an empty square, it must be a prise en passant
    let en_passant = piece.piece_type() == PieceType::Pawn && maybe_capture.is_none() && from.file() != to.file();
    if en_passant {
        maybe_capture = Some(Piece::new(!piece.color(), PieceType::Pawn));
    }

    let mut maybe_castling_side: Option<CastlingSide> = None;
    if piece.piece_type() == PieceType::King {
        // If the move is a king capturing it's own rook, it must be a castling move (chess960)
        if let Some(capture) = maybe_capture {
            if capture.color() == piece.color() && capture.piece_type() == PieceType::Rook {
                let castling_side = if to.file() == position.castling_file(CastlingSide::Kingside) {
                    CastlingSide::Kingside
                } else {
                    CastlingSide::Queenside
                };
                maybe_castling_side = Some(castling_side);
                to = Square::new(position.castling_file(castling_side), to.rank());
                maybe_capture = None;
            }
        }

        // If the move is a king from file e to g or c, it must be a castling move
        if from.file() == File::E {
            if to.file() == File::G {
                maybe_castling_side = Some(CastlingSide::Kingside);
            } else if to.file() == File::C {
                maybe_castling_side = Some(CastlingSide::Queenside);
            }
        }
    }

    if let Some(promotion) = maybe_promotion {
        if let Some(capture) = maybe_capture {
            return Ok(Move::new_capture_promotion(from, to, piece, capture, promotion));
        }
        return Ok(Move::new_promotion(from, to, piece, promotion));
    }

    if let Some(capture) = maybe_capture {
        if en_passant {
            return Ok(Move::new_en_passant(from, to, piece));
        }
        return Ok(Move::new_capture(from, to, piece, capture));
    }

    if let Some(castling_side) = maybe_castling_side {
        return Ok(Move::new_castling(from, to, piece, castling_side));
    }

    if piece.piece_type() == PieceType::Pawn && (from.rank() - to.rank()).abs() == 2 {
        return Ok(Move::new_two_square_pawn_push(from, to, piece));
    }

    Ok(Move::new(from, to, piece))
}
