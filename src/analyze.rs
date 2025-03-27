use thiserror::Error;

use crate::{
    position::{FenError, Position},
    search::Search,
};

/// Represents errors that can occur while analyzing a chess position.
///
/// This enum provides specific error variants with detailed information
/// about what went wrong during position analysis.
///
/// # Variants
/// * `InvalidFen` - Occurs when the provided FEN string is invalid.
///   Contains the original FEN string and the specific FEN parsing error.
#[derive(Error, Debug)]
pub enum AnalyzeError {
    #[error("Invalid FEN ({}): {:?}", .0, .1)]
    InvalidFen(String, FenError),
}

/// Analyzes a chess position to a specified depth and outputs the results.
///
/// This function takes a position in FEN notation, creates a `Position` object,
/// performs a search to the requested depth, and prints the analysis results
/// to standard output.
///
/// # Parameters
/// * `fen` - A chess position in Forsyth-Edwards Notation (FEN)
/// * `depth` - The depth to search, measured in plies (half-moves)
///
/// # Returns
/// * `Ok(())` - If analysis completes successfully
/// * `Err(AnalyzeError::InvalidFen)` - If the provided FEN string is invalid
///
/// # Side Effects
/// Prints analysis results to standard output.
pub fn analyze(fen: &str, depth: u16) -> Result<(), AnalyzeError> {
    let mut position = Position::new_from_fen(fen).map_err(|e| AnalyzeError::InvalidFen(fen.to_string(), e))?;

    println!("Analyzing position:\n\n{}\n\n{}", position.to_compact_string(), fen);

    let mut search = Search::new(&mut position, depth as u16);
    let moves = search.start();

    for mv in moves.iter().rev() {
        print!("{} ", mv.to_uci_string());
        println!("");
    }

    Ok(())
}
