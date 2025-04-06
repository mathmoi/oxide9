use std::time::Duration;

use human_repr::{HumanCount, HumanDuration};
use terminal_size::{terminal_size, Height, Width};
use thiserror::Error;

use crate::{
    config::get_config,
    position::{FenError, Position},
    r#move::Move,
    search::{ProgressType, Search, SearchStats},
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

    println!("Analyzing position:\n\n{}\n\n{}\n", position.to_compact_string(), fen);
    print_header();

    let mut search = Search::new(&mut position, depth as u16, report_progress);

    let start = std::time::Instant::now();
    search.start();
    let elapsed = start.elapsed();

    let stats = search.stats();
    print_stats(elapsed, stats);

    Ok(())
}

//======================================================================================================================
// functions responsible for printing the analysis results
//======================================================================================================================

const DEPTH_COLUMN_WIDTH: usize = 5;
const TIME_COLUMN_WIDTH: usize = 8;
const SCORE_COLUMN_WIDTH: usize = 6;
const NODES_COLUMN_WIDTH: usize = 6;

const EXTRA_SEPARATOR_CHAR_COUNT: usize = 16;

const DEPTH_COLUMN_NAME: &str = "Depth";
const TIME_COLUMN_NAME: &str = "Time";
const SCORE_COLUMN_NAME: &str = "Score";
const NODES_COLUMN_NAME: &str = "Nodes";
const PV_COLUMN_NAME: &str = "Principal Variation";

/// Returns the current terminal width in characters.
///
/// Attempts to determine the actual terminal width. If the terminal size cannot be determined, returns a default width
/// of 80 characters.
///
/// # Returns
///
/// * The width of the terminal in characters (columns)
fn get_terminal_width() -> usize {
    if let Some((Width(width), Height(_))) = terminal_size() {
        width as usize
    } else {
        80 // Default width if terminal size cannot be determined
    }
}

/// Calculates the width available for displaying the Principal Variation (PV) column.
///
/// Determines the PV column width by subtracting the widths of all other columns and separator characters from the
/// total terminal width.
///
/// # Parameters
///
/// * `terminal_width` - The total width of the terminal in characters
///
/// # Returns
///
/// * The width in characters available for displaying the PV column
fn get_pv_column_width(terminal_width: usize) -> usize {
    terminal_width
        - DEPTH_COLUMN_WIDTH
        - TIME_COLUMN_WIDTH
        - SCORE_COLUMN_WIDTH
        - NODES_COLUMN_WIDTH
        - EXTRA_SEPARATOR_CHAR_COUNT
}

/// Prints the header of the search information table.
///
/// Displays a formatted table header with columns for depth, time, score, nodes, and principal variation (PV). The
/// table is automatically sized to fit the current terminal width, with the PV column taking up the remaining available
/// space.
fn print_header() {
    let terminal_width = get_terminal_width();
    let pv_column_width = get_pv_column_width(terminal_width);

    // upper border
    println!(
        "┌─{:─<DEPTH_COLUMN_WIDTH$}─┬─{:─<TIME_COLUMN_WIDTH$}─┬─{:─<SCORE_COLUMN_WIDTH$}─┬─{:─<NODES_COLUMN_WIDTH$}─┬─{:─<pv_column_width$}─┐",
        "", "", "", "", ""
    );

    // header
    println!(
        "│ {:^DEPTH_COLUMN_WIDTH$} │ {:^TIME_COLUMN_WIDTH$} │ {:^SCORE_COLUMN_WIDTH$} │ {:^NODES_COLUMN_WIDTH$} │ {:<pv_column_width$} │",
        DEPTH_COLUMN_NAME,
        TIME_COLUMN_NAME,
        SCORE_COLUMN_NAME,
        NODES_COLUMN_NAME,
        PV_COLUMN_NAME
    );

    // separator
    println!(
        "├─{:─<DEPTH_COLUMN_WIDTH$}─┼─{:─<TIME_COLUMN_WIDTH$}─┼─{:─<SCORE_COLUMN_WIDTH$}─┼─{:─<NODES_COLUMN_WIDTH$}─┼─{:─<pv_column_width$}─┤",
        "", "", "", "", ""
    );
}

/// Reports search progress by printing information about the current search state.
///
/// Displays a formatted row in the search information table with different formatting depending on the type of progress
/// being reported.
///
/// # Parameters
///
/// * `progress_type` - The type of progress to report, which can be:
///   - `Iteration`: Regular depth iteration progress
///   - `NewBestMove`: A new best move has been found
///   - `NewMoveAtRoot`: Search has moved to a new move at the root position
///
/// # Display Format
///
/// The function prints a table row with the following columns:
/// - Depth: Current search depth with a suffix indicating the progress type
/// - Time: Elapsed search time in a human-readable format
/// - Score: Evaluation score or move counting information
/// - Nodes: Number of nodes searched in a human-readable format
/// - PV: Principal variation or current move information with nodes per second
///
/// For `Iteration` and `NewBestMove`, a new line is printed after the information. For `NewMoveAtRoot`, the cursor
/// remains on the current line for updates.
fn report_progress(progress_type: ProgressType) {
    let terminal_width = get_terminal_width();
    let pv_column_width = get_pv_column_width(terminal_width);

    let (new_line, depth, depth_suffix, elapsed, score, nodes, pv) = match progress_type {
        ProgressType::Iteration { depth, elapsed, score, nodes, pv } => {
            (true, depth, "-> ", elapsed, score.to_string(), nodes, get_pv_string(pv, pv_column_width))
        }

        ProgressType::NewBestMove { depth, elapsed, score, nodes, pv } => {
            (true, depth, "   ", elapsed, score.to_string(), nodes, get_pv_string(pv, pv_column_width))
        }

        ProgressType::NewMoveAtRoot { depth, elapsed, nodes, move_number, move_count, mv } => (
            false,
            depth,
            "...",
            elapsed,
            format!("{}/{}", move_number, move_count),
            nodes,
            vec![format!("{} ({})", mv.to_uci_string(), (nodes as f64 / elapsed.as_secs_f64()).human_count("nps"))],
        ),
    };

    // Return to the beginning of the line
    print!("\r");

    // Print the progress information
    print!(
        "│ {:>DEPTH_COLUMN_WIDTH$} │ {:>TIME_COLUMN_WIDTH$} │ {:>SCORE_COLUMN_WIDTH$} │ {:>NODES_COLUMN_WIDTH$} │ {:<pv_column_width$} │",
        depth.to_string() + depth_suffix,
        elapsed.human_duration().to_string(),
        score.to_string(),
        nodes.human_count_bare().to_string(),
        pv[0]
    );

    // Print the principal variation (PV) if there are more lines
    for line in &pv[1..] {
        print!("\n│ {:>DEPTH_COLUMN_WIDTH$} │ {:>TIME_COLUMN_WIDTH$} │ {:>SCORE_COLUMN_WIDTH$} │ {:>NODES_COLUMN_WIDTH$} │ {:<pv_column_width$} │",
            "", "", "", "", line);
    }

    if new_line {
        println!();
    }

    // Flush stdout to ensure changes are displayed immediately
    std::io::Write::flush(&mut std::io::stdout()).unwrap();
}

/// Prints the search statistics footer after completing a search.
///
/// Displays a formatted summary of search performance metrics below the search information table, followed by a bottom
/// border.
///
/// # Parameters
///
/// * `elapsed` - The total time elapsed during the search
/// * `stats` - A reference to SearchStats containing node counts and other statistics
///
/// # Display Format
///
/// The footer includes:
/// - A separator line closing the main table
/// - A statistics line showing time, regular nodes, quiescence nodes, and nodes per second
/// - A bottom border
///
/// Statistics are displayed in precise numerical format if the configuration has `precise` mode enabled, otherwise in
/// human-readable format.
fn print_stats(elapsed: Duration, stats: &SearchStats) {
    let terminal_width = get_terminal_width();
    let pv_column_width = get_pv_column_width(terminal_width);

    println!(
        "├─{:─<DEPTH_COLUMN_WIDTH$}─┴─{:─<TIME_COLUMN_WIDTH$}─┴─{:─<SCORE_COLUMN_WIDTH$}─┴─{:─<NODES_COLUMN_WIDTH$}─┴─{:─<pv_column_width$}─┤",
        "", "", "", "", ""
    );

    let config = get_config();
    let stats_line = if config.precise {
        format!(
            "time={} nodes={} qnodes={} nps={}",
            elapsed.as_secs_f64(),
            stats.nodes,
            stats.qnodes,
            (stats.nodes + stats.qnodes) as f64 / elapsed.as_secs_f64()
        )
    } else {
        format!(
            "time={} nodes={} qnodes={} nps={}",
            elapsed.human_duration(),
            stats.nodes.human_count_bare(),
            stats.qnodes.human_count_bare(),
            ((stats.nodes + stats.qnodes) as f64 / elapsed.as_secs_f64()).human_count_bare()
        )
    };

    for line in split_by_width(&stats_line, get_terminal_width() - 4) {
        println!("│ {:<width$} │", line, width = get_terminal_width() - 4);
    }

    println!("└{:─<width$}┘", "", width = terminal_width - 2);
}

/// Converts a principal variation (PV) move sequence into a space-separated string.
///
/// Takes a slice of Move objects representing the principal variation and converts them into a single string with each
/// move in UCI notation followed by a space.
///
/// # Parameters
///
/// * `pv` - A slice of Move objects representing the principal variation
///
/// # Returns
///
/// * A string containing all moves in the principal variation in UCI notation, with moves separated by spaces
fn get_pv_string(pv: &[Move], pv_column_width: usize) -> Vec<String> {
    let mut pv_str = String::new();
    for mv in pv.iter().rev() {
        pv_str.push_str(&mv.to_uci_string());
        pv_str.push(' ');
    }
    split_by_width(pv_str.trim(), pv_column_width)
}

/// Splits text into multiple lines, ensuring no line exceeds the specified width.
///
/// Breaks a string at word boundaries to create lines that fit within the given maximum width. Words are never split
/// across lines, and each line is as long as possible without exceeding the max width.
///
/// # Parameters
///
/// * `text` - The text to split into multiple lines
/// * `max_width` - The maximum width (in characters) for each line
///
/// # Returns
///
/// * A vector of strings, where each string represents a line that fits within the specified width
fn split_by_width(text: &str, max_width: usize) -> Vec<String> {
    let mut result = Vec::new();
    let mut current_line = String::new();
    let mut current_width = 0;

    for word in text.split_whitespace() {
        let word_len = word.chars().count();

        // If adding this word would exceed max_width
        if current_width > 0 && current_width + 1 + word_len > max_width {
            // Push current line to result and start a new line
            result.push(current_line);
            current_line = word.to_string();
            current_width = word_len;
        } else if current_width == 0 {
            // First word on the line
            current_line = word.to_string();
            current_width = word_len;
        } else {
            // Add word to current line
            current_line.push(' ');
            current_line.push_str(word);
            current_width += 1 + word_len; // +1 for the space
        }
    }

    // Don't forget the last line
    if !current_line.is_empty() {
        result.push(current_line);
    }

    result
}
