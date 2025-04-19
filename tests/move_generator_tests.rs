use colored::*;
use oxide9::{
    coordinates::{Rank, Square},
    move_gen::{generation::generate_all_moves, move_list::MoveList},
    notation::{parse_coordinate_notation, NotationError},
    piece::{Piece, PieceType},
    position::Position,
    r#move::{CastlingSide, Move},
    zobrist::Zobrist,
};
use serde::Deserialize;
use std::{collections::HashSet, fs::File, io::BufReader, path::PathBuf, time::Instant};
use thiserror::Error;

const EXIT_FAILURE: i32 = 1;
const CARGO_MANIFEST_DIR_ENV_VARIABLE: &str = "CARGO_MANIFEST_DIR";

//======================================================================================================================
// Error handling
//======================================================================================================================

/// Errors that are related to the test harness.
#[derive(Error, Debug)]
enum TestHarnessError {
    #[error("The {} environment variable cannot be red", CARGO_MANIFEST_DIR_ENV_VARIABLE)]
    ManifestDirNotFound,

    #[error("Resource path not found: {0:?}")]
    ResourcePathNotFound(PathBuf),

    #[error("Cannot read the test data file ({0:?})")]
    CannotReadTestDataFile(PathBuf),

    #[error("Cannot parse the test data file: {0}")]
    CannotParseTestDataFile(#[from] serde_json::Error),
}

/// Errors that are related to the test data.
#[derive(Error, Debug)]
enum TestDataError {
    #[error("Cannot parse \"{0}\" as a square")]
    CannotParseSquare(String),

    #[error("Cannot parse \"{0}\" as a piece")]
    CannotParsePiece(String),

    #[error("Missing captured piece for move with type Capture or PromotionCapture")]
    MissingCapturedPiece,

    #[error("Missing promotion piece for move with type Promotion or PromotionCapture")]
    MissingPromotionPiece,

    #[error("Unable to parse the fen string : \"{0}\"")]
    UnableToParseFen(String),
}

/// Errors used when tests fail.
#[derive(Error, Debug)]
enum TestFailureError {
    #[error("Missing moves during move generation: {0:?}")]
    MissingMoves(HashSet<Move>),

    #[error("Extra moves during move generation: {0:?}")]
    ExtraMoves(HashSet<Move>),

    #[error("Unexpected position after making a move ({mv:?})\n\nOriginal:\n{original}\n\nExpected:\n{expected}\n\nActual:\n{actual}\n")]
    UnexpectedPositionAfterMake { mv: Move, original: String, expected: String, actual: String },

    #[error("Unexpected position after unmaking a move ({mv:?})\n\nOriginal:\n{original}\n\nActual:\n{actual}\n")]
    UnexpectedPositionAfterUnmake { mv: Move, original: String, actual: String },

    #[error("Unexpected hash after making a move ({mv:?})\n\nOriginal:\n{original}\n\nExpected:\n{expected}\n\nActual:\n{actual}\n")]
    UnexpectedHashAfterMake { mv: Move, original: Zobrist, expected: Zobrist, actual: Zobrist },

    #[error("Unexpected hash after unmaking a move ({mv:?})\n\nOriginal:\n{original}\n\nActual:\n{actual}\n")]
    UnexpectedHashAfterUnmake { mv: Move, original: Zobrist, actual: Zobrist },

    #[error("Error generating UCI notation for move ({mv:?})\n\nExpected: {expected}\n\nActual: {actual}")]
    ErrorGeneratingUciNotation { mv: Move, expected: String, actual: String },

    #[error("Error while parsing UCI notation ({uci:?})\n\nError: {error}")]
    ErrorWhileParsingUciNotation { uci: String, error: NotationError },

    #[error("Error parsing UCI notation ({uci:?})\n\nExpected: {expected:?}\n\nActual: {actual:?}")]
    ErrorParsingUciNotation { uci: String, expected: Move, actual: Move },
}

/// Global errors for this module.
#[derive(Error, Debug)]
enum MoveGeneratorTestError {
    #[error("Test harness error: {}", .0)]
    TestHarnessError(#[from] TestHarnessError),

    #[error("Test data parsing error: {}", .0)]
    TestDataParsingError(#[from] TestDataError),

    #[error("---- {} ----\n{}", .test_name, .test_failure_error)]
    TestFailed { test_name: String, test_failure_error: TestFailureError },

    #[error("Error reading the configuration file: {0}")]
    ConfigError(#[from] oxide9::config::ConfigError),
}

//======================================================================================================================
// Test data structures
//======================================================================================================================

/// A test case for the move generator.
#[derive(Debug, Deserialize)]
struct Test {
    fen: String,
    description: String,
    moves: Vec<TestMove>,
}

/// A move in the test data.
#[derive(Debug, Deserialize)]
struct TestMove {
    #[serde(rename = "move")]
    details: TestMoveDetails,
    uci: String,
    fen: String,
}

/// The type of move in the test data.
#[derive(Debug, Deserialize)]
enum MoveType {
    Basic,
    Capture,
    KingSideCastle,
    QueenSideCastle,
    EnPassant,
    Promotion,
    PromotionCapture,
    TwoSquarePawnPush,
}

/// The details of a move in the test data.
#[derive(Debug, Deserialize)]
struct TestMoveDetails {
    from: String,
    to: String,
    piece: char,
    capture: Option<char>,
    promotion: Option<char>,
    #[serde(rename = "type")]
    move_type: MoveType,
}

//======================================================================================================================
// Test data reading and parsing
//======================================================================================================================

fn parse_square(value: &str) -> Result<Square, TestDataError> {
    Square::try_from(value).map_err(|_| TestDataError::CannotParseSquare(value.to_string()))
}

fn parse_piece(value: char) -> Result<Piece, TestDataError> {
    Piece::try_from(value).map_err(|_| TestDataError::CannotParsePiece(value.to_string()))
}

fn parse_optional_piece(value: Option<char>) -> Result<Option<Piece>, TestDataError> {
    value.map(|c| Piece::try_from(c).map_err(|_| TestDataError::CannotParsePiece(c.to_string()))).transpose()
}

/// Convert a `TestMoveDetails`, which is a move in the test data, to a `Move`.
impl TryFrom<&TestMoveDetails> for Move {
    type Error = MoveGeneratorTestError;

    fn try_from(value: &TestMoveDetails) -> Result<Self, Self::Error> {
        let from_square = parse_square(&value.from)?;
        let to_square = parse_square(&value.to)?;
        let piece = parse_piece(value.piece)?;
        let capture = parse_optional_piece(value.capture)?;
        let promotion = parse_optional_piece(value.promotion)?;

        Ok(match value.move_type {
            MoveType::Basic => {
                let color = piece.color();
                if piece.piece_type() == PieceType::Pawn
                    && from_square.rank() == Rank::R2.relative_to_color(color)
                    && to_square.rank() == Rank::R4.relative_to_color(color)
                {
                    Move::new_two_square_pawn_push(from_square, to_square, piece)
                } else {
                    Move::new(from_square, to_square, piece)
                }
            }
            MoveType::Capture => {
                Move::new_capture(from_square, to_square, piece, capture.ok_or(TestDataError::MissingCapturedPiece)?)
            }
            MoveType::KingSideCastle => Move::new_castling(from_square, to_square, piece, CastlingSide::Kingside),
            MoveType::QueenSideCastle => Move::new_castling(from_square, to_square, piece, CastlingSide::Queenside),
            MoveType::EnPassant => Move::new_en_passant(from_square, to_square, piece),
            MoveType::Promotion => Move::new_promotion(
                from_square,
                to_square,
                piece,
                promotion.ok_or(TestDataError::MissingPromotionPiece)?,
            ),
            MoveType::PromotionCapture => Move::new_capture_promotion(
                from_square,
                to_square,
                piece,
                capture.ok_or(TestDataError::MissingCapturedPiece)?,
                promotion.ok_or(TestDataError::MissingPromotionPiece)?,
            ),
            MoveType::TwoSquarePawnPush => Move::new_two_square_pawn_push(from_square, to_square, piece),
        })
    }
}

/// Read the tests data from the file.
fn read_tests_data() -> Result<Vec<Test>, MoveGeneratorTestError> {
    let tests_file_path = get_resource_path("assets/tests/move_generator_tests.json")?;
    let file = File::open(&tests_file_path).map_err(|_| TestHarnessError::CannotReadTestDataFile(tests_file_path))?;
    let reader = BufReader::new(file);
    let tests: Vec<Test> = serde_json::from_reader(reader).map_err(TestHarnessError::CannotParseTestDataFile)?;
    Ok(tests)
}

//======================================================================================================================
// Test harness
//======================================================================================================================

/// Compare two sets of moves and return the missing and extra moves.
fn compare_moves_set(expected: &[Move], actual: &[Move]) -> (HashSet<Move>, HashSet<Move>) {
    let expected_set: HashSet<_> = expected.iter().copied().collect();
    let actual_set: HashSet<_> = actual.iter().copied().collect();

    let missing: HashSet<_> = expected_set.difference(&actual_set).copied().collect();
    let extra: HashSet<_> = actual_set.difference(&expected_set).copied().collect();

    (missing, extra)
}

fn test_move_generation(test: &Test) -> Result<(), MoveGeneratorTestError> {
    // Prepare the position and the expected moves.
    let position = Position::new_from_fen(&test.fen).or(Err(TestDataError::UnableToParseFen(test.fen.clone())))?;
    let expected_moves: Result<Vec<Move>, MoveGeneratorTestError> =
        test.moves.iter().map(|m| Move::try_from(&m.details)).collect();
    let expected_moves = expected_moves?;

    // Generate the moves
    let mut pseudo_legal_moves = MoveList::default();
    generate_all_moves(&position, &mut pseudo_legal_moves);
    let legal_moves: Vec<Move> = pseudo_legal_moves.iter().filter(|m| position.is_legal(*m)).collect();

    // Compare the moves
    let (missing, extra) = compare_moves_set(&expected_moves, &legal_moves);

    if !missing.is_empty() {
        return Err(MoveGeneratorTestError::TestFailed {
            test_name: test.description.clone(),
            test_failure_error: TestFailureError::MissingMoves(missing),
        });
    }

    if !extra.is_empty() {
        return Err(MoveGeneratorTestError::TestFailed {
            test_name: test.description.clone(),
            test_failure_error: TestFailureError::ExtraMoves(extra),
        });
    }

    Ok(())
}

fn test_move_execution(test: &Test) -> Result<(), MoveGeneratorTestError> {
    let test_position = Position::new_from_fen(&test.fen).or(Err(TestDataError::UnableToParseFen(test.fen.clone())))?;

    for test_move in test.moves.iter() {
        let mut position = test_position.clone();
        let mv = Move::try_from(&test_move.details)?;

        // Make the move
        position.make(mv);
        let actual_fen = position.to_fen();
        let expected_position =
            Position::new_from_fen(&test_move.fen).or(Err(TestDataError::UnableToParseFen(test_move.fen.clone())))?;
        if test_move.fen != actual_fen {
            return Err(MoveGeneratorTestError::TestFailed {
                test_name: test.description.clone(),
                test_failure_error: TestFailureError::UnexpectedPositionAfterMake {
                    mv,
                    original: format!("{test_position:+#}"),
                    expected: format!("{expected_position:#}") + "\n" + &test_move.fen,
                    actual: format!("{position:#}") + "\n\n" + &actual_fen,
                },
            });
        }

        if expected_position.hash() != position.hash() {
            return Err(MoveGeneratorTestError::TestFailed {
                test_name: test.description.clone(),
                test_failure_error: TestFailureError::UnexpectedHashAfterMake {
                    mv,
                    original: test_position.hash(),
                    expected: expected_position.hash(),
                    actual: position.hash(),
                },
            });
        }

        // Unmake the move
        position.unmake();
        let actual_fen = position.to_fen();
        if test.fen != actual_fen {
            return Err(MoveGeneratorTestError::TestFailed {
                test_name: test.description.clone(),
                test_failure_error: TestFailureError::UnexpectedPositionAfterUnmake {
                    mv,
                    original: format!("{test_position:+#}"),
                    actual: format!("{position:#}") + "\n\n" + &actual_fen,
                },
            });
        }

        if test_position.hash() != position.hash() {
            return Err(MoveGeneratorTestError::TestFailed {
                test_name: test.description.clone(),
                test_failure_error: TestFailureError::UnexpectedHashAfterUnmake {
                    mv,
                    original: test_position.hash(),
                    actual: position.hash(),
                },
            });
        }
    }

    Ok(())
}

fn test_uci_notation_generation(test: &Test) -> Result<(), MoveGeneratorTestError> {
    for test_move in test.moves.iter() {
        let mv = Move::try_from(&test_move.details)?;
        let expected = test_move.uci.as_str();
        let actual = mv.to_uci_string();
        if expected != actual {
            return Err(MoveGeneratorTestError::TestFailed {
                test_name: test.description.clone(),
                test_failure_error: TestFailureError::ErrorGeneratingUciNotation {
                    mv,
                    expected: expected.to_string(),
                    actual,
                },
            });
        }
    }

    Ok(())
}

fn test_uci_notation_parser(test: &Test) -> Result<(), MoveGeneratorTestError> {
    let position = Position::new_from_fen(&test.fen).or(Err(TestDataError::UnableToParseFen(test.fen.clone())))?;

    for test_move in test.moves.iter() {
        let uci = test_move.uci.as_str();
        let expected = Move::try_from(&test_move.details)?;
        let actual = parse_coordinate_notation(&position, uci).map_err(|e| MoveGeneratorTestError::TestFailed {
            test_name: test.description.clone(),
            test_failure_error: TestFailureError::ErrorWhileParsingUciNotation { uci: uci.to_string(), error: e },
        })?;
        if expected != actual {
            return Err(MoveGeneratorTestError::TestFailed {
                test_name: test.description.clone(),
                test_failure_error: TestFailureError::ErrorParsingUciNotation {
                    uci: uci.to_string(),
                    expected,
                    actual,
                },
            });
        }
    }

    Ok(())
}

/// Run a single test case.
fn run_test(test: Test) -> Result<(), MoveGeneratorTestError> {
    test_move_generation(&test)?;
    test_move_execution(&test)?;
    test_uci_notation_generation(&test)?;
    test_uci_notation_parser(&test)?;
    Ok(())
}

/// Run all the tests.
fn run_tests() -> Result<(), MoveGeneratorTestError> {
    let tests = read_tests_data()?;

    println!("\nrunning {} tests", tests.len());

    let start = Instant::now();
    let mut passed = 0;
    let mut failed = 0;
    let mut failures: Vec<MoveGeneratorTestError> = Vec::new();
    for test in tests {
        print!("test {} ...", test.description);
        let result_string = match run_test(test) {
            Ok(_) => {
                passed += 1;
                "ok".green()
            }

            Err(MoveGeneratorTestError::TestFailed { test_name, test_failure_error }) => {
                failed += 1;
                failures.push(MoveGeneratorTestError::TestFailed { test_name, test_failure_error });
                "FAILED".red()
            }

            Err(_) => {
                failed += 1;
                "FAILED".red()
            }
        };
        println!(" {}", result_string);
    }
    let seconds = start.elapsed().as_secs_f32();

    for failure in failures {
        println!("\n{}", failure)
    }

    println!(
        "\ntest result: {}. {} passed; {} failed; finished in {:.2}s\n",
        if failed == 0 { "ok".green() } else { "FAILED".red() },
        passed,
        failed,
        seconds
    );

    Ok(())
}

//======================================================================================================================
// Main function and helpers
//======================================================================================================================

/// Get the path to a resource file.
fn get_resource_path(relative_path: &str) -> Result<PathBuf, TestHarnessError> {
    let mut path = std::env::current_dir().map_err(|_| TestHarnessError::ManifestDirNotFound)?;
    path.push(relative_path);

    if !path.exists() {
        return Err(TestHarnessError::ResourcePathNotFound(path));
    }

    Ok(path)
}

/// The main function for the test harness. It will run the tests and print any unexpected errors.
fn main() -> Result<(), MoveGeneratorTestError> {
    oxide9::initialize();

    if let Err(error) = run_tests() {
        eprintln!("{}", error);
        std::process::exit(EXIT_FAILURE)
    }
    Ok(())
}
