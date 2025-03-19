use crate::{
    bitboard::Bitboard,
    coordinates::{File, Rank, Square},
    piece::{Color, Piece, PieceType},
    position::{OccupancyFilter, Position},
    r#move::{CastlingRight, CastlingSide, Move},
};

use super::{attacks::attacks_from, move_list::MoveList};

/// Enum to specify the type of moves to generate.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MoveGenerationType {
    All = 0,
    Quiet = 1,
    Captures = 2,
    Evasions = 3,
}

impl MoveGenerationType {
    pub const ALL_VALUE: u8 = 0;
    pub const QUIET_VALUE: u8 = 1;
    pub const CAPTURES_VALUE: u8 = 2;
    pub const EVASIONS_VALUE: u8 = 3;
}

impl From<u8> for MoveGenerationType {
    fn from(value: u8) -> Self {
        match value {
            MoveGenerationType::ALL_VALUE => MoveGenerationType::All,
            MoveGenerationType::QUIET_VALUE => MoveGenerationType::Quiet,
            MoveGenerationType::CAPTURES_VALUE => MoveGenerationType::Captures,
            MoveGenerationType::EVASIONS_VALUE => MoveGenerationType::Evasions,
            _ => panic!("Invalid MoveGenerationType value"),
        }
    }
}

fn collect_pawn_moves(bb_to: Bitboard, get_from: impl Fn(Square) -> Square, mut add_moves: impl FnMut(Square, Square)) {
    for to_sq in bb_to {
        let from_sq = get_from(to_sq);
        add_moves(from_sq, to_sq);
    }
}

fn left_shift<const COLOR_VALUE: u8>(bb: Bitboard, shift: u32) -> Bitboard {
    match COLOR_VALUE {
        Color::WHITE_VALUE => bb << shift,
        Color::BLACK_VALUE => bb >> shift,
        _ => panic!("Invalid color value"),
    }
}

fn add_captures_promotions<const COLOR_VALUE: u8>(
    from_sq: Square,
    to_sq: Square,
    piece: Piece,
    position: &Position,
    list: &mut MoveList,
) {
    let color: Color = COLOR_VALUE.into();
    list.push(Move::new_capture_promotion(
        from_sq,
        to_sq,
        piece,
        unsafe { position[to_sq].unwrap_unchecked() },
        Piece::new(color, PieceType::Queen),
    ));
    list.push(Move::new_capture_promotion(
        from_sq,
        to_sq,
        piece,
        unsafe { position[to_sq].unwrap_unchecked() },
        Piece::new(color, PieceType::Rook),
    ));
    list.push(Move::new_capture_promotion(
        from_sq,
        to_sq,
        piece,
        unsafe { position[to_sq].unwrap_unchecked() },
        Piece::new(color, PieceType::Knight),
    ));
    list.push(Move::new_capture_promotion(
        from_sq,
        to_sq,
        piece,
        unsafe { position[to_sq].unwrap_unchecked() },
        Piece::new(color, PieceType::Bishop),
    ));
}

fn generate_pawn_moves<const TYPE: u8, const COLOR: u8>(position: &Position, targets: Bitboard, list: &mut MoveList) {
    let generation_type: MoveGenerationType = TYPE.into();
    let color: Color = COLOR.into();

    let direction_factor = match color {
        Color::White => 1,
        Color::Black => -1,
    };
    let bb_rank_4: Bitboard = Bitboard::from(Rank::R4.relative_to_color(color));
    let bb_rank_8: Bitboard = Bitboard::from(Rank::R8.relative_to_color(color));
    let bb_file_a: Bitboard = Bitboard::from(File::A);
    let bb_file_h: Bitboard = Bitboard::from(File::H);
    let bb_occupied: Bitboard = position.occupied(OccupancyFilter::All);
    let bb_them: Bitboard = position.occupied(!color);
    let piece = Piece::new(COLOR.into(), PieceType::Pawn);
    let bb_from = position.occupied(piece);

    if matches!(generation_type, MoveGenerationType::All | MoveGenerationType::Evasions | MoveGenerationType::Quiet) {
        // Single pawn push
        let mut bb_to = left_shift::<COLOR>(bb_from, 8) & !bb_occupied & !bb_rank_8;
        collect_pawn_moves(
            bb_to & targets,
            |sq| unsafe { sq.down_unchecked(direction_factor) },
            |from_sq, to_sq| {
                list.push(Move::new(from_sq, to_sq, piece));
            },
        );

        // Double pawn push
        bb_to = left_shift::<COLOR>(bb_to, 8) & !bb_occupied & bb_rank_4;
        collect_pawn_moves(
            bb_to & targets,
            |sq| unsafe { sq.down_unchecked(direction_factor * 2) },
            |from_sq, to_sq| {
                list.push(Move::new_two_square_pawn_push(from_sq, to_sq, piece));
            },
        );
    }

    if matches!(generation_type, MoveGenerationType::All | MoveGenerationType::Evasions | MoveGenerationType::Captures)
    {
        // Captures towards file A
        let mut bb_to = bb_from & !bb_file_a;
        bb_to = match color {
            Color::White => bb_to << 7,
            Color::Black => bb_to >> 9,
        };
        bb_to &= bb_them;
        collect_pawn_moves(
            bb_to & !bb_rank_8 & targets,
            |sq| unsafe { sq.down_unchecked(direction_factor).right_unchecked(1) },
            |from_sq, to_sq| {
                list.push(Move::new_capture(from_sq, to_sq, piece, unsafe { position[to_sq].unwrap_unchecked() }));
            },
        );
        collect_pawn_moves(
            bb_to & bb_rank_8 & targets,
            |sq| unsafe { sq.down_unchecked(direction_factor).right_unchecked(1) },
            |from_sq, to_sq| {
                add_captures_promotions::<COLOR>(from_sq, to_sq, piece, position, list);
            },
        );

        // Captures towards file H
        let mut bb_to = bb_from & !bb_file_h;
        bb_to = match color {
            Color::White => bb_to << 9,
            Color::Black => bb_to >> 7,
        };
        bb_to &= bb_them;
        collect_pawn_moves(
            bb_to & !bb_rank_8 & targets,
            |sq| unsafe { sq.down_unchecked(direction_factor).left_unchecked(1) },
            |from_sq, to_sq| {
                list.push(Move::new_capture(from_sq, to_sq, piece, unsafe { position[to_sq].unwrap_unchecked() }));
            },
        );
        collect_pawn_moves(
            bb_to & bb_rank_8 & targets,
            |sq| unsafe { sq.down_unchecked(direction_factor).left_unchecked(1) },
            |from_sq, to_sq| {
                add_captures_promotions::<COLOR>(from_sq, to_sq, piece, position, list);
            },
        );

        // Promotions
        bb_to = match color {
            Color::White => bb_from << 8,
            Color::Black => bb_from >> 8,
        } & !bb_occupied
            & bb_rank_8;
        collect_pawn_moves(
            bb_to & targets,
            |sq| unsafe { sq.down_unchecked(direction_factor) },
            |from_sq, to_sq| {
                list.push(Move::new_promotion(from_sq, to_sq, piece, Piece::new(color, PieceType::Queen)));
                list.push(Move::new_promotion(from_sq, to_sq, piece, Piece::new(color, PieceType::Rook)));
                list.push(Move::new_promotion(from_sq, to_sq, piece, Piece::new(color, PieceType::Knight)));
                list.push(Move::new_promotion(from_sq, to_sq, piece, Piece::new(color, PieceType::Bishop)));
            },
        );

        // En passant. En passant square are tricky to calculate in regards to target because it captures on a different
        // square than the pawn moves to. Because of this, we do not use the targets bitboard to filter the moves. This
        // means that when generating evasions, we will generate pseudo-legal moves that leave the king in check. It
        // also possible that we generate moves that put the own king in check by discovering a check. Theses moves will
        // need to be filtered out by the legality check.
        if let Some(sq) = position.en_passant_square() {
            if sq.file() != File::H {
                let from_sq = unsafe { sq.down_unchecked(direction_factor).right_unchecked(1) };
                if position[from_sq] == Some(Piece::new(color, PieceType::Pawn)) {
                    list.push(Move::new_en_passant(from_sq, sq, piece));
                }
            }
            if sq.file() != File::A {
                let from_sq = unsafe { sq.down_unchecked(direction_factor).left_unchecked(1) };
                if position[from_sq] == Some(Piece::new(color, PieceType::Pawn)) {
                    list.push(Move::new_en_passant(from_sq, sq, piece));
                }
            }
        }
    }
}

fn generate_piece_moves<const COLOR: u8, const PIECE_TYPE: u8>(
    position: &Position,
    targets: Bitboard,
    list: &mut MoveList,
) {
    let piece = Piece::new(COLOR.into(), PIECE_TYPE.into());
    let bb_from = position.occupied(piece);

    for from_sq in bb_from {
        let bb_to = attacks_from::<PIECE_TYPE>(position.occupied(OccupancyFilter::All), from_sq) & targets;
        for to_sq in bb_to {
            let capture = position[to_sq];
            match capture {
                Some(captured_piece) => {
                    list.push(Move::new_capture(from_sq, to_sq, piece, captured_piece));
                }
                None => {
                    list.push(Move::new(from_sq, to_sq, piece));
                }
            }
        }
    }
}

fn generate_castlings<const COLOR: u8, const SIDE: u8>(position: &Position, list: &mut MoveList) {
    let color = Color::from(COLOR);
    let side = match SIDE {
        CastlingSide::KINGSIDE_VALUE => CastlingSide::Kingside,
        CastlingSide::QUEENSIDE_VALUE => CastlingSide::Queenside,
        _ => panic!("Invalid castling side value"),
    };
    let castling_right = CastlingRight::new(color, side);

    if !position.castling_availability().contains(castling_right) {
        return;
    }

    let rank = Rank::R1.relative_to_color(color);

    let king_final_file = match side {
        CastlingSide::Kingside => File::G,
        CastlingSide::Queenside => File::C,
    };
    let king_final_sq = Square::new(king_final_file, rank);

    let rook_file = position.castling_file(side);
    let rook_sq = Square::new(rook_file, rank);
    let rook_final_file = match side {
        CastlingSide::Kingside => File::F,
        CastlingSide::Queenside => File::D,
    };
    let rook_final_sq = Square::new(rook_final_file, rank);

    // If there is not movement (poissible in chess960), it is not possible to castle.
    let king_sq = position.king_square(color);
    if king_sq == king_final_sq && rook_sq == rook_final_sq {
        return;
    }

    let king_bb = Bitboard::from(king_sq);
    let king_travel_bb = Bitboard::between(king_sq, king_final_sq);
    let rook_travel_bb = Bitboard::between(rook_sq, rook_final_sq);
    let occupied_bb = position.occupied(OccupancyFilter::All) ^ (king_bb | rook_sq);

    // If any of the travel squares are occupied, it is not possible to castle.
    if !((king_travel_bb | rook_travel_bb) & occupied_bb).has_none() {
        return;
    }

    // TODO : Stockfish checks for this in the legality check. Should we be doing the same?
    // If any of the travel squares are attacked, it is not possible to castle.
    for sq in king_travel_bb | king_bb | king_final_sq {
        if position.is_attacked(sq, occupied_bb, !color) {
            return;
        }
    }

    let king = Piece::new(color, PieceType::King);
    list.push(Move::new_castling(king_sq, king_final_sq, king, side));
}

fn generate_moves_color<const TYPE: u8, const COLOR: u8>(position: &Position, list: &mut MoveList) {
    debug_assert!(TYPE != MoveGenerationType::EVASIONS_VALUE || position.is_check());

    let color = Color::from(COLOR);
    let mut targets = Bitboard::EMPTY;

    let checkers = position.checkers();

    // Non-king moves. If we are generating evasions and there are multiple checkers, non-king moves don't need to be
    // generated.
    if TYPE != MoveGenerationType::EVASIONS_VALUE || !checkers.has_many() {
        targets = match TYPE {
            MoveGenerationType::ALL_VALUE => !position.occupied(color),
            MoveGenerationType::QUIET_VALUE => !position.occupied(OccupancyFilter::All),
            MoveGenerationType::CAPTURES_VALUE => position.occupied(!color),
            MoveGenerationType::EVASIONS_VALUE => {
                Bitboard::between(
                    position.king_square(color),
                    unsafe { checkers.lsb().unwrap_unchecked() }, // Safe because in evasions, there is at least one checker.
                )
            }
            _ => panic!("Invalid MoveGenerationType value"),
        };

        generate_piece_moves::<COLOR, { PieceType::KNIGHT_VALUE }>(position, targets, list);
        generate_piece_moves::<COLOR, { PieceType::ROOK_VALUE }>(position, targets, list);
        generate_piece_moves::<COLOR, { PieceType::BISHOP_VALUE }>(position, targets, list);
        generate_piece_moves::<COLOR, { PieceType::QUEEN_VALUE }>(position, targets, list);
        generate_pawn_moves::<TYPE, COLOR>(position, targets, list);
    }

    // Kings moves, regular and castlings.
    targets = if TYPE == MoveGenerationType::EVASIONS_VALUE { !position.occupied(color) } else { targets };
    generate_piece_moves::<COLOR, { PieceType::KING_VALUE }>(position, targets, list);
    if TYPE == MoveGenerationType::ALL_VALUE || TYPE == MoveGenerationType::QUIET_VALUE {
        generate_castlings::<COLOR, { CastlingSide::KINGSIDE_VALUE }>(position, list);
        generate_castlings::<COLOR, { CastlingSide::QUEENSIDE_VALUE }>(position, list);
    }
}

/// Generates all pseudo-legal moves for the given position.
pub fn generate_moves<const TYPE: u8>(position: &Position, list: &mut MoveList) {
    match position.side_to_move() {
        Color::White => generate_moves_color::<TYPE, { Color::WHITE_VALUE }>(position, list),
        Color::Black => generate_moves_color::<TYPE, { Color::BLACK_VALUE }>(position, list),
    }
}

pub fn generate_all_moves(position: &Position, list: &mut MoveList) {
    if position.is_check() {
        generate_moves::<{ MoveGenerationType::EVASIONS_VALUE }>(&position, list);
    } else {
        generate_moves::<{ MoveGenerationType::ALL_VALUE }>(&position, list);
    }
}
