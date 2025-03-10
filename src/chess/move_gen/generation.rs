use crate::chess::{
    Bitboard, CastlingRight, CastlingSide, Color, File, Move, Piece, PieceType, Position, Rank, Square,
};

use super::attacks::{attacks_from, attacks_to};

/// Enum to specify the type of moves to generate.
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum MoveGenerationType {
    All = 0,
    Quiet = 1,
    Captures = 2,
}

impl MoveGenerationType {
    pub const ALL_VALUE: u8 = 0;
    pub const QUIET_VALUE: u8 = 1;
    pub const CAPTURES_VALUE: u8 = 2;
}

impl From<u8> for MoveGenerationType {
    fn from(value: u8) -> Self {
        match value {
            MoveGenerationType::ALL_VALUE => MoveGenerationType::All,
            MoveGenerationType::QUIET_VALUE => MoveGenerationType::Quiet,
            MoveGenerationType::CAPTURES_VALUE => MoveGenerationType::Captures,
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
    list: &mut Vec<Move>,
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

fn generate_pawn_moves<const GENERATION_TYPE_VALUE: u8, const COLOR_VALUE: u8>(
    position: &Position,
    list: &mut Vec<Move>,
) {
    let generation_type: MoveGenerationType = GENERATION_TYPE_VALUE.into();
    let color: Color = COLOR_VALUE.into();

    let direction_factor = match color {
        Color::White => 1,
        Color::Black => -1,
    };
    let bb_rank_4: Bitboard = Bitboard::from(Rank::R4.relative_to_color(color));
    let bb_rank_8: Bitboard = Bitboard::from(Rank::R8.relative_to_color(color));
    let bb_file_a: Bitboard = Bitboard::from(File::A);
    let bb_file_h: Bitboard = Bitboard::from(File::H);
    let bb_occupied: Bitboard = position.bb_occupied();
    let bb_opponent: Bitboard = position.bb_color(color.opposite());
    let piece = Piece::new(COLOR_VALUE.into(), PieceType::Pawn);
    let bb_from = position.bb_piece(piece);

    if generation_type == MoveGenerationType::All || generation_type == MoveGenerationType::Quiet {
        // Single pawn push
        let mut bb_to = left_shift::<COLOR_VALUE>(bb_from, 8) & !bb_occupied & !bb_rank_8;
        collect_pawn_moves(
            bb_to,
            |sq| unsafe { sq.down_unchecked(direction_factor) },
            |from_sq, to_sq| {
                list.push(Move::new(from_sq, to_sq, piece));
            },
        );

        // Double pawn push
        bb_to = left_shift::<COLOR_VALUE>(bb_to, 8) & !bb_occupied & bb_rank_4;
        collect_pawn_moves(
            bb_to,
            |sq| unsafe { sq.down_unchecked(direction_factor * 2) },
            |from_sq, to_sq| {
                list.push(Move::new_two_square_pawn_push(from_sq, to_sq, piece));
            },
        );
    }

    if generation_type == MoveGenerationType::All || generation_type == MoveGenerationType::Captures {
        // Captures towards file A
        let mut bb_to = bb_from & !bb_file_a;
        bb_to = match color {
            Color::White => bb_to << 7,
            Color::Black => bb_to >> 9,
        };
        bb_to &= bb_opponent;
        collect_pawn_moves(
            bb_to & !bb_rank_8,
            |sq| unsafe { sq.down_unchecked(direction_factor).right_unchecked(1) },
            |from_sq, to_sq| {
                list.push(Move::new_capture(from_sq, to_sq, piece, unsafe { position[to_sq].unwrap_unchecked() }));
            },
        );
        collect_pawn_moves(
            bb_to & bb_rank_8,
            |sq| unsafe { sq.down_unchecked(direction_factor).right_unchecked(1) },
            |from_sq, to_sq| {
                add_captures_promotions::<COLOR_VALUE>(from_sq, to_sq, piece, position, list);
            },
        );

        // Captures towards file H
        let mut bb_to = bb_from & !bb_file_h;
        bb_to = match color {
            Color::White => bb_to << 9,
            Color::Black => bb_to >> 7,
        };
        bb_to &= bb_opponent;
        collect_pawn_moves(
            bb_to & !bb_rank_8,
            |sq| unsafe { sq.down_unchecked(direction_factor).left_unchecked(1) },
            |from_sq, to_sq| {
                list.push(Move::new_capture(from_sq, to_sq, piece, unsafe { position[to_sq].unwrap_unchecked() }));
            },
        );
        collect_pawn_moves(
            bb_to & bb_rank_8,
            |sq| unsafe { sq.down_unchecked(direction_factor).left_unchecked(1) },
            |from_sq, to_sq| {
                add_captures_promotions::<COLOR_VALUE>(from_sq, to_sq, piece, position, list);
            },
        );

        // Promotions
        bb_to = match color {
            Color::White => bb_from << 8,
            Color::Black => bb_from >> 8,
        } & !bb_occupied
            & bb_rank_8;
        collect_pawn_moves(
            bb_to,
            |sq| unsafe { sq.down_unchecked(direction_factor) },
            |from_sq, to_sq| {
                list.push(Move::new_promotion(from_sq, to_sq, piece, Piece::new(color, PieceType::Queen)));
                list.push(Move::new_promotion(from_sq, to_sq, piece, Piece::new(color, PieceType::Rook)));
                list.push(Move::new_promotion(from_sq, to_sq, piece, Piece::new(color, PieceType::Knight)));
                list.push(Move::new_promotion(from_sq, to_sq, piece, Piece::new(color, PieceType::Bishop)));
            },
        );

        // En passant
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

fn generate_piece_moves<const GENERATION_TYPE_VALUE: u8, const COLOR_VALUE: u8, const PIECE_TYPE_VALUE: u8>(
    position: &Position,
    list: &mut Vec<Move>,
) {
    let piece = Piece::new(COLOR_VALUE.into(), PIECE_TYPE_VALUE.into());
    let bb_from = position.bb_piece(piece);

    for from_sq in bb_from {
        let bb_to = attacks_from::<PIECE_TYPE_VALUE>(position.bb_occupied(), from_sq);
        let generation_type: MoveGenerationType = GENERATION_TYPE_VALUE.into();
        if generation_type == MoveGenerationType::All || generation_type == MoveGenerationType::Quiet {
            for to_sq in bb_to & !position.bb_occupied() {
                list.push(Move::new(from_sq, to_sq, piece));
            }
        }
        if generation_type == MoveGenerationType::All || generation_type == MoveGenerationType::Captures {
            for to_sq in bb_to & position.bb_color(Color::from(COLOR_VALUE).opposite()) {
                list.push(Move::new_capture(from_sq, to_sq, piece, unsafe { position[to_sq].unwrap_unchecked() }));
            }
        }
    }
}

fn generate_castlings<const COLOR_VALUE: u8, const SIDE_VALUE: u8>(position: &Position, list: &mut Vec<Move>) {
    let color = Color::from(COLOR_VALUE);
    let side = match SIDE_VALUE {
        CastlingSide::KINGSIDE_VALUE => CastlingSide::Kingside,
        CastlingSide::QUEENSIDE_VALUE => CastlingSide::Queenside,
        _ => panic!("Invalid castling side value"),
    };
    let castling_right = CastlingRight::new(color, side);

    if !position.castling_availability().contains(castling_right) {
        return;
    }

    let rank = Rank::R1.relative_to_color(color);

    let king = Piece::new(color, PieceType::King);
    let king_bb = position.bb_piece(king);
    let king_sq = unsafe { king_bb.lsb().unwrap_unchecked() }; // Safe because there is always a king.
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
    if king_sq == king_final_sq && rook_sq == rook_final_sq {
        return;
    }

    let king_travel_bb = Bitboard::between(king_sq, king_final_sq);
    let rook_travel_bb = Bitboard::between(rook_sq, rook_final_sq);
    let occupied_bb = position.bb_occupied() ^ (king_bb | rook_sq);

    // If any of the travel squares are occupied, it is not possible to castle.
    if (king_travel_bb | rook_travel_bb) & occupied_bb != Bitboard::EMPTY {
        return;
    }

    // If any of the travel squares are attacked, it is not possible to castle.
    for sq in king_travel_bb | king_bb | king_final_sq {
        let attacks = match color {
            Color::White => attacks_to::<{ Color::BLACK_VALUE }>(position, sq),
            Color::Black => attacks_to::<{ Color::WHITE_VALUE }>(position, sq),
        };
        if attacks != Bitboard::EMPTY {
            return;
        }
    }

    list.push(Move::new_castling(king_sq, king_final_sq, king, castling_right));
}

fn generate_moves_color<const GENERATION_TYPE_VALUE: u8, const COLOR_VALUE: u8>(
    position: &Position,
    list: &mut Vec<Move>,
) {
    generate_piece_moves::<GENERATION_TYPE_VALUE, COLOR_VALUE, { PieceType::KING_VALUE }>(position, list);
    generate_piece_moves::<GENERATION_TYPE_VALUE, COLOR_VALUE, { PieceType::KNIGHT_VALUE }>(position, list);
    generate_piece_moves::<GENERATION_TYPE_VALUE, COLOR_VALUE, { PieceType::ROOK_VALUE }>(position, list);
    generate_piece_moves::<GENERATION_TYPE_VALUE, COLOR_VALUE, { PieceType::BISHOP_VALUE }>(position, list);
    generate_piece_moves::<GENERATION_TYPE_VALUE, COLOR_VALUE, { PieceType::QUEEN_VALUE }>(position, list);
    generate_pawn_moves::<GENERATION_TYPE_VALUE, COLOR_VALUE>(position, list);
    if GENERATION_TYPE_VALUE == MoveGenerationType::ALL_VALUE
        || GENERATION_TYPE_VALUE == MoveGenerationType::QUIET_VALUE
    {
        generate_castlings::<COLOR_VALUE, { CastlingSide::KINGSIDE_VALUE }>(position, list);
        generate_castlings::<COLOR_VALUE, { CastlingSide::QUEENSIDE_VALUE }>(position, list);
    }
}

pub fn generate_moves<const GENERATION_TYPE_VALUE: u8>(position: &Position, list: &mut Vec<Move>) {
    match position.side_to_move() {
        Color::White => generate_moves_color::<GENERATION_TYPE_VALUE, { Color::WHITE_VALUE }>(position, list),
        Color::Black => generate_moves_color::<GENERATION_TYPE_VALUE, { Color::BLACK_VALUE }>(position, list),
    }
}
