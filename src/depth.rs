use std::ops::{Add, AddAssign, Sub, SubAssign};

/// Represents the search depth in the engine. The depth is stored as a signed 16-bit integer, but it can be fractional,
/// earch ply if depth can be separated into 16 parts.
#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct Depth(i16);

impl Depth {
    const FRACTIONAL_PLY: i16 = 16; // Each ply is divided into 16 parts (sixteenths)

    pub const ZERO: Depth = Depth(0);
    pub const ONE_PLY: Depth = Depth(16);

    /// Creates a new `Depth` instance integer representing the depth in plies.
    pub const fn from_plies(ply: i16) -> Self {
        Depth(ply * Self::FRACTIONAL_PLY)
    }

    /// Creates a new `Depth` instance from a fractional depth value expressed in sixteenths of a ply.
    pub fn from_sixteenths(sixteenths: i16) -> Self {
        Depth(sixteenths)
    }

    /// Returns the depth in plies as a signed integer. Partial depth are rounded away from zero.
    pub fn as_plies(self) -> i16 {
        if self.0 >= 0 {
            (self.0 + Self::FRACTIONAL_PLY - 1) / Self::FRACTIONAL_PLY // Round up for positive depths
        } else {
            (self.0 - Self::FRACTIONAL_PLY - 1) / Self::FRACTIONAL_PLY // Round down for negative depths
        }
    }
}

impl Add<Depth> for Depth {
    type Output = Self;

    /// Adds two `Depth` instances together, returning a new `Depth` instance.
    fn add(self, other: Depth) -> Self::Output {
        Depth(self.0 + other.0)
    }
}

impl AddAssign<Depth> for Depth {
    /// Adds another `Depth` instance to the current instance, modifying it in place.
    fn add_assign(&mut self, other: Depth) {
        self.0 += other.0;
    }
}

impl Sub<Depth> for Depth {
    type Output = Self;

    /// Subtracts one `Depth` instance from another, returning a new `Depth` instance.
    fn sub(self, other: Depth) -> Self::Output {
        Depth(self.0 - other.0)
    }
}

impl SubAssign<Depth> for Depth {
    /// Subtracts another `Depth` instance from the current instance, modifying it in place.
    fn sub_assign(&mut self, other: Depth) {
        self.0 -= other.0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_depth_from_plies() {
        let depth = Depth::from_plies(3);
        assert_eq!(depth.0, 48); // 3 * 16
    }

    #[test]
    fn test_depth_from_sixteenths() {
        let depth = Depth::from_sixteenths(32);
        assert_eq!(depth.0, 32);
    }

    #[test]
    fn test_depth_as_plies() {
        assert_eq!(Depth::from_sixteenths(-17).as_plies(), -2);
        assert_eq!(Depth::from_sixteenths(-16).as_plies(), -1);
        assert_eq!(Depth::from_sixteenths(-15).as_plies(), -1);
        assert_eq!(Depth::from_sixteenths(-1).as_plies(), -1);

        assert_eq!(Depth::from_sixteenths(0).as_plies(), 0);

        assert_eq!(Depth::from_sixteenths(1).as_plies(), 1);
        assert_eq!(Depth::from_sixteenths(15).as_plies(), 1);
        assert_eq!(Depth::from_sixteenths(16).as_plies(), 1);
        assert_eq!(Depth::from_sixteenths(17).as_plies(), 2);
    }
}
