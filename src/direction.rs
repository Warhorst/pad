#[cfg(feature = "bevy")]
use bevy_reflect::Reflect;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use Direction::*;

/// A direction along the x-axis, y-axis or both.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
#[cfg_attr(feature = "bevy", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
pub enum Direction {
    /// x plus
    XP,
    /// x minus
    XM,
    /// y plus
    YP,
    /// y minus
    YM,
    /// x plus y plus
    XPYP,
    /// x plus y minus
    XPYM,
    /// x minus y plus
    XMYP,
    /// x minus y minus
    XMYM,
}

impl Direction {
    /// Get all possible directions in the order they are defined in Direction.
    pub fn dirs() -> [Direction; 8] {
        [XP, XM, YP, YM, XPYP, XPYM, XMYP, XMYM]
    }

    /// Get all cardinal directions in the order they are defined in Direction.
    pub fn cardinal_dirs() -> [Direction; 4] {
        [XP, XM, YP, YM]
    }
   
    /// Returns true if this direction is a cardinal direction (increments either x or y), else false.
    pub fn is_cardinal(&self) -> bool {
        matches!(self, XP | XM | YP | YM)
    }

    /// Return the opposite direction of this direction
    pub fn opposite(&self) -> Direction {
        match self {
            XP => XM,
            XM => XP,
            YP => YM,
            YM => YP,
            XPYP => XMYM,
            XPYM => XMYP,
            XMYP => XPYM,
            XMYM => XPYP
        }
    }

    /// Get the direction vector of this direction as an isize tuple.
    pub fn get_direction_vec(&self) -> (isize, isize) {
        match self {
            XP => (1, 0),
            XM => (-1, 0),
            YP => (0, 1),
            YM => (0, -1),
            XPYP => (1, 1),
            XPYM => (1, -1),
            XMYP => (-1, 1),
            XMYM => (-1, -1)
        }
    }

    /// Get the direction vector of this direction as a f32 tuple.
    pub fn get_direction_vec_f32(&self) -> (f32, f32) {
        match self {
            XP => (1.0, 0.0),
            XM => (-1.0, 0.0),
            YP => (0.0, 1.0),
            YM => (0.0, -1.0),
            XPYP => (1.0, 1.0),
            XPYM => (1.0, -1.0),
            XMYP => (-1.0, 1.0),
            XMYM => (-1.0, -1.0)
        }
    }
}
