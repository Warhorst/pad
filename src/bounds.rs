use crate::position::Position;
use std::cmp::{max, min};

/// Describes the value bounds a position can have.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq)]
pub struct Bounds {
    pub min_x: isize,
    pub min_y: isize,
    pub max_x: isize,
    pub max_y: isize,
}

impl Bounds {
    pub fn new(
        min_x: isize,
        min_y: isize,
        max_x: isize,
        max_y: isize,
    ) -> Self {
        Bounds {
            min_x,
            min_y,
            max_x,
            max_y,
        }
    }

    /// Create bounds by determining them from the given positions
    pub fn from_positions(positions: impl IntoIterator<Item = Position>) -> Self {
        positions
            .into_iter()
            .fold(Bounds::default(), |mut bounds, item| {
                bounds.min_x = min(bounds.min_x, item.x);
                bounds.min_y = min(bounds.min_y, item.y);
                bounds.max_x = max(bounds.max_x, item.x);
                bounds.max_y = max(bounds.max_y, item.y);
                bounds
            })
    }

    /// Tells if this Bounds contains the given [Position]
    pub fn contains_position(
        &self,
        pos: Position,
    ) -> bool {
        self.contains_x(pos.x) && self.contains_y(pos.y)
    }

    /// Tells if the given x coordinate is in bounds.
    pub fn contains_x(
        &self,
        x: isize,
    ) -> bool {
        self.min_x <= x && self.max_x >= x
    }

    /// Tells if the given y coordinate is in bounds.
    pub fn contains_y(
        &self,
        y: isize,
    ) -> bool {
        self.min_y <= y && self.max_y >= y
    }
}
