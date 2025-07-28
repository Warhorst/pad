use std::cmp::{max, min};
use crate::position::Position;

/// Describes the value bounds a position can have.
#[derive(Copy, Clone, Default)]
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
    pub fn from_positions(positions: impl IntoIterator<Item=Position>) -> Self {
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

    /// Tells if this Bounds contains the given position
    pub fn contains_position(&self, pos: Position) -> bool {
        self.min_x <= pos.x
            && self.max_x >= pos.x
            && self.min_y <= pos.y
            && self.max_y >= pos.y
    }
}
