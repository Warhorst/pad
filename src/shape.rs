use std::collections::HashSet;
use crate::{p, Position, PositionPrinter};

/// A shape is a collection of positions which form some figure, image, etc.
/// The positions are all relative to the origin of the shape, which is top left
pub struct Shape {
    pub width: usize,
    pub height: usize,
    positions: HashSet<Position>
}

#[cfg(feature = "board_shape_macro")]
/// Quickly create a Shape from a static string input.
/// Uses indoc internally to handle string formatting.
#[macro_export]
macro_rules! shape {
    ($string:expr) => {
        {
            use $crate::shape::Shape;
            use indoc::indoc;
            Shape::from(indoc!($string))
        }
    }
}

impl From<&str> for Shape {
    /// Create the shape from a string where the chars with letter 'X' are all considered part of
    /// the shape and every other char isn't.
    fn from(input: &str) -> Self {
        let width = input
            .lines()
            .next()
            .expect("the string should not be empty")
            .chars().count();
        let height = input.lines().count();
        let positions = input
            .lines()
            .enumerate()
            .flat_map(|(y, line)| line
                .chars()
                .enumerate()
                .filter(|(_, c)| *c == 'X')
                .map(move |(x, _)| p!(x, y))
            )
            .collect();

        Shape {
            width,
            height,
            positions
        }
    }
}

impl Shape {
    /// Return all positions in this shape.
    pub fn positions(&self) -> impl IntoIterator<Item=Position> + '_ {
        self.positions.iter().copied()
    }

    /// Print all positions in this shape to the terminal.
    pub fn print(&self) {
        PositionPrinter::new()
            .draw_axis(true)
            .print(self.positions.iter().copied())
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "board_shape_macro")]
    #[test]
    fn shape_macro_works() {
        let shape = shape!{"
            XXXXXXXX
            XX    XX
            XX    XX
            XXXXXXXX
        "};

        shape.print()
    }
}