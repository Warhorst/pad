pub mod board;
pub mod shape;
pub mod position;
pub mod direction;
pub mod position_iter;
pub mod position_printer;
pub mod bounds;

/// Provides a quick way to create a Position from a given x and y value.
/// x and y can be anything that can be converted to an isize and don't need to be of
/// the same type.
///
/// Example:
/// ```
/// let position = p!(12, -34);
/// ```
#[macro_export]
macro_rules! p {
    ($x:expr, $y:expr) => {
        {
            use $crate::position::Position;
            Position::new($x as isize, $y as isize)
        }
    };
}