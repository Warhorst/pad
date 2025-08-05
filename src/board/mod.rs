pub mod parse;
pub mod line;
pub mod print;

use crate::p;
use crate::position::Position;
use crate::position_iter::PositionIter;
use crate::position_printer::{PositionPrinter, RenderStyle};
use crate::shape::Shape;
use itertools::Itertools;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};

/// A 2D board of tiles where the tiles can be access by positions.
/// Orientation: The position (-inf, -inf) is top left and the position (+inf, +inf) is bottom right.
pub struct Board<T> {
    /// The width of the board
    pub width: usize,
    /// The height of the board
    pub height: usize,
    /// every tile of the board
    tiles: Vec<T>,
}

#[cfg(feature = "board_shape_macro")]
/// Quickly create a Board from a static string input.
/// Uses [indoc] internally to handle string formatting.
#[macro_export]
macro_rules! board {
    ($string:expr) => {
        {
            use $crate::board::Board;
            use indoc::indoc;
            Board::from(indoc! {$string})
        }
    }
}

impl<T> Board<T> {
    /// Create a new board filled with the tile created by the given closure and the given dimension.
    /// * `width` - The width of the board
    /// * `height` - The height of the board
    /// * `get_tile` - The closure which creates the tile inserted in every position of the board
    pub fn new(
        width: usize,
        height: usize,
        get_tile: impl Fn() -> T,
    ) -> Self {
        let tiles = p!(0, 0).iter_to(p!(width - 1, height - 1)).map(|_| get_tile()).collect();

        Board {
            width,
            height,
            tiles,
        }
    }

    /// Create a board from given positions where every position is set to one tile
    /// and every absent one is set to another one.
    /// * `positions` - The iterator yielding all positions which will be set to the result of
    ///   get_match_tile. Positions returned which are out of bounds are ignored.
    /// * `width` - The width of the board.
    /// * `height` - The width of the board.
    /// * `get_match_tile` - Returns a tile which is set to a position returned by the positions iterator.
    /// * `get_non_match_tile` - Return a tile which is set to a position which was not returned by
    ///   the positions iterator, but is still part of the board and must be set.
    pub fn from_positions_and_bounds(
        positions: impl IntoIterator<Item=Position>,
        width: usize,
        height: usize,
        get_match_tile: impl Fn() -> T,
        get_non_match_tile: impl Fn() -> T,
    ) -> Self {
        let positions = positions.into_iter().collect::<HashSet<_>>();

        let tiles = p!(0, 0)
            .iter_to(p!(width - 1, height - 1))
            .map(|pos| match positions.contains(&pos) {
                true => get_match_tile(),
                false => get_non_match_tile()
            })
            .collect();

        Board {
            tiles,
            width,
            height,
        }
    }

    /// The amount of tiles on the board. Basically width * height.
    pub fn len(&self) -> usize {
        self.tiles.len()
    }

    /// Returns whether the given position is in the bounds of this board.
    pub fn pos_in_bounds(&self, pos: Position) -> bool {
        (0..self.width).contains(&(pos.x as usize)) && (0..self.height).contains(&(pos.y as usize))
    }

    /// Returns whether the given position is on the border of this board.
    pub fn pos_on_border(&self, pos: Position) -> bool {
        pos.x == 0 || pos.x == self.width as isize - 1 || pos.y == 0 || pos.y == self.height as isize - 1
    }

    /// Returns an iterator over all tiles and their positions on the board.
    pub fn tiles_and_positions(&self) -> impl Iterator<Item=(&T, Position)> {
        self.positions()
            .into_iter()
            .map(|pos| (self.get_tile(pos).expect("the tile must exist"), pos))
    }

    /// Return an iterator over all positions on the board.
    pub fn positions(&self) -> PositionIter {
        p!(0, 0).iter_to(p!(self.width - 1, self.height - 1))
    }

    /// Return a reference to the tile on the given position.
    /// Might return None if the position is out of bounds.
    pub fn get_tile(&self, pos: Position) -> Option<&T> {
        match self.pos_in_bounds(pos) {
            true => self.tiles.get(pos.y as usize * self.width + pos.x as usize),
            false => None
        }
    }

    /// Return a mutable reference to the tile on the given position.
    /// Might return None if the position is out of bounds.
    pub fn get_tile_mut(&mut self, pos: Position) -> Option<&mut T> {
        match self.pos_in_bounds(pos) {
            true => self.tiles.get_mut(pos.y as usize * self.width + pos.x as usize),
            false => None
        }
    }

    /// Returns all the tiles at the positions provided by the given iterator.
    pub fn get_tiles_at_positions(
        &self,
        positions: impl IntoIterator<Item=Position>,
    ) -> impl Iterator<Item=&T> {
        positions
            .into_iter()
            .flat_map(|pos| self.get_tile(pos))
    }

    /// Set the tile at the given position to the given tile.
    /// Might fail if the position is out of bounds.
    pub fn set_tile(
        &mut self,
        pos: Position,
        tile: T,
    ) -> Result<(), ()> {
        match self.get_tile_mut(pos) {
            Some(t) => {
                *t = tile;
                Ok(())
            }
            None => Err(())
        }
    }
}

impl<T> Board<T> where T: Eq + PartialEq {
    /// Tells if the board contains the given shape.
    /// * `shape` - The shape which is checked if it is on the board
    /// * `shape_tile` - The tile which might form the shape
    pub fn contains_shape(
        &self,
        shape: &Shape,
        shape_tile: T
    ) -> bool {
        // currently very slow, as it iterates over every tile and checks if the shape is there :)
        self
            .positions()
            .into_iter()
            .filter(|pos| self.pos_in_bounds(*pos + p!(shape.width - 1, shape.height - 1)))
            .any(|pos| shape
                .positions()
                .into_iter()
                .all(|p| match self.get_tile(pos + p) {
                    Some(t) => t == &shape_tile,
                    _ => unreachable!()
                })
            )
    }

    /// Returns the positions which contain the given tile
    pub fn get_positions_of<'a>(&'a self, tile: &'a T) -> impl Iterator<Item=Position> + 'a {
        self.tiles_and_positions()
            .filter(move |(t, _)| *t == tile)
            .map(|(_, p)| p)
    }
}

impl <T> Display for Board<T> where for<'a> &'a T: Into<char> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let board_string = PositionPrinter::new()
            .draw_axis(false)
            .render_style(RenderStyle::Screen)
            .to_string(&self.positions().collect(), |pos| self.get_tile(pos).unwrap().into());

        write!(f, "{board_string}")
    }
}

impl <T> Debug for Board<T> where for<'a> &'a T: Into<char> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "Width: {}", self.width)?;
        writeln!(f, "Height: {}", self.height)?;
        let positions_and_tiles_string = self
            .tiles_and_positions()
            .map(|(t, pos)| format!("({}, {}) : {}", pos.x, pos.y, t.into()))
            .join("\n");
        write!(f, "{positions_and_tiles_string}")
    }
}

impl<T> PartialEq for Board<T> where T: PartialEq {
    fn eq(&self, other: &Self) -> bool {
        self.width == other.width && self.height == other.height && self.tiles == other.tiles
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "board_shape_macro")]
    #[test]
    fn board_macro_works() {
        use super::Board;

        #[derive(Copy, Clone)]
        enum Tile {
            Filled,
            Free
        }

        use Tile::*;

        impl From<char> for Tile {
            fn from(value: char) -> Self {
                match value {
                    '#' => Filled,
                    '.' => Free,
                    _ => unreachable!()
                }
            }
        }

        impl Into<char> for Tile {
            fn into(self) -> char {
                match self {
                    Filled => '#',
                    Free => '.'
                }
            }
        }

        let board: Board<Tile> = board! {"
            ........
            ..####..
            ..####..
            ........
        "};

        board.print()
    }
}