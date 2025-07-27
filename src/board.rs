use std::collections::HashSet;
use crate::{p, Position, PositionIter, PositionPrinter};
use crate::shape::Shape;

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
/// Uses indoc internally to handle string formatting.
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

impl<T: From<char>> From<&str> for Board<T> {
    /// Create the board from a text input, where each character represents
    /// a tile.
    /// * `input` - The text input which represents the board. Expected to be a multiline string
    ///             where every line has the same amount of characters
    fn from(input: &str) -> Self {
        let width = width_from_input(input);
        let height = height_from_input(input);

        let tiles = input
            .lines()
            .flat_map(|line| line.chars().map(|c| T::from(c)))
            .collect();

        Board {
            width,
            height,
            tiles,
        }
    }
}

impl<T: From<char>> Board<T> {
    /// Create a board and all special tiles from the given text input
    ///
    /// * `input` - The text input which represents the board. Expected to be a multiline string
    ///             where every line has the same amount of characters
    /// * `special_map` - A mapping closure which might map a given char and position to a special
    ///                   Tile and a Tile default. The special tile is part of the input, but not
    ///                   of the actual board, so it gets extracted. If the char at the current
    ///                   position could be mapped to a special tile, the special case gets stored
    ///                   and the spot on the board gets replaced by the default tile, specified by
    ///                   the mapper.
    pub fn board_and_specials_from_str<S>(
        input: &str,
        special_map: impl Fn(char, Position) -> Option<(S, T)>,
    ) -> (Self, Vec<S>) {
        let width = width_from_input(input);
        let height = height_from_input(input);

        let mut tiles = Vec::with_capacity(width * height);
        let mut specials = Vec::new();

        input
            .lines()
            .enumerate()
            .for_each(|(y, line)| line.chars().enumerate().for_each(|(x, c)| match special_map(c, p!(x, y)) {
                Some((special, tile)) => {
                    specials.push(special);
                    tiles.push(tile)
                }
                None => tiles.push(T::from(c))
            }));

        (
            Board {
                width,
                height,
                tiles,
            },
            specials
        )
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

    /// Same as From<&str>, but the tile type does not implement From<char>, so the provided
    /// mapper is used to parse the chars to tiles.
    /// * `input` - The text input which represents the board. Expected to be a multiline string
    ///             where every line has the same amount of characters
    /// * `map` - Mapper which converts a char to at tile
    pub fn from_str_using_mapping(
        input: &str,
        map: impl Fn(char) -> T,
    ) -> Self {
        let width = width_from_input(input);
        let height = height_from_input(input);

        let tiles = input
            .lines()
            .flat_map(|line| line.chars().map(|c| map(c)))
            .collect();

        Board {
            width,
            height,
            tiles,
        }
    }

    /// Create a board from given positions where every position is set to one tile
    /// and every absent one is set to another one.
    /// * `positions` - The iterator yielding all positions which will be set to the result of
    ///                 get_match_tile. Positions returned which are out of bounds are ignored.
    /// * `width` - The width of the board.
    /// * `height` - The width of the board.
    /// * `get_match_tile` - Returns a tile which is set to a position returned by the positions iterator.
    /// * `get_non_match_tile` - Return a tile which is set to a position which was not returned by
    ///                          the positions iterator, but is still part of the board and must be set.
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

    /// Return an iterator over all rows in the board, from y = 0 to y = height (top to bottom).
    pub fn rows(&self) -> Rows<T> {
        Rows::new(self)
    }

    /// Return an iterator over all columns in the board, from x = 0 to x = width (left to right)
    pub fn columns(&self) -> Columns<T> {
        Columns::new(self)
    }

    /// Print every position in the board, using the given mapper to represent the tile as a char
    /// used in the printed output.
    pub fn print_with_mapping(&self, map: impl Fn(&T) -> char) {
        PositionPrinter::new()
            .draw_axis(false)
            .y_is_top(true)
            .print_with_mapping(self.positions(), |pos| match self.get_tile(pos) {
                Some(t) => map(t),
                None => ' '
            })
    }
}

impl<T> Board<T>
where
    T: Eq + PartialEq,
{
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
            .into_iter()
            .filter(move |(t, _)| *t == tile)
            .map(|(_, p)| p)
    }

    /// Print all the tiles where the current tile equals the given tile
    pub fn print_occurrences_of_tile(&self, tile: T) {
        PositionPrinter::new()
            .draw_axis(false)
            .y_is_top(true)
            .print(self.tiles_and_positions()
                .into_iter()
                .filter_map(|(t, p)| match tile == *t {
                    true => Some(p),
                    false => None
                })
            )
    }
}

impl<T> Board<T>
where
    T: Into<char> + Copy,
{
    /// Print the board to the console, using the Into<char> implementation of the tile
    /// to represent it.
    pub fn print(&self) {
        self.print_with_mapping(|t| (*t).into())
    }
}

fn width_from_input(input: &str) -> usize {
    input
        .lines()
        .next()
        .expect("The input must contain at least one line")
        .chars()
        .count()
}

fn height_from_input(input: &str) -> usize {
    input.lines().count()
}

pub struct Rows<'a, T> {
    board: &'a Board<T>,
    current_y: usize,
}

impl<'a, T> Rows<'a, T> {
    fn new(board: &'a Board<T>) -> Self {
        Rows {
            board,
            current_y: 0,
        }
    }
}

impl<'a, T> Iterator for Rows<'a, T> {
    type Item = Row<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_y == self.board.height {
            None
        } else {
            let next = Some(Row::new(self.board, self.current_y));
            self.current_y += 1;
            next
        }
    }
}

pub struct Row<'a, T> {
    board: &'a Board<T>,
    position_iter: PositionIter,
}

impl<'a, T> Row<'a, T> {
    fn new(board: &'a Board<T>, current_y: usize) -> Self {
        let position_iter = p!(0, current_y).iter_to(p!(board.width - 1, current_y));

        Row {
            board,
            position_iter,
        }
    }
}

impl<'a, T> Iterator for Row<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.position_iter.next() {
            Some(pos) => self.board.get_tile(pos),
            None => None
        }
    }
}

impl<'a, T> DoubleEndedIterator for Row<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.position_iter.next_back() {
            Some(pos) => self.board.get_tile(pos),
            None => None
        }
    }
}

pub struct Columns<'a, T> {
    board: &'a Board<T>,
    current_x: usize,
}

impl<'a, T> Columns<'a, T> {
    fn new(board: &'a Board<T>) -> Self {
        Columns {
            board,
            current_x: 0,
        }
    }
}

impl<'a, T> Iterator for Columns<'a, T> {
    type Item = Column<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_x == self.board.width {
            None
        } else {
            let next = Some(Column::new(self.board, self.current_x));
            self.current_x += 1;
            next
        }
    }
}

pub struct Column<'a, T> {
    board: &'a Board<T>,
    position_iter: PositionIter,
}

impl<'a, T> Column<'a, T> {
    fn new(board: &'a Board<T>, current_x: usize) -> Self {
        Column {
            board,
            position_iter: p!(current_x, 0).iter_to(p!(current_x, board.height - 1)),
        }
    }
}

impl<'a, T> Iterator for Column<'a, T> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        match self.position_iter.next() {
            Some(pos) => self.board.get_tile(pos),
            None => None,
        }
    }
}

impl<'a, T> DoubleEndedIterator for Column<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.position_iter.next_back() {
            Some(pos) => self.board.get_tile(pos),
            None => None,
        }
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