pub mod line;
pub mod parse;
pub mod print;

use thiserror::Error;

use crate::bounds::Bounds;
use crate::p;
use crate::position::Position;
use crate::position_iter::PositionIter;
use crate::position_printer::{PositionPrinter, RenderStyle};
use crate::shape::Shape;
use std::collections::HashSet;
use std::fmt::{Debug, Display, Formatter};
use std::ops::{Index, IndexMut};

/// A 2D board of tiles where the tiles can be access by positions.
/// Orientation: The position (-inf, -inf) is top left and the position (+inf, +inf) is bottom right.
pub struct Board<T> {
    /// The width of the [Board]
    pub width: usize,
    /// The height of the [Board]
    pub height: usize,
    /// The [Bounds] of the [Board]
    pub bounds: Bounds,
    /// Every tile of the [Board]
    tiles: Vec<T>,
}

#[cfg(feature = "board_shape_macro")]
/// Quickly create a Board from a static string input.
/// Uses [indoc] internally to handle string formatting.
#[macro_export]
macro_rules! board {
    ($string:expr) => {{
        use indoc::indoc;
        use $crate::board::Board;
        Board::from(indoc! {$string})
    }};
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
        let tiles = p!(0, 0)
            .iter_to(p!(width - 1, height - 1))
            .map(|_| get_tile())
            .collect();

        Board {
            width,
            height,
            tiles,
            bounds: bounds_from_origin_and_dimension(p!(0, 0), width, height),
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
        positions: impl IntoIterator<Item = Position>,
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
                false => get_non_match_tile(),
            })
            .collect();

        Board {
            tiles,
            width,
            height,
            bounds: bounds_from_origin_and_dimension(p!(0, 0), width, height),
        }
    }

    /// The amount of tiles on the board. Basically width * height.
    pub fn len(&self) -> usize {
        self.tiles.len()
    }

    /// Returns true if the board has no tiles
    pub fn is_empty(&self) -> bool {
        self.len() == 0
    }

    /// Returns whether the given position is in the [Bounds] of this [Board].
    pub fn pos_in_bounds(
        &self,
        pos: Position,
    ) -> bool {
        self.bounds.contains_position(pos)
    }

    /// Returns whether the given position is on the border of this board.
    pub fn pos_on_border(
        &self,
        pos: Position,
    ) -> bool {
        pos.x == 0
            || pos.x == self.width as isize - 1
            || pos.y == 0
            || pos.y == self.height as isize - 1
    }

    /// Returns an iterator over all tiles and their positions on the board.
    pub fn tiles_and_positions(&self) -> impl Iterator<Item = (&T, Position)> {
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
    ///
    /// For an unchecked access, use the [Index] implementation.
    pub fn get_tile(
        &self,
        pos: impl Into<Position>,
    ) -> Option<&T> {
        let pos = pos.into();
        match self.pos_in_bounds(pos) {
            true => self.tiles.get(pos.y as usize * self.width + pos.x as usize),
            false => None,
        }
    }

    /// Return a mutable reference to the tile on the given position.
    /// Might return None if the position is out of bounds.
    ///
    /// For an unchecked access, use the [IndexMut] implementation.
    pub fn get_tile_mut(
        &mut self,
        pos: impl Into<Position>,
    ) -> Option<&mut T> {
        let pos = pos.into();
        match self.pos_in_bounds(pos) {
            true => self
                .tiles
                .get_mut(pos.y as usize * self.width + pos.x as usize),
            false => None,
        }
    }

    /// Returns all the tiles at the positions provided by the given iterator.
    pub fn get_tiles_at_positions(
        &self,
        positions: impl IntoIterator<Item = Position>,
    ) -> impl Iterator<Item = &T> {
        positions.into_iter().flat_map(|pos| self.get_tile(pos))
    }

    /// Set the tile at the given position to the given tile.
    /// Might fail if the position is out of bounds.
    pub fn set_tile(
        &mut self,
        pos: Position,
        tile: T,
    ) -> Result<(), BoardError> {
        match self.get_tile_mut(pos) {
            Some(t) => {
                *t = tile;
                Ok(())
            }
            None => Err(BoardError::PositionOutOfBounds(self.bounds, pos)),
        }
    }

    /// Return the tile at the given index. The index is the number of the tile
    /// when going from the top left position in the board (index 0) to the right most
    /// position, starting at the left most position of the next row until the position
    /// in the bottom right is reached. For example, see the following board:
    ///
    /// ```txt
    /// ....
    /// ..X.
    /// ....
    /// ```
    ///
    /// The position (2, 1) is set to 'X', while the other positions are '.'. The position
    /// with 'X' has the index 6 when going from top left to bottom right. See the board with indexes below the positions:
    ///
    /// ```txt
    /// . . . .
    /// 0 1 2 3
    /// . . X .
    /// 4 5 6 7
    /// . . . .
    /// 8 9 1011
    /// ```
    ///
    /// Will return [None] if the index is not part of the board.
    pub fn get_tile_at_index(
        &self,
        index: usize,
    ) -> Option<&T> {
        self.tiles.get(index)
    }

    /// Mutable version of [Board::get_tile_at_index]
    pub fn get_tile_mut_at_index(
        &mut self,
        index: usize,
    ) -> Option<&mut T> {
        self.tiles.get_mut(index)
    }

    /// Sets the tile at the given index to the given tile. See [Board::get_tile_at_index] for
    /// the indexing rules.
    /// Returns an [Err] if the index is not part of the board.
    pub fn set_tile_at_index(
        &mut self,
        index: usize,
        tile: T,
    ) -> Result<(), BoardError> {
        match self.get_tile_mut_at_index(index) {
            Some(t) => {
                *t = tile;
                Ok(())
            }
            None => Err(BoardError::IndexOutOfBounds(self.bounds, index)),
        }
    }

    /// Insert a row at the specified row index.
    ///
    /// Example:
    ///
    /// Given the following board:
    ///
    /// X..
    /// .X.
    /// ..X
    ///
    /// Calling ``board.insert_row(1, ['A', 'A', 'A'])``
    ///
    /// will result in
    ///
    /// X..
    /// AAA
    /// .X.
    /// ..X
    ///
    /// The call will return an error if
    /// - The index is out of bounds (if the index is the board height, the row will be the last row)
    /// - The number of new tiles is not equal to the width of the board
    pub fn insert_row(
        &mut self,
        row_index: usize,
        tiles: impl IntoIterator<Item = T>,
    ) -> Result<(), BoardError> {
        if !self.bounds.contains_y(row_index as isize) && row_index != self.height {
            return Err(BoardError::RowOutOfBounds(self.bounds, row_index));
        }

        let tiles = tiles.into_iter().collect::<Vec<_>>();

        if tiles.len() != self.width {
            return Err(BoardError::MismatchingRowTiles(self.width, tiles.len()));
        }

        for (x, t) in (0..self.width).zip(tiles.into_iter()) {
            let pos = p!(x, row_index);
            self.tiles.insert(pos.to_index(self.width), t);
        }

        self.height += 1;
        self.bounds = bounds_from_origin_and_dimension(p!(0, 0), self.width, self.height);

        Ok(())
    }

    /// Delete a row at the given row index.
    ///
    /// Example:
    ///
    /// Given the following board:
    ///
    /// X..
    /// .X.
    /// ..X
    ///
    /// Calling ``board.delete_row(1)``
    ///
    /// will result in
    ///
    /// X..
    /// ..X
    ///
    /// The call will return an error if the index is out of bounds.
    pub fn delete_row(
        &mut self,
        row_index: usize,
    ) -> Result<(), BoardError> {
        if !self.bounds.contains_y(row_index as isize) {
            return Err(BoardError::RowOutOfBounds(self.bounds, row_index));
        }

        for x in (0..self.width).rev() {
            let pos = p!(x, row_index);
            self.tiles.remove(pos.to_index(self.width));
        }

        self.height -= 1;
        self.bounds = bounds_from_origin_and_dimension(p!(0, 0), self.width, self.height);

        Ok(())
    }

    /// Insert a column at the specified column index.
    ///
    /// Example:
    ///
    /// Given the following board:
    ///
    /// X..
    /// .X.
    /// ..X
    ///
    /// Calling ``board.insert_column(1, ['A', 'A', 'A'])``
    ///
    /// will result in
    ///
    /// XA..
    /// .AX.
    /// .A.X
    ///
    /// The call will return an error if
    /// - The index is out of bounds (if the index is the board width, the column will be the last column)
    /// - The number of new tiles is not equal to the height of the board
    pub fn insert_column(
        &mut self,
        column_index: usize,
        tiles: impl IntoIterator<Item = T>,
    ) -> Result<(), BoardError> {
        if !self.bounds.contains_x(column_index as isize) && column_index != self.width {
            return Err(BoardError::ColumnOutOfBounds(self.bounds, column_index));
        }

        let tiles = tiles.into_iter().collect::<Vec<_>>();

        if tiles.len() != self.height {
            return Err(BoardError::MismatchingColumnTiles(self.height, tiles.len()));
        }

        for (y, t) in (0..self.height).zip(tiles.into_iter()) {
            let pos = p!(column_index, y);
            // width + 1, as the width of the board now changed
            self.tiles.insert(pos.to_index(self.width + 1), t);
        }

        self.width += 1;
        self.bounds = bounds_from_origin_and_dimension(p!(0, 0), self.width, self.height);

        Ok(())
    }

    /// Delete a column at the given column index.
    ///
    /// Example:
    ///
    /// Given the following board:
    ///
    /// X..
    /// .X.
    /// ..X
    ///
    /// Calling ``board.delete_column(1)``
    ///
    /// will result in
    ///
    /// X.
    /// ..
    /// .X
    ///
    /// The call will return an error if the index is out of bounds.
    pub fn delete_column(
        &mut self,
        column_index: usize,
    ) -> Result<(), BoardError> {
        if !self.bounds.contains_x(column_index as isize) {
            return Err(BoardError::ColumnOutOfBounds(self.bounds, column_index));
        }

        for y in (0..self.height).rev() {
            let pos = p!(column_index, y);
            self.tiles.remove(pos.to_index(self.width));
        }

        self.width -= 1;
        self.bounds = bounds_from_origin_and_dimension(p!(0, 0), self.width, self.height);

        Ok(())
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
        shape_tile: T,
    ) -> bool {
        // currently very slow, as it iterates over every tile and checks if the shape is there :)
        self.positions()
            .into_iter()
            .filter(|pos| self.pos_in_bounds(*pos + p!(shape.width - 1, shape.height - 1)))
            .any(|pos| {
                shape
                    .positions()
                    .into_iter()
                    .all(|p| match self.get_tile(pos + p) {
                        Some(t) => t == &shape_tile,
                        _ => unreachable!(),
                    })
            })
    }

    /// Returns the positions which contain the given tile
    pub fn get_positions_of<'a>(
        &'a self,
        tile: &'a T,
    ) -> impl Iterator<Item = Position> + 'a {
        self.tiles_and_positions()
            .filter(move |(t, _)| *t == tile)
            .map(|(_, p)| p)
    }
}

impl<T, Idx> Index<Idx> for Board<T>
where
    Idx: Into<Position>,
{
    type Output = T;

    fn index(
        &self,
        index: Idx,
    ) -> &Self::Output {
        self.get_tile(index)
            .expect("The position must be in bounds.")
    }
}

impl<T, Idx> IndexMut<Idx> for Board<T>
where
    Idx: Into<Position>,
{
    fn index_mut(
        &mut self,
        index: Idx,
    ) -> &mut Self::Output {
        self.get_tile_mut(index)
            .expect("The position must be in bounds.")
    }
}

impl<T> Display for Board<T>
where
    T: Into<char> + Clone,
{
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
    ) -> std::fmt::Result {
        let board_string = PositionPrinter::new()
            .draw_axis(false)
            .render_style(RenderStyle::Screen)
            .to_string(&self.positions().collect(), |pos| {
                self.get_tile(pos).unwrap().clone().into()
            });

        write!(f, "{board_string}")
    }
}

// todo when https://doc.rust-lang.org/beta/unstable-book/language-features/specialization.html is stabilized,
//  add an implementation for types whose reference implements Into<char>, like this:
//  impl <T> Debug for Board<T> where for<'a> &'a T: Into<char>
//  The same is true for the Display implementation

impl<T> Clone for Board<T>
where
    T: Clone,
{
    fn clone(&self) -> Self {
        Self {
            width: self.width,
            height: self.height,
            bounds: self.bounds,
            tiles: self.tiles.clone(),
        }
    }
}

impl<T> Debug for Board<T>
where
    T: Into<char> + Clone,
{
    fn fmt(
        &self,
        f: &mut Formatter<'_>,
    ) -> std::fmt::Result {
        writeln!(f, "Width: {}, Height: {}", self.width, self.height)?;
        write!(f, "{self}")
    }
}

impl<T> PartialEq for Board<T>
where
    T: PartialEq,
{
    fn eq(
        &self,
        other: &Self,
    ) -> bool {
        self.width == other.width && self.height == other.height && self.tiles == other.tiles
    }
}

pub(in crate::board) const fn bounds_from_origin_and_dimension(
    origin: Position,
    width: usize,
    height: usize,
) -> Bounds {
    Bounds {
        min_x: origin.x,
        min_y: origin.y,
        max_x: origin.x + width as isize - 1,
        max_y: origin.y + height as isize - 1,
    }
}

/// Erros which might occur when performing [Board] operations.
#[derive(Debug, Eq, Error, PartialEq)]
pub enum BoardError {
    #[error("Position {1:?} is out of bounds ({0:?})")]
    PositionOutOfBounds(Bounds, Position),
    #[error("Index {1:?} is out of bounds ({0:?})")]
    IndexOutOfBounds(Bounds, usize),
    #[error("Row {1:?} is out of bounds ({0:?})")]
    RowOutOfBounds(Bounds, usize),
    #[error("Column {1:?} is out of bounds ({0:?})")]
    ColumnOutOfBounds(Bounds, usize),
    #[error("{0} tiles are required to insert the row, but {1:?} were provided")]
    MismatchingRowTiles(usize, usize),
    #[error("{0} tiles are required to insert the column, but {1:?} were provided")]
    MismatchingColumnTiles(usize, usize),
}

#[cfg(test)]
mod tests {
    use crate::{
        board::{Board, BoardError},
        p,
    };

    #[test]
    fn set_tile_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        assert_eq!(board.set_tile(p!(0, 1), 'A'), Ok(()));
        assert_eq!(
            board.set_tile(p!(3, 3), 'B'),
            Err(BoardError::PositionOutOfBounds(board.bounds, p!(3, 3)))
        );
        assert_eq!(
            board,
            Board::from("....\nA.X.\n...."),
            "the board must be unchanged"
        );
    }

    #[test]
    fn get_tile_works() {
        let board_string = "....\n..X.\n....";
        let board = Board::<char>::from(board_string);

        assert_eq!(board.get_tile(p!(0, 0)), Some(&'.'));
        assert_eq!(board.get_tile(p!(2, 1)), Some(&'X'));
        assert_eq!(board.get_tile(p!(4, 4)), None);
    }

    #[test]
    fn get_tile_impl_index_works() {
        let board_string = "....\n..X.\n....";
        let board = Board::<char>::from(board_string);

        assert_eq!(&board[p!(0, 0)], &'.');
        assert_eq!(&board[p!(2, 1)], &'X');
    }

    #[test]
    #[should_panic]
    fn get_tile_impl_index_out_of_bounds_works() {
        let board_string = "....\n..X.\n....";
        let board = Board::<char>::from(board_string);
        let _ = &board[p!(4, 4)];
    }

    #[test]
    fn get_tile_mut_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        assert_eq!(board.get_tile_mut(p!(0, 0)), Some(&mut '.'));
        assert_eq!(board.get_tile_mut(p!(2, 1)), Some(&mut 'X'));
        assert_eq!(board.get_tile_mut(p!(4, 4)), None);
    }

    #[test]
    fn get_tile_mut_impl_index_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        assert_eq!(&mut board[p!(0, 0)], &mut '.');
        assert_eq!(&mut board[p!(2, 1)], &mut 'X');
    }

    #[test]
    #[should_panic]
    fn get_tile_mut_impl_index_out_of_bounds_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);
        let _ = &mut board[p!(4, 4)];
    }

    #[test]
    fn get_tile_at_index_works() {
        let board_string = "....\n..X.\n....";
        let board = Board::<char>::from(board_string);

        assert_eq!(board.get_tile_at_index(0), Some(&'.'));
        assert_eq!(board.get_tile_at_index(6), Some(&'X'));
        assert_eq!(board.get_tile_at_index(12), None);
    }

    #[test]
    fn get_tile_mut_at_index_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        assert_eq!(board.get_tile_mut_at_index(0), Some(&mut '.'));
        assert_eq!(board.get_tile_mut_at_index(6), Some(&mut 'X'));
        assert_eq!(board.get_tile_mut_at_index(12), None);
    }

    #[test]
    fn set_tile_at_index_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        assert_eq!(board.set_tile_at_index(0, 'A'), Ok(()));
        assert_eq!(board, Board::from("A...\n..X.\n...."));
        assert_eq!(board.set_tile_at_index(6, 'B'), Ok(()));
        assert_eq!(board, Board::from("A...\n..B.\n...."));
        assert_eq!(
            board.set_tile_at_index(12, 'C'),
            Err(BoardError::IndexOutOfBounds(board.bounds, 12))
        );
        assert_eq!(
            board,
            Board::from("A...\n..B.\n...."),
            "the board must be unchanged"
        );
    }

    #[test]
    fn insert_row_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        // Insert at the start of the board
        let mut start = board.clone();
        let res = start.insert_row(0, ['A', 'A', 'A', 'A']);
        assert_eq!(res, Ok(()));
        assert_eq!(start, Board::<char>::from("AAAA\n....\n..X.\n...."));

        // Insert in the middle of the board
        let mut middle = board.clone();
        let res = middle.insert_row(1, ['A', 'A', 'A', 'A']);
        assert_eq!(res, Ok(()));
        assert_eq!(middle, Board::<char>::from("....\nAAAA\n..X.\n...."));

        // Insert at the end of the board
        let mut end = board.clone();
        let res = end.insert_row(end.height, ['A', 'A', 'A', 'A']);
        assert_eq!(res, Ok(()));
        assert_eq!(end, Board::<char>::from("....\n..X.\n....\nAAAA"));

        // Potential errors
        assert_eq!(
            board.insert_row(5, ['A', 'A', 'A', 'A']),
            Err(BoardError::RowOutOfBounds(board.bounds, 5))
        );
        assert_eq!(
            board.insert_row(1, ['A', 'A', 'A', 'A', 'A']),
            Err(BoardError::MismatchingRowTiles(4, 5))
        );
    }

    #[test]
    fn delete_row_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        let res = board.delete_row(1);
        assert_eq!(res, Ok(()));
        assert_eq!(board, Board::<char>::from("....\n...."));

        assert_eq!(
            board.delete_row(5),
            Err(BoardError::RowOutOfBounds(board.bounds, 5))
        );
    }

    #[test]
    fn insert_column_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        // Insert at the start of the board
        let mut start = board.clone();
        let res = start.insert_column(0, ['A', 'A', 'A']);
        assert_eq!(res, Ok(()));
        assert_eq!(start, Board::<char>::from("A....\nA..X.\nA...."));

        // Insert in the middle of the board
        let mut middle = board.clone();
        let res = middle.insert_column(1, ['A', 'A', 'A']);
        assert_eq!(res, Ok(()));
        assert_eq!(middle, Board::<char>::from(".A...\n.A.X.\n.A..."));

        // Insert at the end of the board
        let mut end = board.clone();
        let res = end.insert_column(end.width, ['A', 'A', 'A']);
        assert_eq!(res, Ok(()));
        assert_eq!(end, Board::<char>::from("....A\n..X.A\n....A"));

        // Potential errors
        assert_eq!(
            board.insert_column(5, ['A', 'A', 'A']),
            Err(BoardError::ColumnOutOfBounds(board.bounds, 5))
        );
        assert_eq!(
            board.insert_column(1, ['A', 'A', 'A', 'A']),
            Err(BoardError::MismatchingColumnTiles(3, 4))
        );
    }

    #[test]
    fn delete_column_works() {
        let board_string = "....\n..X.\n....";
        let mut board = Board::<char>::from(board_string);

        let res = board.delete_column(2);
        assert_eq!(res, Ok(()));
        assert_eq!(board, Board::<char>::from("...\n...\n..."));

        assert_eq!(
            board.delete_column(5),
            Err(BoardError::ColumnOutOfBounds(board.bounds, 5))
        );
    }

    #[cfg(feature = "board_shape_macro")]
    #[test]
    fn board_macro_works() {
        use super::Board;

        #[derive(Copy, Clone)]
        enum Tile {
            Filled,
            Free,
        }

        use Tile::*;

        impl From<char> for Tile {
            fn from(value: char) -> Self {
                match value {
                    '#' => Filled,
                    '.' => Free,
                    _ => unreachable!(),
                }
            }
        }

        impl Into<char> for &Tile {
            fn into(self) -> char {
                match self {
                    Filled => '#',
                    Free => '.',
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
