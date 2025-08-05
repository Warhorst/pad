//! Contains methods to create [Line]s on a [Board].

use crate::board::Board;
use crate::direction::Direction;
use crate::p;
use crate::position::Position;
use crate::position_iter::PositionIter;

impl<T> Board<T> {
    /// Return an iterator over all rows in the board, from y = 0 to y = height (top to bottom).
    pub fn rows(&self) -> Rows<T> {
        Rows::new(self)
    }

    /// Return an iterator over all columns in the board, from x = 0 to x = width (left to right)
    pub fn columns(&self) -> Columns<T> {
        Columns::new(self)
    }

    /// Returns a [Line] which goes from the given start to the position in the given direction
    /// and length. Returns an error if
    /// - the start is out of bounds
    /// - the end is out of bounds
    /// - the direction is not cardinal
    ///
    /// Example:
    /// ```
    /// use pad::board::Board;
    /// use pad::direction::Direction;
    /// use pad::p;
    ///
    /// let board = Board::new(5, 5, || 0);
    /// let line = board.line_in_dir(p!(2, 2), Direction::XP, 2).unwrap();
    /// ```
    pub fn line_in_dir(
        &self,
        start: Position,
        dir: Direction,
        len: usize
    ) -> Result<Line<T>, LineError> {
        let end = if dir.is_cardinal() {
            start.position_in_direction(dir, len)
        } else {
            return Err(LineError::NotALine)
        };

        Line::new(self, start, end)
    }

    /// Creates a [Line] from the given start position to the boarder of the board in the given direction.
    /// Might return an error if:
    /// - the start position is not on the boarder
    /// - the direction is not cardinal
    ///
    /// Example:
    /// ```
    /// use pad::board::Board;
    /// use pad::direction::Direction;
    /// use pad::p;
    ///
    /// let board = Board::new(5, 5, || 0);
    /// let line = board.line_to_border(p!(2, 2), Direction::XP).unwrap();
    /// ```
    pub fn line_to_border(
        &self,
        start: Position,
        dir: Direction
    ) -> Result<Line<T>, LineError> {
        use Direction::*;
        let end = match dir {
            XP => p!(self.width - 1, start.y),
            XM => p!(0, start.y),
            YP => p!(start.x, self.height - 1),
            YM => p!(start.x, 0),
            _ => return Err(LineError::NotALine)
        };

        Line::new(self, start, end)
    }
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
    type Item = Line<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_y == self.board.height {
            None
        } else {
            let start = p!(0, self.current_y);
            let end = p!(self.board.width - 1, self.current_y);
            let next = Some(Line::new(self.board, start, end).unwrap());
            self.current_y += 1;
            next
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
    type Item = Line<'a, T>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.current_x == self.board.width {
            None
        } else {
            let start = p!(self.current_x, 0);
            let end = p!(self.current_x, self.board.height - 1);
            let next = Some(Line::new(self.board, start, end).unwrap());
            self.current_x += 1;
            next
        }
    }
}

/// A horizontal or vertical, even line on a [Board].
///
/// Can be used as an iterator to retrieve the values and their positions from the line
/// from its start to end
pub struct Line<'a, T> {
    board: &'a Board<T>,
    position_iter: PositionIter
}

impl<'a, T> Line<'a, T> {
    /// Create a new Line on the given board going from start to end.
    /// Returns an error if start and end are not an even line or the
    /// board does not contain both positions.
    fn new(
        board: &'a Board<T>,
        start: Position,
        end: Position
    ) -> Result<Self, LineError> {
        let diff = start - end;

        if !(diff.x == 0 || diff.y == 0) {
            return Err(LineError::NotALine)
        }

        if !board.pos_in_bounds(start) {
            return Err(LineError::StartNotOnBoard)
        }

        if !board.pos_in_bounds(end) {
            return Err(LineError::EndNotOnBoard)
        }

        Ok(Line {
            board,
            position_iter: start.iter_to(end)
        })
    }

    /// Returns where this line starts.
    pub fn start(&self) -> Position {
        self.position_iter.start()
    }

    /// Returns where this line ends.
    pub fn end(&self) -> Position {
        self.position_iter.end()
    }
}

impl <'a, T> Iterator for Line<'a, T> {
    type Item = (Position, &'a T);

    fn next(&mut self) -> Option<Self::Item> {
        match self.position_iter.next() {
            Some(pos) => Some((pos, self.board.get_tile(pos)?)),
            None => None
        }
    }
}

impl<'a, T> DoubleEndedIterator for Line<'a, T> {
    fn next_back(&mut self) -> Option<Self::Item> {
        match self.position_iter.next_back() {
            Some(pos) => Some((pos, self.board.get_tile(pos)?)),
            None => None
        }
    }
}

/// An error that might occur when attempting to create a line
/// on a board.
#[derive(Clone, Copy, Debug)]
pub enum LineError {
    NotALine,
    StartNotOnBoard,
    EndNotOnBoard
}

#[cfg(test)]
mod tests {
    use crate::board::Board;
    use crate::direction::Direction;
    use crate::direction::Direction::{XM, XMYM, XMYP, XP, XPYM, XPYP, YM, YP};
    use crate::p;
    use crate::position::Position;

    #[test]
    fn line_in_dir_works() {
        let board = Board::new(5, 5, || 0);
        let start = p!(2, 2);

        [
            // line of length 0 will only contain start
            (XP, 0, Some(start)),
            (XM, 0, Some(start)),
            (YP, 0, Some(start)),
            (YM, 0, Some(start)),
            // line which is in bounds
            (XP, 2, Some(p!(4, 2))),
            (XM, 2, Some(p!(0, 2))),
            (YP, 2, Some(p!(2, 4))),
            (YM, 2, Some(p!(2, 0))),
            // line which would be out of bounds
            (XP, 3, None),
            (XM, 3, None),
            (YP, 3, None),
            (YM, 3, None),
            // diagonal directions are forbidden
            (XPYP, 0, None),
            (XPYM, 0, None),
            (XMYP, 0, None),
            (XMYM, 0, None)
        ]
            .into_iter()
            .for_each(|(dir, len, expected_end): (Direction, usize, Option<Position>)| {
                let line_res = board.line_in_dir(start, dir, len);

                match expected_end {
                    Some(end) => {
                        assert!(line_res.is_ok());
                        assert_eq!(line_res.unwrap().end(), end, "{start:?} in direction {dir:?} with len {len}")
                    }
                    None => assert!(line_res.is_err())
                }
            });
    }

    #[test]
    fn line_to_boarder_works() {
        let board = Board::new(5, 5, || 0);
        let start = p!(2, 2);

        // start at border
        assert_eq!(board.line_to_border(p!(4, 2), XP).unwrap().end(), p!(4, 2));
        assert_eq!(board.line_to_border(p!(0, 2), XM).unwrap().end(), p!(0, 2));
        assert_eq!(board.line_to_border(p!(2, 4), YP).unwrap().end(), p!(2, 4));
        assert_eq!(board.line_to_border(p!(2, 0), YM).unwrap().end(), p!(2, 0));

        [
            // basic lines
            (XP, Some(p!(4, 2))),
            (XM, Some(p!(0, 2))),
            (YP, Some(p!(2, 4))),
            (YM, Some(p!(2, 0))),
            // diagonal directions are forbidden
            (XPYP, None),
            (XPYM, None),
            (XMYP, None),
            (XMYM, None)
        ]
            .into_iter()
            .for_each(|(dir, expected_end): (Direction, Option<Position>)| {
                let line_res = board.line_to_border(start, dir, );

                match expected_end {
                    Some(end) => {
                        assert!(line_res.is_ok());
                        assert_eq!(line_res.unwrap().end(), end, "{start:?} to border in direction {dir:?}")
                    }
                    None => assert!(line_res.is_err())
                }
            });
    }
}