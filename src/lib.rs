use std::ops::Add;
use Direction::*;

/// A 2D-point with signed x and y values. Used for boards and similar stuff.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash)]
pub struct Position {
    pub x: isize,
    pub y: isize,
}

impl Position {
    pub fn new(x: isize, y: isize) -> Self {
        Position { x, y }
    }

    /// Return only neighbours in cardinal directions which are in the given bounds.
    pub fn neighbours_in_bounds(&self, start: Position, end: Position) -> impl IntoIterator<Item=Neighbour> {
        self.neighbours()
            .into_iter()
            .filter(move |neighbour| neighbour.position.x >= start.x
                && neighbour.position.y >= start.y
                && neighbour.position.x <= end.x
                && neighbour.position.y <= end.y
            )
    }

    /// Return the neighbours in all four cardinal directions (up, down, left, right)
    pub fn neighbours(&self) -> [Neighbour; 4] {
        [Up, Down, Left, Right].map(|dir| Neighbour::new(self.position_in_direction(dir), dir))
    }

    /// Return the position next to the current one in the given direction
    ///
    /// Interpretation of directions:
    /// Right -> x gets higher
    /// Left -> x gets lower
    /// Up -> y gets higher
    /// Down -> y gets lower
    pub fn position_in_direction(&self, dir: Direction) -> Position {
        match dir {
            Left => *self + (-1, 0),
            Right => *self + (1, 0),
            Up => *self + (0, 1),
            Down => *self + (0, -1)
        }
    }

    /// Creates an iterator from all positions between self and the given end.
    ///
    /// The two positions form a square. For example (0,0) and (4,3) form the following shape
    ///
    /// XXXX
    /// XXXX
    /// XXXX
    ///
    /// The iterator takes every position from left to right and bottom to top. So the first element will be (0,0),
    /// followed by (1,0) until (3,0). Then (0,1), (1,1) and so on until (3,3) was returned
    pub fn iter_to(&self, other: Position) -> PositionIter {
        PositionIter::new(*self, other)
    }

    /// Create a Position from an index. This value might be the index for a 2D-matrix which is stored
    /// in a 1D-array. Requires the width of the matrix.
    pub fn from_index(index: usize, width: usize) -> Self {
        Position::from((
            index % width,
            index / width,
        ))
    }

    /// Creates an index for this position. This value might be the index for a 2D-matrix which is stored
    /// in a 1D-array. Requires the width of the matrix.
    pub fn to_index(&self, width: usize) -> usize {
        self.y as usize * width + self.x as usize
    }
}

/// Represents an adjacent position in a specific direction.
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct Neighbour {
    pub position: Position,
    pub direction: Direction,
}

impl Neighbour {
    pub fn new(position: Position, direction: Direction) -> Self {
        Self { position, direction }
    }
}

impl Add for Position {
    type Output = Position;

    fn add(self, other: Self) -> Self::Output {
        Position::new(
            self.x + other.x,
            self.y + other.y,
        )
    }
}

impl Add<(isize, isize)> for Position {
    type Output = Position;

    fn add(self, (x, y): (isize, isize)) -> Self::Output {
        Position::new(
            self.x + x,
            self.y + y,
        )
    }
}

impl From<(isize, isize)> for Position {
    fn from((x, y): (isize, isize)) -> Self {
        Position::new(x, y)
    }
}

impl From<(usize, usize)> for Position {
    fn from((x, y): (usize, usize)) -> Self {
        Position::new(x as isize, y as isize)
    }
}

pub struct PositionIter {
    current_x: isize,
    current_y: isize,
    start: Position,
    end: Position,
}

impl PositionIter {
    fn new(start: Position, end: Position) -> Self {
        if start.x > end.x || start.y > end.y {
            panic!("start must be less or equal to end")
        }

        PositionIter
        {
            current_x: start.x,
            current_y: start.y,
            start,
            end,
        }
    }

    fn is_finished(&self) -> bool {
        self.current_x > self.end.x || self.current_y > self.end.y
    }

    fn increment(&mut self) {
        self.current_x += 1;

        if self.current_x > self.end.x && self.current_y < self.end.y {
            self.current_x = self.start.x;
            self.current_y += 1;
        }
    }
}

impl Iterator for PositionIter {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_finished() {
            return None;
        }

        let current = Position::new(self.current_x, self.current_y);
        self.increment();
        Some(current)
    }
}

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}

#[cfg(test)]
mod tests {
    use crate::{Direction, Neighbour, Position};
    use crate::Direction::*;

    #[test]
    fn add_position_works() {
        let pos = Position::new(1, 2);
        let other = Position::new(2, 3);
        assert_eq!(Position::new(3, 5), pos + other);
    }

    #[test]
    fn add_tuple_works() {
        let pos = Position::new(1, 2);
        let other = (2, 3);
        assert_eq!(Position::new(3, 5), pos + other);
    }

    #[test]
    fn get_position_in_direction_works() {
        let pos = Position::new(0, 0);

        [
            (Up, Position::new(0, 1)),
            (Down, Position::new(0, -1)),
            (Left, Position::new(-1, 0)),
            (Right, Position::new(1, 0)),
        ]
            .into_iter()
            .for_each(|(dir, expected)| assert_eq!(pos.position_in_direction(dir), expected))
    }

    #[test]
    fn neighbours_works() {
        let pos = Position::new(0, 0);

        let neighbours = pos.neighbours();

        [
            Neighbour::new(Position::new(0, 1), Up),
            Neighbour::new(Position::new(0, -1), Down),
            Neighbour::new(Position::new(-1, 0), Left),
            Neighbour::new(Position::new(1, 0), Right),
        ].into_iter().for_each(|expected| assert!(neighbours.contains(&expected)))
    }

    #[test]
    fn neighbours_in_bounds_work() {
        let pos = Position::new(1, 1);
        let neighbours = pos.neighbours_in_bounds(Position::new(0, 0), Position::new(2, 2)).into_iter().collect::<Vec<_>>();

        assert_eq!(neighbours.len(), 4);
        assert!(neighbours.contains(&neigh(2, 1, Right)));
        assert!(neighbours.contains(&neigh(0, 1, Left)));
        assert!(neighbours.contains(&neigh(1, 2, Up)));
        assert!(neighbours.contains(&neigh(1, 0, Down)));

        let pos = Position::new(0, 0);
        let neighbours = pos.neighbours_in_bounds(Position::new(0, 0), Position::new(1, 1)).into_iter().collect::<Vec<_>>();

        assert_eq!(neighbours.len(), 2);
        assert!(neighbours.contains(&neigh(1, 0, Right)));
        assert!(neighbours.contains(&neigh(0, 1, Up)));

        let pos = Position::new(2, 2);
        let neighbours = pos.neighbours_in_bounds(Position::new(0, 0), Position::new(2, 2)).into_iter().collect::<Vec<_>>();

        assert_eq!(neighbours.len(), 2);
        assert!(neighbours.contains(&neigh(1, 2, Left)));
        assert!(neighbours.contains(&neigh(2, 1, Down)));

        let pos = Position::new(1, 0);
        let neighbours = pos.neighbours_in_bounds(Position::new(0, 0), Position::new(2, 2)).into_iter().collect::<Vec<_>>();

        assert_eq!(neighbours.len(), 3);
        assert!(neighbours.contains(&neigh(0, 0, Left)));
        assert!(neighbours.contains(&neigh(2, 0, Right)));
        assert!(neighbours.contains(&neigh(1, 1, Up)));
    }

    #[test]
    fn position_iter_works() {
        [
            (
                Position::new(0, 0),
                Position::new(0, 0),
                vec![Position::new(0, 0)]
            ),
            (
                Position::new(-1, -1),
                Position::new(1, 1),
                [(-1isize, -1isize), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)].into_iter().map(From::from).collect()
            ),
            (
                Position::new(0, 0),
                Position::new(2, 0),
                [(0isize, 0isize), (1, 0), (2, 0)].into_iter().map(From::from).collect()
            ),
            (
                Position::new(0, 0),
                Position::new(0, 2),
                [(0isize, 0isize), (0, 1), (0, 2)].into_iter().map(From::from).collect()
            )
        ]
            .into_iter()
            .for_each(|(start, end, expected)| assert_eq!(start.iter_to(end).collect::<Vec<_>>(), expected))
    }

    #[test]
    fn from_index_works() {
        let width = 10;
        [
            (22, Position::new(2, 2)),
            (3, Position::new(3, 0)),
            (30, Position::new(0, 3))
        ]
            .into_iter()
            .for_each(|(index, expected)| assert_eq!(Position::from_index(index, width), expected))
    }

    #[test]
    fn to_index_works() {
        let width = 10;

        [
            (Position::new(2, 2), 22),
            (Position::new(3, 0), 3),
            (Position::new(0, 3), 30),
        ]
            .into_iter()
            .for_each(|(pos, expected)| assert_eq!(pos.to_index(width), expected))
    }

    fn neigh(x: isize, y: isize, dir: Direction) -> Neighbour {
        Neighbour::new(Position::new(x, y), dir)
    }
}