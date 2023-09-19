use std::ops::Add;
use bevy_reflect::Reflect;
use Direction::*;

/// A 2D-point with signed x and y values. Used for boards and similar stuff.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash, Reflect)]
pub struct Position {
    pub x: isize,
    pub y: isize,
}

impl Position {
    pub fn new(x: isize, y: isize) -> Self {
        Position { x, y }
    }

    /// Return all neighbours of the given position
    pub fn neighbours(&self) -> [Position; 8] {
        Direction::all_directions().map(|dir| self.neighbour_in_direction(dir))
    }

    /// Return the neighbours in all four cardinal directions (up, down, left, right)
    pub fn cardinal_neighbours(&self) -> [Position; 4] {
        Direction::cardinal_directions().map(|dir| self.neighbour_in_direction(dir))
    }

    /// Return all neighbours of the given position with the direction they are relative to the origin
    pub fn neighbours_with_directions(&self) -> [(Position, Direction); 8] {
        Direction::all_directions().map(|dir| (self.neighbour_in_direction(dir), dir))
    }

    /// Return the neighbours in all four cardinal directions (up, down, left, right)
    /// with the direction they are relative to the origin
    pub fn cardinal_neighbours_with_directions(&self) -> [(Position, Direction); 4] {
        Direction::cardinal_directions().map(|dir| (self.neighbour_in_direction(dir), dir))
    }

    /// Return the position in the given direction next to this position
    pub fn neighbour_in_direction(&self, dir: Direction) -> Position {
        self.position_in_direction(dir, 1)
    }

    /// Return the position in the given direction in the given distance
    pub fn position_in_direction(&self, dir: Direction, distance: usize) -> Position {
        let distance = distance as isize;

        match dir {
            XP => *self + (distance, 0),
            XM => *self + (-distance, 0),
            YP => *self + (0, distance),
            YM => *self + (0, -distance),
            XPYP => *self + (distance, distance),
            XPYM => *self + (distance, -distance),
            XMYP => *self + (-distance, distance),
            XMYM => *self + (-distance, -distance),
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

/// Provides a quick way to create a Position from a given x and y value.
/// x and y can be anything that can be converted to an isize and don't need to be of
/// the same type.
#[macro_export]
macro_rules! p {
    ($x:expr, $y:expr) => {
        Position::new($x as isize, $y as isize)
    };
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

/// A direction along the x-axis, y-axis or both.
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
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
    fn all_directions() -> [Direction; 8] {
        [XP, XM, YP, YM, XPYP, XPYM, XMYP, XMYM]
    }

    fn cardinal_directions() -> [Direction; 4] {
        [XP, XM, YP, YM]
    }
}

#[cfg(test)]
mod tests {
    use crate::Position;
    use crate::Direction::*;

    #[test]
    fn add_position_works() {
        let pos = p!(1, 2);
        let other = p!(2, 3);
        assert_eq!(p!(3, 5), pos + other);
    }

    #[test]
    fn add_tuple_works() {
        let pos = p!(1, 2);
        let other = (2, 3);
        assert_eq!(p!(3, 5), pos + other);
    }

    #[test]
    fn neighbours_works() {
        let pos = p!(0, 0);

        let neighbours = pos.neighbours();

        [
            p!(1, 0),
            p!(-1, 0),
            p!(0, 1),
            p!(0, -1),
            p!(1, 1),
            p!(-1, 1),
            p!(1, -1),
            p!(-1, -1),
        ].into_iter().for_each(|expected| assert!(neighbours.contains(&expected)))
    }

    #[test]
    fn cardinal_neighbours_works() {
        let pos = p!(0, 0);

        let neighbours = pos.cardinal_neighbours();

        [
            p!(0, 1),
            p!(0, -1),
            p!(-1, 0),
            p!(1, 0),
        ].into_iter().for_each(|expected| assert!(neighbours.contains(&expected)))
    }

    #[test]
    fn neighbours_with_directions_works() {
        let pos = p!(0, 0);

        let neighbours = pos.neighbours_with_directions();

        [
            (p!(1, 0), XP),
            (p!(-1, 0), XM),
            (p!(0, 1), YP),
            (p!(0, -1), YM),
            (p!(1, 1), XPYP),
            (p!(-1, 1), XMYP),
            (p!(1, -1), XPYM),
            (p!(-1, -1), XMYM),
        ].into_iter().for_each(|expected| assert!(neighbours.contains(&expected)))
    }

    #[test]
    fn cardinal_neighbours_with_directions_works() {
        let pos = p!(0, 0);

        let neighbours = pos.cardinal_neighbours_with_directions();

        [
            (p!(1, 0), XP),
            (p!(-1, 0), XM),
            (p!(0, 1), YP),
            (p!(0, -1), YM),
        ].into_iter().for_each(|expected| assert!(neighbours.contains(&expected)))
    }

    #[test]
    fn position_in_direction_works() {
        let pos = p!(0, 0);
        let distance = 5;

        [
            (YP, p!(0, distance)),
            (YM, p!(0, -distance)),
            (XM, p!(-distance, 0)),
            (XP, p!(distance, 0)),
            (XPYP, p!(distance, distance)),
            (XPYM, p!(distance, -distance)),
            (XMYP, p!(-distance, distance)),
            (XMYM, p!(-distance, -distance)),
        ]
            .into_iter()
            .for_each(|(dir, expected)| assert_eq!(pos.position_in_direction(dir, distance as usize), expected))
    }

    #[test]
    fn neighbour_in_direction_works() {
        let pos = p!(0, 0);

        [
            (YP, p!(0, 1)),
            (YM, p!(0, -1)),
            (XM, p!(-1, 0)),
            (XP, p!(1, 0)),
            (XPYP, p!(1, 1)),
            (XPYM, p!(1, -1)),
            (XMYP, p!(-1, 1)),
            (XMYM, p!(-1, -1)),
        ]
            .into_iter()
            .for_each(|(dir, expected)| assert_eq!(pos.neighbour_in_direction(dir), expected))
    }

    #[test]
    fn position_iter_works() {
        [
            (
                p!(0, 0),
                p!(0, 0),
                vec![p!(0, 0)]
            ),
            (
                p!(-1, -1),
                p!(1, 1),
                [(-1isize, -1isize), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)].into_iter().map(From::from).collect()
            ),
            (
                p!(0, 0),
                p!(2, 0),
                [(0isize, 0isize), (1, 0), (2, 0)].into_iter().map(From::from).collect()
            ),
            (
                p!(0, 0),
                p!(0, 2),
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
            (22, p!(2, 2)),
            (3, p!(3, 0)),
            (30, p!(0, 3))
        ]
            .into_iter()
            .for_each(|(index, expected)| assert_eq!(Position::from_index(index, width), expected))
    }

    #[test]
    fn to_index_works() {
        let width = 10;

        [
            (p!(2, 2), 22),
            (p!(3, 0), 3),
            (p!(0, 3), 30),
        ]
            .into_iter()
            .for_each(|(pos, expected)| assert_eq!(pos.to_index(width), expected))
    }
}