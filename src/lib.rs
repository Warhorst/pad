use std::collections::HashSet;
use std::ops::{Add, Sub};

use Direction::*;
#[cfg(feature = "bevy")]
use bevy_math::*;
#[cfg(feature = "bevy")]
use bevy_reflect::Reflect;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};

/// A 2D-point with signed x and y values. Used for boards and similar stuff.
#[derive(Copy, Clone, Debug, Default, Eq, PartialEq, Hash, Ord, PartialOrd)]
#[cfg_attr(feature = "bevy", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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
    /// ```
    /// // XXXX
    /// // XXXX
    /// // XXXX
    /// ```
    ///
    /// The iterator takes every position from left to right and bottom to top. So the first element will be (0,0),
    /// followed by (1,0) until (3,0). Then (0,1), (1,1) and so on until (4,3) was returned
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

    /// Tells if two positions are neighboured in any way.
    /// A position is not neighboured with itself.
    pub fn is_neighbour_with(&self, other: &Position) -> bool {
        if self == other {
            return false;
        }

        let diff = *self - *other;
        let range = -1..=1;

        range.contains(&diff.x) && range.contains(&diff.y)
    }

    /// Tells if two positions are neighboured in cardinal directions
    /// A position is not neighboured with itself.
    pub fn is_cardinal_neighbour_with(&self, other: &Position) -> bool {
        let diff = *self - *other;

        diff.x.abs() == 1 && diff.y == 0 || diff.x == 0 && diff.y.abs() == 1
    }

    /// Tells if two positions are neighboured in diagonal directions
    /// A position is not neighboured with itself.
    pub fn is_diagonal_neighbour_with(&self, other: &Position) -> bool {
        let diff = *self - *other;

        diff.x.abs() == 1 && diff.y.abs() == 1
    }

    /// Return the direction the given neighbour is relative to this position.
    /// None is returned if the positions are not neighboured or equals.
    pub fn get_direction_to_neighbour(&self, other: &Position) -> Option<Direction> {
        match (other.x - self.x, other.y - self.y) {
            (1, 0) => Some(XP),
            (0, 1) => Some(YP),
            (-1, 0) => Some(XM),
            (0, -1) => Some(YM),
            (1, 1) => Some(XPYP),
            (1, -1) => Some(XPYM),
            (-1, 1) => Some(XMYP),
            (-1, -1) => Some(XMYM),
            _ => None
        }
    }

    /// Calculates the manhattan distance between this position and another one (https://en.wikipedia.org/wiki/Taxicab_geometry)
    pub fn manhattan_distance(&self, other: &Position) -> f32 {
        if self == other {
            return 0.0
        }

        self.x.abs_diff(other.x) as f32 + self.y.abs_diff(other.y) as f32
    }

    /// Calculates the euclidean distance between this position and another one (https://en.wikipedia.org/wiki/Euclidean_distance)
    pub fn euclidean_distance(&self, other: &Position) -> f32 {
        if self == other {
            return 0.0
        }

        (self.x.abs_diff(other.x).pow(2) as f32 + self.y.abs_diff(other.y).pow(2) as f32).sqrt()
    }

    /// Calculates the distance from this position to another one.
    ///
    /// Explanation: If the positions correlate to tiles on a board and you can take
    /// steps from one position to a neighboured one, the distance is the minimum amount of
    /// steps you need to make to reach the other position.
    pub fn distance_to(&self, other: &Position) -> usize {
        usize::max(
            (self.x - other.x).abs() as usize,
            (self.y - other.y).abs() as usize
        )
    }

    /// Converts Vec2 coordinates to a Position.
    ///
    /// Use case: The game world is a big plane, which is logically divided in tiles. Every tile has
    /// the same dimension, and every tile has a unique position. The goal is to get the position
    /// the given coordinates are in.
    #[cfg(feature = "bevy")]
    pub fn from_vec2(coordinates: Vec2, dimension: Vec2) -> Self {
        let mut x_div = coordinates.x / dimension.x;
        let mut y_div = coordinates.y / dimension.y;

        if x_div < 0.0 {
            x_div = x_div.floor()
        }

        if y_div < 0.0 {
            y_div = y_div.floor()
        }

        Position::new(
            x_div as isize,
            y_div as isize,
        )
    }

    /// Same as from_vec2, but for Vec3 coordinates instead.
    #[cfg(feature = "bevy")]
    pub fn from_vec3(coordinates: Vec3, dimension: Vec2) -> Self {
        Position::from_vec2(coordinates.xy(), dimension)
    }

    /// Converts this position to Vec2 coordinates.
    ///
    /// Use case: The game world is a big plane, which is logically divided in tiles. Every tile has
    /// the same dimension, and every tile has a unique position. The goal is to get the corner coordinates
    /// for the tile at this position.
    #[cfg(feature = "bevy")]
    pub fn to_vec2(&self, dimension: Vec2) -> Vec2 {
        Vec2::new(
            self.x as f32 * dimension.x,
            self.y as f32 * dimension.y,
        )
    }

    /// Same as to_vec2, but for Vec3 coordinates. As the Vec3 also needs a z coordinate, it is provided as
    /// a parameter.
    #[cfg(feature = "bevy")]
    pub fn to_vec3(&self, dimension: Vec2, z: f32) -> Vec3 {
        Vec3::from((self.to_vec2(dimension), z))
    }

    /// Returns an iterator over all positions in the line between self and goal,
    /// including self and goal. Uses Bresenham's line algorithm.
    pub fn line_to(&self, goal: Position) -> impl IntoIterator<Item=Position> {
        // todo slow due to allocations, refactor to iterator

        // stolen from wikipedia

        let mut positions = vec![];

        let (dx, dy) = (goal.x - self.x, goal.y - self.y);
        let mut current = *self;

        positions.push(current);

        let mut error = dx / 2;

        while current.x < goal.x {
            current.x += 1;
            error -= dy;

            if error < 0 {
                current.y += 1;
                error += dx;
            }

            positions.push(current)
        }

        positions
    }

    /// Return an iterator over all positions in the circle around self with the given radius.
    /// Uses an adapted version of Bresenham's line algorithm.
    pub fn circle(&self, radius: usize) -> impl IntoIterator<Item=Position> {
        // todo slow due to allocations, refactor to iterator

        // stolen from wikipedia

        // conversion to isize, so we don't have to cast everywhere
        let radius = radius as isize;

        let mut positions = HashSet::new();

        let mut f = 1 - radius;
        let mut ddf_x = 0isize;
        let mut ddf_y = -2 * radius;
        let mut x = 0;
        let mut y = radius;

        positions.insert(p!(self.x, self.y + radius));
        positions.insert(p!(self.x, self.y - radius));
        positions.insert(p!(self.x + radius, self.y));
        positions.insert(p!(self.x - radius, self.y));

        while x < y {
            if f >= 0 {
                y -= 1;
                ddf_y += 2;
                f += ddf_y;
            }

            x += 1;
            ddf_x += 2;
            f += ddf_x + 1;

            positions.insert(p!(self.x + x, self.y + y));
            positions.insert(p!(self.x - x, self.y + y));
            positions.insert(p!(self.x + x, self.y - y));
            positions.insert(p!(self.x - x, self.y - y));
            positions.insert(p!(self.x + y, self.y + x));
            positions.insert(p!(self.x - y, self.y + x));
            positions.insert(p!(self.x + y, self.y - x));
            positions.insert(p!(self.x - y, self.y - x));
        }

        positions
    }

    /// Return an iterator over all positions in a filled circle around this position
    /// with the given radius.
    pub fn circle_filled(&self, radius: usize) -> impl IntoIterator<Item=Position> {
        // todo slow due to allocations, refactor to iterator

        // source http://fredericgoset.ovh/mathematiques/courbes/en/filled_circle.html
        // I absolutely don't understand this, but it does work
        let radius = radius as isize;
        let mut positions = vec![];

        let mut x = 0;
        let mut y = radius;
        let mut m = 5 - 4 * radius;

        while x <= y {
            positions.extend(p!(self.x - x, self.y - y).line_to(p!(self.x + x, self.y - y)));
            positions.extend(p!(self.x - y, self.y - x).line_to(p!(self.x + y, self.y - x)));
            positions.extend(p!(self.x - y, self.y + x).line_to(p!(self.x + y, self.y + x)));
            positions.extend(p!(self.x - x, self.y + y).line_to(p!(self.x + x, self.y + y)));

            if m > 0 {
                y -= 1;
                m -= 8 * y;
            }

            x += 1;
            m += 8 * x + 4;
        }

        positions
    }

    /// Prints a simple representation of the given positions to the terminal.
    /// Signs are omitted, top/right goes to positive infinity, down/left to negative infinity.
    pub fn print_positions(positions: impl IntoIterator<Item=Position>) {
        let positions = positions.into_iter().collect::<Vec<_>>();

        let min_x = positions.iter().map(|p| p.x).min().expect("at least one position must be given");
        let min_y = positions.iter().map(|p| p.y).min().expect("at least one position must be given");
        let max_x = positions.iter().map(|p| p.x).max().expect("at least one position must be given");
        let max_y = positions.iter().map(|p| p.y).max().expect("at least one position must be given");

        let x_axis = (min_x..=max_x)
            .into_iter()
            .fold(String::new(), |mut acc, item| {
                acc += format!(" {}", item.abs()).as_str();
                acc
            });

        println!(" {x_axis}");

        for y in (min_y..=max_y).rev() {
            print!("{} ", y.abs());
            for x in min_x..=max_x {
                if positions.contains(&p!(x, y)) {
                    print!("X ");
                } else {
                    print!("  ");
                }
            }
            println!("{} ", y.abs());
        }

        println!(" {x_axis}");
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

impl Sub for Position {
    type Output = Position;

    fn sub(self, other: Self) -> Self::Output {
        Position::new(
            self.x - other.x,
            self.y - other.y,
        )
    }
}

impl Sub<(isize, isize)> for Position {
    type Output = Position;

    fn sub(self, (x, y): (isize, isize)) -> Self::Output {
        Position::new(
            self.x - x,
            self.y - y,
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
        {
            use $crate::Position;
            Position::new($x as isize, $y as isize)
        }
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
#[cfg_attr(feature = "bevy", derive(Reflect))]
#[cfg_attr(feature = "serde", derive(Serialize, Deserialize))]
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

    /// Return the opposite direction of this direction
    pub fn opposite(&self) -> Direction {
        match self {
            XP => XM,
            XM => XP,
            YP => YM,
            YM => YP,
            XPYP => XMYM,
            XPYM => XMYP,
            XMYP => XPYM,
            XMYM => XPYP
        }
    }

    /// Get the direction vector of this direction as an isize tuple.
    pub fn get_direction_vec(&self) -> (isize, isize) {
        match self {
            XP => (1, 0),
            XM => (-1, 0),
            YP => (0, 1),
            YM => (0, -1),
            XPYP => (1, 1),
            XPYM => (1, -1),
            XMYP => (-1, 1),
            XMYM => (-1, -1)
        }
    }

    /// Get the direction vector of this direction as a f32 tuple.
    pub fn get_direction_vec_f32(&self) -> (f32, f32) {
        match self {
            XP => (1.0, 0.0),
            XM => (-1.0, 0.0),
            YP => (0.0, 1.0),
            YM => (0.0, -1.0),
            XPYP => (1.0, 1.0),
            XPYM => (1.0, -1.0),
            XMYP => (-1.0, 1.0),
            XMYM => (-1.0, -1.0)
        }
    }
}

#[cfg(test)]
mod tests {
    #[cfg(feature = "bevy")]
    use bevy_math::Vec2;
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
    fn sup_position_works() {
        let pos = p!(1, 2);
        let other = p!(2, 3);
        assert_eq!(p!(-1, -1), pos - other);
    }

    #[test]
    fn sub_tuple_works() {
        let pos = p!(1, 2);
        let other = (2, 3);
        assert_eq!(p!(-1, -1), pos - other);
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

    #[test]
    fn is_neighbour_with_work() {
        let pos = p!(1, 1);

        [
            (p!(1, 1), false),
            (p!(1, 2), true),
            (p!(1, 0), true),
            (p!(2, 1), true),
            (p!(0, 1), true),
            (p!(0, 0), true),
            (p!(2, 2), true),
            (p!(2, 0), true),
            (p!(0, 2), true),
            (p!(3, 1), false),
            (p!(1, 3), false),
        ]
            .into_iter()
            .for_each(|(other, expectation)| assert_eq!(
                pos.is_neighbour_with(&other),
                expectation,
                "{:?}.is_neighbour_with({:?}) should be {}",
                pos,
                other,
                expectation
            ))
    }

    #[test]
    fn is_cardinal_neighbour_with_works() {
        let pos = p!(1, 1);

        [
            (p!(1, 1), false),
            (p!(1, 2), true),
            (p!(1, 0), true),
            (p!(2, 1), true),
            (p!(0, 1), true),
            (p!(0, 0), false),
            (p!(2, 2), false),
            (p!(2, 0), false),
            (p!(0, 2), false),
            (p!(3, 1), false),
            (p!(1, 3), false),
        ]
            .into_iter()
            .for_each(|(other, expectation)| assert_eq!(
                pos.is_cardinal_neighbour_with(&other),
                expectation,
                "{:?}.is_cardinal_neighbour_with({:?}) should be {}",
                pos,
                other,
                expectation
            ))
    }

    #[test]
    fn is_diagonal_neighbour_with_works() {
        let pos = p!(1, 1);

        [
            (p!(1, 1), false),
            (p!(1, 2), false),
            (p!(1, 0), false),
            (p!(2, 1), false),
            (p!(0, 1), false),
            (p!(0, 0), true),
            (p!(2, 2), true),
            (p!(2, 0), true),
            (p!(0, 2), true),
            (p!(3, 1), false),
            (p!(1, 3), false),
        ]
            .into_iter()
            .for_each(|(other, expectation)| assert_eq!(
                pos.is_diagonal_neighbour_with(&other),
                expectation,
                "{:?}.is_diagonal_neighbour_with({:?}) should be {}",
                pos,
                other,
                expectation
            ))
    }

    #[test]
    fn get_direction_to_neighbour_works() {
        let pos = p!(5, 5);

        [
            (p!(5, 5), None),
            (p!(6, 5), Some(XP)),
            (p!(4, 5), Some(XM)),
            (p!(5, 6), Some(YP)),
            (p!(5, 4), Some(YM)),
            (p!(6, 6), Some(XPYP)),
            (p!(6, 4), Some(XPYM)),
            (p!(4, 6), Some(XMYP)),
            (p!(4, 4), Some(XMYM)),
            (p!(7, 5), None),
        ]
            .into_iter()
            .for_each(|(other, expectation)| assert_eq!(
                pos.get_direction_to_neighbour(&other),
                expectation,
                "{:?}.get_direction_to_neighbour({:?}) should be {:?}",
                pos,
                other,
                expectation
            ))
    }

    #[test]
    fn distance_to_works() {
        [
            (p!(0,0), p!(0, 0), 0),
            (p!(0,0), p!(1, 1), 1),
            (p!(0,0), p!(3, 1), 3),
            (p!(0,0), p!(3, 3), 3),
            (p!(0,0), p!(-3, -3), 3),
        ]
            .into_iter()
            .for_each(|(a, b, expectation)| assert_eq!(
                a.distance_to(&b),
                expectation,
                "{:?}.distance_to({:?}) should be {}",
                a,
                b,
                expectation
            ))
        ;
    }

    #[test]
    #[cfg(feature = "bevy")]
    fn from_vec2_works() {
        let dimension = 32.0;

        let values_and_expectation = [
            (Vec2::new(0.0, 0.0), Position::new(0, 0)),
            (Vec2::new(dimension / 2.0, dimension / 2.0), Position::new(0, 0)),
            (Vec2::new(dimension, dimension), Position::new(1, 1)),
            (Vec2::new(-dimension / 2.0, -dimension / 2.0), Position::new(-1, -1)),
        ];

        values_and_expectation
            .into_iter()
            .for_each(|(val, expected)| assert_eq!(Position::from_vec2(val, Vec2::splat(dimension)), expected));
    }

    #[test]
    fn line_to_works() {
        let start = p!(0, 0);
        let goal = p!(6, 2);

        let positions = start.line_to(goal).into_iter().collect::<Vec<_>>();
        assert_eq!(positions.len(), 7);

        let expected = [
            p!(0, 0),
            p!(1, 0),
            p!(2, 1),
            p!(3, 1),
            p!(4, 1),
            p!(5, 2),
            p!(6, 2),
        ];

        assert_eq!(positions.len(), expected.len());

        for pos in expected {
            assert!(positions.contains(&pos), "expected position not in output: {:?}", pos);
        }
    }

    #[test]
    fn circle_works() {
        let origin = p!(0, 0);
        let radius = 4;

        let positions = origin.circle(radius).into_iter().collect::<Vec<_>>();

        let expected = [
            p!(-2, -3),
            p!(1, -4),
            p!(-1, -4),
            p!(4, -1),
            p!(2, -3),
            p!(-3, -3),
            p!(0, 4),
            p!(-4, 0),
            p!(-2, 3),
            p!(-3, 2),
            p!(3, -2),
            p!(-1, 4),
            p!(3, 3),
            p!(-4, -1),
            p!(-3, 3),
            p!(4, 0),
            p!(3, -3),
            p!(1, 4),
            p!(-4, 1),
            p!(2, 3),
            p!(3, 2),
            p!(-3, -2),
            p!(0, -4),
            p!(4, 1),
        ];

        assert_eq!(expected.len(), positions.len());

        for pos in expected {
            assert!(positions.contains(&pos), "expected position not in output: {:?}", pos);
        }
    }

    #[test]
    fn circle_filled_works() {
        let origin = p!(0, 0);
        let radius = 5;

        Position::print_positions(origin.circle(radius));
        Position::print_positions(origin.circle_filled(radius));
    }

    #[test]
    fn print_positions_works() {
        let positions = [
            p!(-2, -2),
            p!(-3, -2),
            p!(0, 0),
            p!(3, 3)
        ];

        Position::print_positions(positions);
    }
}