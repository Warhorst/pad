pub mod board;
pub mod shape;

use std::cmp::{max, min};
use std::collections::HashSet;
use std::ops::{Add, AddAssign, Mul, Neg, Sub, SubAssign};

#[cfg(feature = "bevy")]
use bevy_math::*;
#[cfg(feature = "bevy")]
use bevy_reflect::Reflect;
#[cfg(feature = "serde")]
use serde::{Deserialize, Serialize};
use Direction::*;

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
        Direction::dirs().map(|dir| self.neighbour_in_direction(dir))
    }

    /// Return the neighbours in all four cardinal directions (up, down, left, right)
    pub fn cardinal_neighbours(&self) -> [Position; 4] {
        Direction::cardinal_dirs().map(|dir| self.neighbour_in_direction(dir))
    }

    /// Return all neighbours of the given position with the direction they are relative to the origin
    pub fn neighbours_with_directions(&self) -> [(Position, Direction); 8] {
        Direction::dirs().map(|dir| (self.neighbour_in_direction(dir), dir))
    }

    /// Return the neighbours in all four cardinal directions (up, down, left, right)
    /// with the direction they are relative to the origin
    pub fn cardinal_neighbours_with_directions(&self) -> [(Position, Direction); 4] {
        Direction::cardinal_dirs().map(|dir| (self.neighbour_in_direction(dir), dir))
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
    /// // XXXXX
    /// // XXXXX
    /// // XXXXX
    /// // XXXXX
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
            return 0.0;
        }

        self.x.abs_diff(other.x) as f32 + self.y.abs_diff(other.y) as f32
    }

    /// Calculates the euclidean distance between this position and another one (https://en.wikipedia.org/wiki/Euclidean_distance)
    pub fn euclidean_distance(&self, other: &Position) -> f32 {
        if self == other {
            return 0.0;
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
            (self.y - other.y).abs() as usize,
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

    /// Tells if this position is in the given bounds
    pub fn in_bounds(&self, bounds: Bounds) -> bool {
        bounds.contains_position(*self)
    }
}

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

/// Builder like object which is used to print a simple representation of the given positions to the terminal.
/// Signs are omitted, top/right goes to positive infinity, down/left to negative infinity.
pub struct PositionPrinter {
    /// Tells if the x anc y-axis should be printed. Defaults to true
    draw_axis: bool,
    /// Tells if (0, 0) on the printed positions is top left rather than bottom left. Defaults to false
    y_is_top: bool,
    /// The bounds of the printed positions. If not set, the bounds will be calculated from the given positions. Defaults to None
    bounds: Option<Bounds>,
}

impl PositionPrinter {
    pub fn new() -> Self {
        PositionPrinter {
            draw_axis: true,
            y_is_top: false,
            bounds: None,
        }
    }

    pub fn draw_axis(mut self, draw_axis: bool) -> Self {
        self.draw_axis = draw_axis;
        self
    }

    pub fn y_is_top(mut self, y_is_top: bool) -> Self {
        self.y_is_top = y_is_top;
        self
    }

    pub fn bounds(mut self, bounds: Bounds) -> Self {
        self.bounds = Some(bounds);
        self
    }

    pub fn print(
        self,
        positions: impl IntoIterator<Item=Position>,
    ) {
        let positions = positions.into_iter().collect::<HashSet<_>>();

        let default_mapping = |pos: Position| if positions.contains(&pos) {
            'X'
        } else {
            ' '
        };

        println!("{}", self.to_string(&positions, default_mapping));
    }

    pub fn print_with_mapping(
        self,
        positions: impl IntoIterator<Item=Position>,
        pos_map: impl Fn(Position) -> char,
    ) {
        let positions = positions.into_iter().collect::<HashSet<_>>();
        println!("{}", self.to_string(&positions, pos_map));
    }

    pub fn to_string(
        self,
        positions: &HashSet<Position>,
        pos_map: impl Fn(Position) -> char,
    ) -> String {
        // todo this code was partially written while drunk, and so it looks like. refactor!

        let mut result = String::new();

        let bounds = match self.bounds {
            Some(b) => b,
            None => Bounds::from_positions(positions.iter().copied())
        };

        if self.draw_axis {
            result += &self.print_x_axis(bounds, false);
        }

        if self.draw_axis {
            result += &self.print_with_y_axis(bounds, pos_map)
        } else {
            result += &self.print_without_y_axis(bounds, pos_map)
        }

        if self.draw_axis {
            result += &self.print_x_axis(bounds, true);
        }

        result
    }

    fn print_x_axis(
        &self,
        bounds: Bounds,
        reverse: bool,
    ) -> String {
        let mut result = String::new();

        let max_x_digital_places = bounds.min_x.abs().to_string().len().max(bounds.max_x.abs().to_string().len());
        let max_y_digital_places = bounds.min_y.abs().to_string().len().max(bounds.max_y.abs().to_string().len());

        // if the x-axis should be reversed, it will be written from bottom to top instead of top to bottom
        // this just looks better
        let places = if reverse {
            (0..max_x_digital_places).rev().into_iter().collect::<Vec<_>>()
        } else {
            (0..max_x_digital_places).into_iter().collect::<Vec<_>>()
        };

        for i in places {
            // shift the start of the x-axis so it matches with the start of the y values
            result += &(0..max_y_digital_places).into_iter().map(|_| ' ').collect::<String>();

            for x in bounds.min_x..=bounds.max_x {
                // append enough whitespace so every number is in line
                let x_str = format!(
                    "{}{}",
                    (0..(max_x_digital_places - x.abs().to_string().len())).into_iter().map(|_| ' ').collect::<String>(),
                    x.abs()
                );

                let char = x_str.chars().skip(i).next().unwrap();
                result += &format!("{char}");
            }
            result += "\n";
        }

        result
    }

    fn print_with_y_axis(
        &self,
        bounds: Bounds,
        pos_map: impl Fn(Position) -> char,
    ) -> String {
        let mut result = String::new();
        let max_digital_places = bounds.min_y.abs().to_string().len().max(bounds.max_y.abs().to_string().len());

        let mut append_result = |y: isize| {
            // append enough whitespace so every number is in line
            result += &format!(
                "{}{}",
                (0..(max_digital_places - y.abs().to_string().len())).into_iter().map(|_| ' ').collect::<String>(),
                y.abs()
            );

            for x in bounds.min_x..=bounds.max_x {
                result += &format!("{}", pos_map(p!(x, y)));
            }

            result += &format!("{}\n", y.abs());
        };

        if self.y_is_top {
            for y in bounds.min_y..=bounds.max_y {
                append_result(y);
            }
        } else {
            for y in (bounds.min_y..=bounds.max_y).rev() {
                append_result(y);
            }
        }

        result
    }

    fn print_without_y_axis(
        &self,
        bounds: Bounds,
        pos_map: impl Fn(Position) -> char,
    ) -> String {
        let mut result = String::new();
        let mut append_result = |y: isize| {
            for x in bounds.min_x..=bounds.max_x {
                result += &format!("{}", pos_map(p!(x, y)));
            }

            result += "\n";
        };

        if self.y_is_top {
            for y in bounds.min_y..=bounds.max_y {
                append_result(y)
            }
        } else {
            for y in (bounds.min_y..=bounds.max_y).rev() {
                append_result(y)
            }
        }

        result
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

impl AddAssign for Position {
    fn add_assign(&mut self, rhs: Self) {
        *self = *self + rhs
    }
}

impl AddAssign<(isize, isize)> for Position {
    fn add_assign(&mut self, rhs: (isize, isize)) {
        *self = *self + rhs
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

impl SubAssign for Position {
    fn sub_assign(&mut self, rhs: Self) {
        *self = *self - rhs
    }
}

impl SubAssign<(isize, isize)> for Position {
    fn sub_assign(&mut self, rhs: (isize, isize)) {
        *self = *self - rhs
    }
}

impl Mul<i32> for Position {
    type Output = Position;

    fn mul(self, rhs: i32) -> Self::Output {
        p!(rhs as isize * self.x, rhs as isize * self.y)
    }
}

impl Mul<isize> for Position {
    type Output = Position;

    fn mul(self, rhs: isize) -> Self::Output {
        p!(rhs * self.x, rhs * self.y)
    }
}

impl Mul<Position> for i32 {
    type Output = Position;

    fn mul(self, rhs: Position) -> Self::Output {
        p!(rhs.x * self as isize, rhs.y * self as isize)
    }
}

impl Mul<Position> for isize {
    type Output = Position;

    fn mul(self, rhs: Position) -> Self::Output {
        p!(rhs.x * self, rhs.y * self)
    }
}

impl Neg for Position {
    type Output = Position;

    fn neg(self) -> Self::Output {
        -1 * self
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

// todo adapt the position iter to allow any direction (currently only -inf, -inf to +inf, +inf is allowed)
//  maybe store the direction and apply the x/y adaptions accordingly

pub struct PositionIter {
    current_x_front: isize,
    current_y_front: isize,
    current_x_back: isize,
    current_y_back: isize,
    start: Position,
    end: Position,
}

impl PositionIter {
    fn new(start: Position, end: Position) -> Self {
        if start.x > end.x || start.y > end.y {
            panic!("start must be less or equal to end")
        }

        PositionIter {
            current_x_front: start.x,
            current_y_front: start.y,
            current_x_back: end.x,
            current_y_back: end.y,
            start,
            end,
        }
    }

    fn is_finished(&self) -> bool {
        self.current_x_front > self.current_x_back || self.current_y_front > self.current_y_back
    }

    fn increment(&mut self) {
        self.current_x_front += 1;

        if self.current_x_front > self.end.x && self.current_y_front < self.end.y {
            self.current_x_front = self.start.x;
            self.current_y_front += 1;
        }
    }

    fn increment_back(&mut self) {
        self.current_x_back -= 1;

        if self.current_x_back < self.start.x && self.current_y_back > self.start.y {
            self.current_x_back = self.end.x;
            self.current_y_back -= 1;
        }
    }
}

impl Iterator for PositionIter {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        if self.is_finished() {
            return None;
        }

        let current = Position::new(self.current_x_front, self.current_y_front);
        self.increment();
        Some(current)
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let size = ((self.end.x - self.start.x) * (self.end.y - self.start.y)) as usize;
        // The iterator will always have at least one element, the start position
        (1, Some(size))
    }
}

impl DoubleEndedIterator for PositionIter {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.is_finished() {
            return None;
        }

        let current = Position::new(self.current_x_back, self.current_y_back);
        self.increment_back();
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
    /// Get all possible directions in the order they are defined in Direction.
    pub fn dirs() -> [Direction; 8] {
        [XP, XM, YP, YM, XPYP, XPYM, XMYP, XMYM]
    }

    /// Get all cardinal directions in the order they are defined in Direction.
    pub fn cardinal_dirs() -> [Direction; 4] {
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
    use crate::Direction::*;
    use crate::{Bounds, Position, PositionPrinter};
    #[cfg(feature = "bevy")]
    use bevy_math::Vec2;

    #[test]
    fn add_position_works() {
        let mut pos = p!(1, 2);
        let other = p!(2, 3);
        assert_eq!(p!(3, 5), pos + other);

        pos += other;
        assert_eq!(p!(3, 5), pos);
    }

    #[test]
    fn add_tuple_works() {
        let mut pos = p!(1, 2);
        let other = (2, 3);
        assert_eq!(p!(3, 5), pos + other);

        pos += other;
        assert_eq!(p!(3, 5), pos);
    }

    #[test]
    fn sup_position_works() {
        let mut pos = p!(1, 2);
        let other = p!(2, 3);
        assert_eq!(p!(-1, -1), pos - other);

        pos -= other;
        assert_eq!(p!(-1, -1), pos);
    }

    #[test]
    fn sub_tuple_works() {
        let mut pos = p!(1, 2);
        let other = (2, 3);
        assert_eq!(p!(-1, -1), pos - other);

        pos -= other;
        assert_eq!(p!(-1, -1), pos);
    }

    #[test]
    fn mul_works() {
        let pos = p!(-5, 6);
        let expected = p!(25, -30);

        assert_eq!(pos * -5i32, expected);
        assert_eq!(pos * -5isize, expected);
        assert_eq!(-5i32 * pos, expected);
        assert_eq!(-5isize * pos, expected);
    }

    #[test]
    fn neg_works() {
        let pos = p!(-5, 6);
        assert_eq!(-pos, p!(5, -6));
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
    fn position_iter_rev_works() {
        [
            (
                p!(0, 0),
                p!(0, 0),
                vec![p!(0, 0)]
            ),
            (
                p!(-1, -1),
                p!(1, 1),
                [(1isize, 1isize), (0, 1), (-1, 1), (1, 0), (0, 0), (-1, 0), (1, -1), (0, -1), (-1, -1)].into_iter().map(From::from).collect()
            ),
            (
                p!(0, 0),
                p!(2, 0),
                [(2isize, 0isize), (1, 0), (0, 0)].into_iter().map(From::from).collect()
            ),
            (
                p!(0, 0),
                p!(0, 2),
                [(0isize, 2isize), (0, 1), (0, 0)].into_iter().map(From::from).collect()
            )
        ]
            .into_iter()
            .for_each(|(start, end, expected)| assert_eq!(start.iter_to(end).rev().collect::<Vec<_>>(), expected))
    }

    #[test]
    fn position_iter_size_hint_works() {
        let iter = p!(0, 0).iter_to(p!(4, 4));
        assert_eq!(iter.size_hint(), (1, Some(16)))
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
        let radius = 15;

        PositionPrinter::new().print(origin.circle(radius));
        PositionPrinter::new().print(origin.circle_filled(radius));
    }

    #[test]
    fn print_positions_works() {
        let positions = [
            p!(-2, -2),
            p!(-3, -2),
            p!(0, 0),
            p!(3, 3)
        ];

        println!("basic with axis");
        PositionPrinter::new().print(positions);
        println!("basic with axis and custom bounds");
        PositionPrinter::new().bounds(Bounds::new(-5, -5, 5, 5)).print(positions);
        println!("basic without axis");
        PositionPrinter::new().draw_axis(false).print(positions);
        println!("basic with axis and y is top");
        PositionPrinter::new().y_is_top(true).print(positions);
        println!("basic without axis and y is top");
        PositionPrinter::new().draw_axis(false).y_is_top(true).print(positions);
        println!("basic without axis and custom position mapping");
        PositionPrinter::new()
            .draw_axis(false)
            .print_with_mapping(positions, |pos| if positions.contains(&pos) {
                'O'
            } else {
                'X'
            });
    }
}