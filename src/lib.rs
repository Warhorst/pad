#[derive(Copy, Clone, Eq, PartialEq, Hash)]
pub struct Position {
    x: isize,
    y: isize
}

#[derive(Copy, Clone, Eq, PartialEq)]
pub enum Direction {
    Left,
    Right,
    Up,
    Down,
}