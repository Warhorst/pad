//! Contains methods to parse a board from a string input.

use std::str::FromStr;
use crate::board::{bounds_from_origin_and_dimension, Board};
use crate::p;
use crate::position::Position;

impl<T> FromStr for Board<T> where T: From<char>, {
    type Err = BoardParseError;

    fn from_str(input: &str) -> Result<Self, Self::Err> {
        let width = width_from_input(input)?;
        let height = height_from_input(input);

        let tiles = input
            .lines()
            .flat_map(|line| line.chars().map(|c| T::from(c)))
            .collect();

        Ok(Board {
            width,
            height,
            tiles,
            bounds: bounds_from_origin_and_dimension(p!(0, 0), width, height)
        })
    }
}

impl<T> From<&str> for Board<T> where T: From<char> {
    /// Create the board from a text input, where each character represents
    /// a tile.
    /// * `input` - The text input which represents the board. Expected to be a multiline string
    ///   where every line has the same amount of characters
    fn from(input: &str) -> Self {
        Self::from_str(input).expect("Error while parsing the board from input")
    }
}

impl<T> Board<T> {
    /// Same as From<&str>, but the tile type does not implement From<char>, so the provided
    /// mapper is used to parse the chars to tiles.
    /// * `input` - The text input which represents the board. Expected to be a multiline string
    ///   where every line has the same amount of characters
    /// * `map` - Mapper which converts a char to at tile
    pub fn from_str_using_mapping(
        input: &str,
        map: impl Fn(char) -> T,
    ) -> Result<Self, BoardParseError> {
        let width = width_from_input(input)?;
        let height = height_from_input(input);

        let tiles = input
            .lines()
            .flat_map(|line| line.chars().map(&map))
            .collect();

        Ok(Board {
            width,
            height,
            tiles,
            bounds: bounds_from_origin_and_dimension(p!(0, 0), width, height)
        })
    }
}

impl<T> Board<T> where T: From<char> {
    /// Create a board and all special tiles from the given text input
    ///
    /// * `input` - The text input which represents the board. Expected to be a multiline string
    ///   where every line has the same amount of characters
    /// * `special_map` - A mapping closure which might map a given char and position to a special
    ///   Tile and a Tile default. The special tile is part of the input, but not
    ///   of the actual board, so it gets extracted. If the char at the current
    ///   position could be mapped to a special tile, the special case gets stored
    ///   and the spot on the board gets replaced by the default tile, specified by
    ///   the mapper.
    pub fn board_and_specials_from_str<S>(
        input: &str,
        special_map: impl Fn(char, Position) -> Option<(S, T)>,
    ) -> Result<(Self, Vec<S>), BoardParseError> {
        let width = width_from_input(input)?;
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

        Ok((
            Board {
                width,
                height,
                tiles,
                bounds: bounds_from_origin_and_dimension(p!(0, 0), width, height)
            },
            specials
        ))
    }
}

fn width_from_input(input: &str) -> Result<usize, BoardParseError> {
    let mut width = None;

    for len in input.lines().map(|line| line.len()) {
        match width {
            None => width = Some(len),
            Some(current_len) => if len != current_len {
                return Err(BoardParseError::LinesHaveDifferentWidths)
            }
        }
    }

    match width {
        Some(width) => Ok(width),
        None => Err(BoardParseError::InputIsEmpty)
    }
}

fn height_from_input(input: &str) -> usize {
    input.lines().count()
}

/// Error that might be returned when a board could not
/// be parsed from a string input.
#[derive(Debug)]
pub enum BoardParseError {
    InputIsEmpty,
    LinesHaveDifferentWidths
}
