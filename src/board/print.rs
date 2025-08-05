//! Contains methods to print a [Board] to stdout.

use crate::board::Board;
use crate::position_printer::{PositionPrinter, RenderStyle};

impl<T> Board<T> where for<'a> &'a T: Into<char>, {
    /// Print the board to the console, using the Into<char> implementation of the tile
    /// to represent it.
    pub fn print(&self) {
        self.print_with_mapping(|t| t.into())
    }
}

impl<T> Board<T> {
    /// Print every position in the board, using the given mapper to represent the tile as a char
    /// used in the printed output.
    pub fn print_with_mapping(&self, map: impl Fn(&T) -> char) {
        PositionPrinter::new()
            .draw_axis(false)
            .render_style(RenderStyle::Screen)
            .print_with_mapping(self.positions(), |pos| match self.get_tile(pos) {
                Some(t) => map(t),
                None => ' '
            })
    }
}

impl<T> Board<T> where T: Eq + PartialEq {
    /// Print all the tiles where the current tile equals the given tile
    pub fn print_occurrences_of_tile(&self, tile: T) {
        PositionPrinter::new()
            .draw_axis(false)
            .render_style(RenderStyle::Screen)
            .print(self.tiles_and_positions().filter_map(|(t, p)| match tile == *t {
                true => Some(p),
                false => None
            }))
    }
}