use crate::bounds::Bounds;
use crate::p;
use crate::position::Position;
use std::collections::HashSet;

/// Builder like object which is used to print a simple representation of the given positions to the terminal.
/// Signs are omitted, top/right goes to positive infinity, down/left to negative infinity.
pub struct PositionPrinter {
    /// Tells if the x anc y-axis should be printed. Defaults to true
    draw_axis: bool,
    /// Tells how the positions will be rendered depending on their y coordinate
    render_style: RenderStyle,
    /// The bounds of the printed positions. If not set, the bounds will be calculated from the given positions. Defaults to None
    bounds: Option<Bounds>,
}

/// Defines how the positions get rendered depending on their y coordinate
#[derive(Clone, Copy, Default, Eq, PartialOrd, PartialEq)]
pub enum RenderStyle {
    /// The positions will be rendered like a
    /// frame on a monitor, where the low y values are on top and the
    /// high y values are at the bottom.
    #[default]
    Screen,
    /// The positions will be rendered like
    /// a coordinate system, where the low y values are on the bottom
    /// and the high y values are on the top.
    CoordinateSystem
}

impl PositionPrinter {
    pub fn new() -> Self {
        PositionPrinter {
            draw_axis: true,
            render_style: RenderStyle::CoordinateSystem,
            bounds: None,
        }
    }

    pub fn draw_axis(mut self, draw_axis: bool) -> Self {
        self.draw_axis = draw_axis;
        self
    }
    
    pub fn render_style(mut self, y_axis_render_style: RenderStyle) -> Self {
        self.render_style = y_axis_render_style;
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

    // todo this is very ineffective. I should use only one string and append to it, instead of
    //  creating new strings for every line of the output.
    //  Also, a hash map is only required to figure out the bounds, there must be a better way for this
    pub fn to_string(
        self,
        positions: &HashSet<Position>,
        pos_map: impl Fn(Position) -> char,
    ) -> String {
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
            (0..max_x_digital_places).rev().collect::<Vec<_>>()
        } else {
            (0..max_x_digital_places).collect::<Vec<_>>()
        };

        for i in places {
            // shift the start of the x-axis so it matches with the start of the y values
            result += &(0..max_y_digital_places).map(|_| ' ').collect::<String>();

            for x in bounds.min_x..=bounds.max_x {
                // append enough whitespace so every number is in line
                let x_str = format!(
                    "{}{}",
                    (0..(max_x_digital_places - x.abs().to_string().len())).map(|_| ' ').collect::<String>(),
                    x.abs()
                );

                let char = x_str.chars().nth(i).unwrap();
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
                (0..(max_digital_places - y.abs().to_string().len())).map(|_| ' ').collect::<String>(),
                y.abs()
            );

            for x in bounds.min_x..=bounds.max_x {
                result += &format!("{}", pos_map(p!(x, y)));
            }

            result += &format!("{}\n", y.abs());
        };
        
        match self.render_style {
            RenderStyle::Screen => {
                for y in bounds.min_y..=bounds.max_y {
                    append_result(y);
                }
            }
            RenderStyle::CoordinateSystem => {
                for y in (bounds.min_y..=bounds.max_y).rev() {
                    append_result(y);
                }
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
        
        match self.render_style {
            RenderStyle::Screen => {
                for y in bounds.min_y..=bounds.max_y {
                    append_result(y)
                }
            }
            RenderStyle::CoordinateSystem => {
                for y in (bounds.min_y..=bounds.max_y).rev() {
                    append_result(y)
                }
            }
        }

        result
    }
}

#[cfg(test)]
mod tests {
    use crate::bounds::Bounds;
    use crate::p;
    use crate::position_printer::{PositionPrinter, RenderStyle};

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
        println!("basic with axis and screen render style");
        PositionPrinter::new().render_style(RenderStyle::Screen).print(positions);
        println!("basic without axis and screen render style");
        PositionPrinter::new().draw_axis(false).render_style(RenderStyle::Screen).print(positions);
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