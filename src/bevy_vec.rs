//! Provides helper methods for bevys [`Vec2`] and [`Vec3`] vector types.

use crate::position::Position;
use bevy_math::*;

impl Position {
    /// Converts [`Vec2`] coordinates to a tile [`Position`].
    ///
    /// Use case: The game world is a big plane, which is logically divided in tiles. Every tile has
    /// the same dimension, and every tile has a unique position. The goal is to get the position
    /// the given coordinates are in.
    ///
    /// This conversion expects the tile sprites coordinates to be the bottom left corner.
    ///
    /// * `coordinates` - The coordinates to convert
    /// * `dimension` - The dimension of the tiles.
    pub fn from_vec2(
        coordinates: Vec2,
        dimension: Vec2,
    ) -> Self {
        let mut x_div = coordinates.x / dimension.x;
        let mut y_div = coordinates.y / dimension.y;

        if x_div < 0.0 {
            x_div = x_div.floor()
        }

        if y_div < 0.0 {
            y_div = y_div.floor()
        }

        Position::new(x_div as isize, y_div as isize)
    }

    /// Same as [`Position::from_vec2`], but expects the tile sprites to use bevys default Anchor
    /// (the tiles coordinates are in the center of the sprite).
    pub fn vec2_to_tile_pos(
        coordinates: Vec2,
        dimension: Vec2,
    ) -> Self {
        Position::from_vec2(coordinates + dimension * 0.5, dimension)
    }

    /// Same as [`Position::from_vec2`], but for [`Vec3`] coordinates instead.
    pub fn from_vec3(
        coordinates: Vec3,
        dimension: Vec2,
    ) -> Self {
        Position::from_vec2(coordinates.xy(), dimension)
    }

    /// Same as [`Position::vec2_to_tile_pos`], but for [`Vec3`] coordinates instead.
    pub fn vec3_to_tile_pos(
        coordinates: Vec3,
        dimension: Vec2,
    ) -> Position {
        Position::vec2_to_tile_pos(coordinates.xy(), dimension)
    }

    /// Converts this [`Position`] to [`Vec2`] coordinates.
    ///
    /// Use case: The game world is a big plane, which is logically divided in tiles. Every tile has
    /// the same dimension, and every tile has a unique position. The goal is to get the corner coordinates
    /// for the tile at this position.
    ///
    /// This conversion expects the tile sprites coordinates to be the bottom left corner.
    pub fn to_vec2(
        &self,
        dimension: Vec2,
    ) -> Vec2 {
        Vec2::new(self.x as f32 * dimension.x, self.y as f32 * dimension.y)
    }

    /// Same as [`Position::to_vec2`], but for [`Vec3`] coordinates. As the Vec3 also needs a z coordinate, it is provided as
    /// a parameter.
    pub fn to_vec3(
        &self,
        dimension: Vec2,
        z: f32,
    ) -> Vec3 {
        Vec3::from((self.to_vec2(dimension), z))
    }
}

#[cfg(test)]
mod tests {
    use crate::position::Position;
    use bevy_math::*;

    #[test]
    fn from_vec2_works() {
        let dimension = 32.0;

        let values_and_expectation = [
            (Vec2::new(0.0, 0.0), Position::new(0, 0)),
            (
                Vec2::new(dimension / 2.0, dimension / 2.0),
                Position::new(0, 0),
            ),
            (Vec2::new(dimension, dimension), Position::new(1, 1)),
            (
                Vec2::new(-dimension / 2.0, -dimension / 2.0),
                Position::new(-1, -1),
            ),
        ];

        values_and_expectation
            .into_iter()
            .for_each(|(val, expected)| {
                assert_eq!(Position::from_vec2(val, Vec2::splat(dimension)), expected)
            });
    }

    #[test]
    fn vec2_to_tile_pos_works() {
        let dimension = 32.0;

        let values_and_expectation = [
            (Vec2::new(0.0, 0.0), Position::new(0, 0)),
            (Vec2::new(-10.0, -10.0), Position::new(0, 0)),
            (Vec2::new(10.0, 10.0), Position::new(0, 0)),
            (Vec2::new(-20.0, -20.0), Position::new(-1, -1)),
            (Vec2::new(20.0, 20.0), Position::new(1, 1)),
        ];

        values_and_expectation
            .into_iter()
            .for_each(|(val, expected)| {
                assert_eq!(
                    Position::vec2_to_tile_pos(val, Vec2::splat(dimension)),
                    expected
                );
            });
    }
}
