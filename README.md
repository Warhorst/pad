# pad - Position and Direction
A library to work with positions, directions and more. It provides the following core types:
- [Position](./src/position.rs): A 2D point with integer x / y coordinates
- [Direction](./src/direction.rs): Cardinal and diagonal directions
- [Board](./src/board/mod.rs): A 2D area consisting of tiles
- [Shape](./src/shape.rs): An irregular collection of positions

Optional Features
- `bevy`: An integration for [Bevy](https://bevyengine.org/), which provides derives for bevys Reflect trait, alongside methods to convert a Position to and from a Vec2/3
- `serde`: Provides for [serde](https://github.com/serde-rs/serde)s Serialize and Deserialize traits
- `board_shape_macro`: Provide a macro to create a Shape from a string. Optional, as it uses [indoc](https://github.com/dtolnay/indoc) internally.

## Ideas
A collections of ideas which could be implemented in this library:
- Rotating a board
- Flipping a board
- Iterating over a (rectangular) subarea of a board
- Using Vec internally in the Shape
- Crating a shape from given positions
- Rotating a shape
- Flipping a shape
