# pad - Position and Direction
Provides the Position data type, which is a 2D position with signed x and y coordinates, alongside several methods to work with it.

This crate currently has 2 optional features:
- bevy: An integration for the bevy game engine ([Link](https://bevyengine.org/)), which provides derives for bevys Reflect trait, alongside methods to convert a Position to and from a Vec2/3
- serde: Provides for serdes Serialize and Deserialize traits
