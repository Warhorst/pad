use crate::position::Position;

pub struct PositionIter {
    current_x_front: isize,
    current_y_front: isize,
    current_x_back: isize,
    current_y_back: isize,
    start: Position,
    end: Position,
    direction_vec: (isize, isize),
    finished: bool
}

impl PositionIter {
    pub (crate) fn new(start: Position, end: Position) -> Self {
        let dir_x = match end.x - start.x {
            val if val > 0 => 1,
            val if val < 0 => -1,
            _ => 0
        };

        let dir_y = match end.y - start.y {
            val if val > 0 => 1,
            val if val < 0 => -1,
            _ => 0
        };

        let direction_vec = (dir_x, dir_y);

        PositionIter {
            current_x_front: start.x,
            current_y_front: start.y,
            current_x_back: end.x,
            current_y_back: end.y,
            start,
            end,
            direction_vec,
            finished: false
        }
    }

    fn is_finished(&self) -> bool {
        self.current_x_front == self.current_x_back && self.current_y_front == self.current_y_back
    }

    fn progress(
        direction_vec: (isize, isize),
        current_x: &mut isize,
        current_y: &mut isize,
        start: Position,
        end: Position
    ) {
        match direction_vec {
            (1, 0) => {
                *current_x += direction_vec.0;
            },
            (-1, 0) => {
                *current_x += direction_vec.0;
            },
            (0, 1) => {
                *current_y += direction_vec.1;
            },
            (0, -1) => {
                *current_y += direction_vec.1;
            },
            (1, 1) => {
                *current_x += direction_vec.0;

                if *current_x > end.x && *current_y < end.y {
                    *current_x = start.x;
                    *current_y += direction_vec.1;
                }
            },
            (-1, 1) => {
                *current_x += direction_vec.0;

                if *current_x < end.x && *current_y < end.y {
                    *current_x = start.x;
                    *current_y += direction_vec.1;
                }
            },
            (1, -1) => {
                *current_x += direction_vec.0;

                if *current_x > end.x && *current_y > end.y {
                    *current_x = start.x;
                    *current_y += direction_vec.1;
                }
            },
            (-1, -1) => {
                *current_x += direction_vec.0;

                if *current_x < end.x && *current_y > end.y {
                    *current_x = start.x;
                    *current_y += direction_vec.1;
                }
            },
            _ => {}
        }

    }
}

impl Iterator for PositionIter {
    type Item = Position;

    fn next(&mut self) -> Option<Self::Item> {
        if self.finished {
            return None;
        }

        let current = Position::new(self.current_x_front, self.current_y_front);
        self.finished = self.is_finished();
        Self::progress(
            self.direction_vec,
            &mut self.current_x_front,
            &mut self.current_y_front,
            self.start,
            self.end
        );
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
        if self.finished {
            return None
        }

        let current = Position::new(self.current_x_back, self.current_y_back);
        self.finished = self.is_finished();
        // differences when progressing to the next position backwards:
        // - the direction vector gets reversed
        // - we modify the back values for x and y
        // - we switch start and end, as end is treated as the start and start is treated as the end
        Self::progress(
            (-self.direction_vec.0, -self.direction_vec.1),
            &mut self.current_x_back,
            &mut self.current_y_back,
            self.end,
            self.start,
        );
        Some(current)
    }
}

#[cfg(test)]
mod tests {
    use crate::p;
    use crate::position::Position;

    #[test]
    fn position_iter_works() {
        [
            // same position -> should return only this position
            (
                p!(0, 0),
                p!(0, 0),
                vec![(0, 0)]
            ),
            // block top left to bottom right
            (
                p!(-1, -1),
                p!(1, 1),
                vec![(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
            ),
            // block bottom right to top left
            (
                p!(1, 1),
                p!(-1, -1),
                vec![(1, 1), (0, 1), (-1, 1), (1, 0), (0, 0), (-1, 0), (1, -1), (0, -1), (-1, -1)]
            ),
            // block bottom left to top right
            (
                p!(-1, 1),
                p!(1, -1),
                vec![(-1, 1), (0, 1), (1, 1), (-1, 0), (0, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]
            ),
            // block top right to bottom left
            (
                p!(1, -1),
                p!(-1, 1),
                vec![(1, -1), (0, -1), (-1, -1), (1, 0), (0, 0), (-1, 0), (1, 1), (0, 1), (-1, 1)]
            ),
            // line left to right
            (
                p!(0, 0),
                p!(2, 0),
                vec![(0, 0), (1, 0), (2, 0)]
            ),
            // line right to left
            (
                p!(2, 0),
                p!(0, 0),
                vec![(2, 0), (1, 0), (0, 0)]
            ),
            // line top to bottom
            (
                p!(0, 0),
                p!(0, 2),
                vec![(0, 0), (0, 1), (0, 2)]
            ),
            // line bottom to top
            (
                p!(0, 2),
                p!(0, 0),
                vec![(0, 2), (0, 1), (0, 0)]
            )
        ]
            .into_iter()
            .for_each(|(start, end, expected): (Position, Position, Vec<(isize, isize)>)| {
                let expected: Vec<Position> = expected.into_iter().map(From::from).collect();
                assert_eq!(start.iter_to(end).collect::<Vec<_>>(), expected, "{start:?} -> {end:?}")
            })
    }

    #[test]
    fn position_iter_rev_works() {
        [
            // same position -> should return only this position
            (
                p!(0, 0),
                p!(0, 0),
                vec![(0, 0)]
            ),
            // block top left to bottom right
            (
                p!(-1, -1),
                p!(1, 1),
                vec![(1, 1), (0, 1), (-1, 1), (1, 0), (0, 0), (-1, 0), (1, -1), (0, -1), (-1, -1)],
            ),
            // block bottom right to top left
            (
                p!(1, 1),
                p!(-1, -1),
                vec![(-1, -1), (0, -1), (1, -1), (-1, 0), (0, 0), (1, 0), (-1, 1), (0, 1), (1, 1)]
            ),
            // block bottom left to top right
            (
                p!(-1, 1),
                p!(1, -1),
                vec![(1, -1), (0, -1), (-1, -1), (1, 0), (0, 0), (-1, 0), (1, 1), (0, 1), (-1, 1)]
            ),
            // block top right to bottom left
            (
                p!(1, -1),
                p!(-1, 1),
                vec![(-1, 1), (0, 1), (1, 1), (-1, 0), (0, 0), (1, 0), (-1, -1), (0, -1), (1, -1)]
            ),
            // line left to right
            (
                p!(0, 0),
                p!(2, 0),
                vec![(2, 0), (1, 0), (0, 0)]
            ),
            // line right to left
            (
                p!(2, 0),
                p!(0, 0),
                vec![(0, 0), (1, 0), (2, 0)]
            ),
            // line top to bottom
            (
                p!(0, 0),
                p!(0, 2),
                vec![(0, 2), (0, 1), (0, 0)]
            ),
            // line bottom to top
            (
                p!(0, 2),
                p!(0, 0),
                vec![(0, 0), (0, 1), (0, 2)]
            )
        ]
            .into_iter()
            .for_each(|(start, end, expected): (Position, Position, Vec<(isize, isize)>)| {
                let expected: Vec<Position> = expected.into_iter().map(From::from).collect();
                assert_eq!(start.iter_to(end).rev().collect::<Vec<_>>(), expected, "{start:?} -> {end:?}")
            })
    }

    #[test]
    fn position_iter_size_hint_works() {
        let iter = p!(0, 0).iter_to(p!(4, 4));
        assert_eq!(iter.size_hint(), (1, Some(16)))
    }
}