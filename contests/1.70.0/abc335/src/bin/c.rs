use std::collections::VecDeque;

use proconio::input;

fn main() {
    input!(n: isize, q: usize);

    let mut pos: VecDeque<_> = (1..=n).map(|i| (i, 0)).collect();
    for _ in 0..q {
        input!(t: usize);
        match t {
            1 => {
                input!(c: char);
                let (x, y) = pos.front().unwrap();
                match c {
                    'R' => pos.push_front((*x + 1, *y)),
                    'L' => pos.push_front((*x - 1, *y)),
                    'U' => pos.push_front((*x, *y + 1)),
                    'D' => pos.push_front((*x, *y - 1)),
                    _ => unreachable!(),
                }
            },
            2 => {
                input!(p: usize);
                let (x, y) = pos[p - 1];
                println!("{x} {y}");
            },
            _ => unreachable!(),
        }
    }
}
