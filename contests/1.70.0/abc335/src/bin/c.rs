use std::collections::VecDeque;

use proconio::input;

fn main() {
    input!(n: isize, q: usize, query: [(isize, String); q]);

    let mut pos: VecDeque<_> = (1..=n).zip(std::iter::repeat(0)).collect();
    for (idx, s) in query {
        match idx {
            1 => {
                let (x, y) = pos.front().unwrap();
                match s.chars().next().unwrap() {
                    'R' => pos.push_front((*x + idx, *y)),
                    'L' => pos.push_front((*x - idx, *y)),
                    'U' => pos.push_front((*x, *y + idx)),
                    'D' => pos.push_front((*x, *y - idx)),
                    _ => unreachable!(),
                }
                pos.pop_back();
            },
            2 => {
                let p = s.parse::<usize>().unwrap();
                let (x, y) = pos[p - 1];
                println!("{x} {y}");
            },
            _ => unreachable!(),
        }
    }
}
