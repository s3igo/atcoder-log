use std::collections::VecDeque;

use itertools::{Itertools, MinMaxResult};
use proconio::input;

fn main() {
    input!(s: String);

    let minmax = (0..s.len())
        .map(|i| {
            let mut s = s.chars().collect::<VecDeque<_>>();
            s.rotate_left(i);
            s
        })
        .minmax();

    let (min, max): (String, String) = match minmax {
        MinMaxResult::OneElement(s) => (s.iter().collect(), s.iter().collect()),
        MinMaxResult::MinMax(min, max) => (min.iter().collect(), max.iter().collect()),
        _ => unreachable!(),
    };

    println!("{min}\n{max}");
}
