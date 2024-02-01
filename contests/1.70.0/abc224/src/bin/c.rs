use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, xy: [(isize, isize); n]);

    let ans = xy
        .iter()
        .tuple_combinations()
        .filter(|(a, b, c)| (b.0 - a.0) * (c.1 - a.1) - (b.1 - a.1) * (c.0 - a.0) != 0)
        .count();

    println!("{ans}");
}
