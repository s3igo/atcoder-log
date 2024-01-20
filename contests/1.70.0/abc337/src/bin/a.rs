use std::cmp::Ordering;

use proconio::input;

fn main() {
    input!(n: usize, xy: [(usize, usize); n]);

    let (sum_x, sum_y) = xy.iter().fold((0, 0), |(sum_x, sum_y), (x, y)| (sum_x + x, sum_y + y));

    let ans = match sum_x.cmp(&sum_y) {
        Ordering::Less => "Aoki",
        Ordering::Greater => "Takahashi",
        Ordering::Equal => "Draw",
    };

    println!("{ans}");
}
