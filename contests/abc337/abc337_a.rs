use std::cmp::Ordering;

use proconio::input;

fn main() {
    input!(n: usize, xy: [(usize, usize); n]);

    let (sum_x, sum_y) = xy.iter().fold((0, 0), |(acc_x, acc_y), (x, y)| (acc_x + x, acc_y + y));

    let ans = match sum_x.cmp(&sum_y) {
        Ordering::Greater => "Takahashi",
        Ordering::Less => "Aoki",
        Ordering::Equal => "Draw",
    };

    println!("{ans}");
}
