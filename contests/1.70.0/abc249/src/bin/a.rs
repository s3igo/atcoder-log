use std::cmp::Ordering;

use proconio::{input, marker::Usize1};

fn main() {
    input!(a: usize, b: usize, c: usize, d: usize, e: usize, f: usize, x: Usize1);

    let ans = match distance(a, b, c, x).cmp(&distance(d, e, f, x)) {
        Ordering::Greater => "Takahashi",
        Ordering::Equal => "Draw",
        Ordering::Less => "Aoki",
    };

    println!("{}", ans);
}

fn distance(span: usize, speed: usize, rest: usize, time: usize) -> usize {
    (0..=time).fold(0, |acc, i| if i % (span + rest) < span { acc + speed } else { acc })
}
