use itertools::iproduct;
use proconio::input;

fn main() {
    input!(s: usize, t: usize);

    let ans = iproduct!(0..=s, 0..=s, 0..=s)
        .filter(|(a, b, c)| a + b + c <= s)
        .filter(|(a, b, c)| a * b * c <= t)
        .count();

    println!("{ans}");
}
