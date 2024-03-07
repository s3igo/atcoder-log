use itertools::iproduct;
use proconio::input;

fn main() {
    input!(a: usize, b: usize, c: usize, x: usize);

    let ans = iproduct!(0..=a, 0..=b, 0..=c)
        .filter(|(i, j, k)| 500 * i + 100 * j + 50 * k == x)
        .count();

    println!("{ans}");
}
