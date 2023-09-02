use itertools::iproduct;
use proconio::input;

fn main() {
    input! {
        n: isize,
        k: isize,
    }

    let ans = iproduct!(1..=n, 1..=n).map(|(a, b)| k - a - b).filter(|&e| 1 <= e && e <= n).count();

    println!("{}", ans);
}
