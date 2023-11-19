use itertools::iproduct;
use proconio::input;

fn main() {
    input!(n: usize, k: usize);

    let ans = iproduct!(1..=n, 1..=n)
        .map(|(i, j)| k.saturating_sub(i).saturating_sub(j))
        .filter(|e| (1..=n).contains(e))
        .count();

    println!("{ans}");
}
