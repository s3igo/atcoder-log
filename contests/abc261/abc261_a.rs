use std::collections::HashSet;

use proconio::input;

fn main() {
    input! {
        l1: usize,
        r1: usize,
        l2: usize,
        r2: usize,
    }

    let red = (l1..=r1).collect::<HashSet<_>>();
    let blue = (l2..=r2).collect::<HashSet<_>>();

    let ans = (&red & &blue).len().saturating_sub(1);

    println!("{}", ans);
}
