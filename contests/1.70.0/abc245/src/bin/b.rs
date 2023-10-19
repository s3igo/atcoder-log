use std::collections::HashSet;

use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let a = a.into_iter().collect::<HashSet<_>>();
    let seq = (0..=n).collect::<HashSet<_>>();
    let ans = seq.difference(&a).min().unwrap();

    println!("{}", ans);
}
