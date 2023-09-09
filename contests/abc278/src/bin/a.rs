use std::cmp::Ordering;

use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        k: usize,
        a: [usize; n],
    }

    let ans = match n.cmp(&k) {
        Ordering::Greater => a.iter().skip(k).chain(std::iter::repeat(&0).take(k)).collect_vec(),
        _ => vec![&0; n],
    };

    println!("{}", ans.iter().join(" "));
}
