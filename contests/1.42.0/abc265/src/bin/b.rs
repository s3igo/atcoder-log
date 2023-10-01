use std::collections::HashMap;

use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        t: isize,
        a: [isize; n - 1],
        xy: [(usize, isize); m],
    }

    let xy = xy.into_iter().collect::<HashMap<_, _>>();

    let cond = a.iter().zip(2..).try_fold(t, |acc, (a, i)| match acc - a {
        ..=0 => None,
        acc => Some(acc + *xy.get(&i).unwrap_or(&0)),
    });

    println!("{}", if cond.is_some() { "Yes" } else { "No" });
}
