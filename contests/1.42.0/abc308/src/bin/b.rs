use std::collections::HashMap;

use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        c: [String; n],
        d: [String; m],
        p0: usize,
        p: [usize; m],
    }

    let dict = d.iter().zip(p.iter()).collect::<HashMap<_, _>>();
    let ans = c.iter().map(|x| *dict.get(x).unwrap_or(&&p0)).sum::<usize>();

    println!("{}", ans);
}
