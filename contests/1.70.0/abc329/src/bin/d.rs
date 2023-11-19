use std::collections::BTreeSet;

use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, m: usize, a: [Usize1; m]);

    let mut set = std::iter::repeat(m).zip(0..n).collect::<BTreeSet<_>>();
    let mut cnt = vec![m; n];

    for a in a {
        set.remove(&(cnt[a], a));
        cnt[a] -= 1;
        set.insert((cnt[a], a));
        let ans = set.first().unwrap().1 + 1;
        println!("{ans}");
    }
}
