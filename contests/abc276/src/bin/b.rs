use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        ab: [(usize, usize); m],
    }

    let mut ans = vec![vec![]; n];
    for (a, b) in ab {
        ans[a - 1].push(b);
        ans[b - 1].push(a);
    }

    for mut e in ans {
        e.sort();
        println!("{} {}", e.len(), e.iter().join(" "));
    }
}
