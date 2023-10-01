use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        _: usize,
        m: usize,
        mut a: [usize; m],
    }

    a.insert(0, 0);
    let diffs = a.iter().tuple_windows().map(|(l, r)| r - l).collect_vec();

    let ans = diffs.iter().flat_map(|&a| (0..a).rev().collect_vec()).collect_vec();

    println!("{}", ans.iter().join("\n"));
}
