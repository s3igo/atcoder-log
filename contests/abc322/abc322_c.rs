use itertools::Itertools;
use proconio::input;

fn main() {
    input!(_: usize, m: usize, a: [usize; m]);

    let adjacent_diff = std::iter::once(0).chain(a).tuple_windows().map(|(l, r)| r - l);
    let ans = adjacent_diff.flat_map(|a| (0..a).rev()).collect_vec();

    println!("{}", ans.iter().join("\n"));
}
