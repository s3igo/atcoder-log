use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        s: [isize; n],
    }

    let s = std::iter::once(0).chain(s).collect_vec();

    let ans = s.iter().tuple_windows().map(|(&l, &r)| r - l).collect_vec();

    println!("{}", ans.iter().join(" "));
}
