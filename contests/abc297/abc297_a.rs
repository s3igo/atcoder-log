use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        d: usize,
        t: [usize; n],
    }

    let ans = t
        .iter()
        .tuple_windows()
        .find_map(|(l, r)| if r - l <= d { Some(*r as i64) } else { None })
        .unwrap_or(-1);

    println!("{}", ans);
}
