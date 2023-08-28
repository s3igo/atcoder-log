use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        _: usize,
        s: String,
    }

    let cond = s.chars().tuple_windows().all(|(l, r)| l != r);

    println!("{}", if cond { "Yes" } else { "No" });
}
