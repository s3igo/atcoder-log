use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: String);

    let cond = n.chars().tuple_windows().all(|(l, r)| l > r);

    println!("{}", if cond { "Yes" } else { "No" });
}
