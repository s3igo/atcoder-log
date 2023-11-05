use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, s: String);

    let cond = s.chars().tuple_windows().any(|(l, r)| l == 'a' && r == 'b' || l == 'b' && r == 'a');

    println!("{}", if cond { "Yes" } else { "No" });
}
