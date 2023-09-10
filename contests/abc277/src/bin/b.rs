use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        s: [String; n],
    }
    let first = "HDCS";
    let second = "A23456789TJQK";

    let cond = s.iter().all(|s| {
        let mut chars = s.chars();
        let f = chars.next().unwrap();
        let s = chars.next().unwrap();
        first.contains(f) && second.contains(s)
    }) && s.iter().sorted().tuple_windows().all(|(a, b)| a != b);

    println!("{}", if cond { "Yes" } else { "No" });
}
