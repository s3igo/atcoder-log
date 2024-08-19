use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        k: usize,
        s: [String; n],
    }

    println!("{}", s.iter().take(k).sorted().join("\n"));
}
