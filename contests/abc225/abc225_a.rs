use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    let ans = s.chars().permutations(s.len()).unique().count();

    println!("{ans}");
}
