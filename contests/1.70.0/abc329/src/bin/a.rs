use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    println!("{}", s.chars().join(" "));
}
