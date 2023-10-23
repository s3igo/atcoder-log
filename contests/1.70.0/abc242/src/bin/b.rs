use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    let ans = s.chars().sorted().collect::<String>();

    println!("{ans}");
}
