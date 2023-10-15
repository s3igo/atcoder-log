use itertools::Itertools;
use proconio::input;

fn main() {
    input!(s: String);

    let ans = ('0'..='9').find(|x| !s.chars().contains(x)).unwrap();

    println!("{}", ans);
}
