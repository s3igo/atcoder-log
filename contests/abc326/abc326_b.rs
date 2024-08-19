use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize);

    let ans = (n..).map(|i| [i / 100, i / 10 % 10, i % 10]).find(|n| n[0] * n[1] == n[2]).unwrap();

    println!("{}", ans.iter().join(""));
}
