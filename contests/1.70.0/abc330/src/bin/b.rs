use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, l: usize, r: usize, a: [usize; n]);

    let ans: Vec<_> = a.iter().map(|&a| a.clamp(l, r)).collect();

    println!("{}", ans.iter().join(" "));
}
