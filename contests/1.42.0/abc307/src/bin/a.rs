use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [[usize; 7]; n],
    }

    let ans = a.iter().map(|x| x.iter().sum::<usize>()).collect_vec();

    println!("{}", ans.iter().join(" "));
}
