use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [usize; n],
    }

    let ans = a
        .iter()
        .enumerate()
        .fold(vec![false; n], |mut called, (i, e)| {
            if !called[i] {
                called[e - 1] = true;
            }
            called
        })
        .iter()
        .positions(|&b| !b)
        .map(|i| i + 1)
        .collect_vec();

    println!("{}", ans.len());
    println!("{}", ans.iter().join(" "));
}
