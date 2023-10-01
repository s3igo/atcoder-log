use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        fs: [(usize, usize); n],
    }

    let ans = fs.iter().tuple_combinations().fold(0, |acc, ((f1, s1), (f2, s2))| {
        acc.max(if f1 != f2 {
            s1 + s2
        } else {
            s1.max(s2) + s1.min(s2) / 2
        })
    });

    println!("{}", ans); // NOTE: TLE
}
