use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        h: usize,
        w: usize,
        c: [String; h],
    }

    let ans = c.iter().fold(vec![0; w], |mut acc, row| {
        for i in row.chars().positions(|c| c == '#') {
            acc[i] += 1;
        }
        acc
    });

    println!("{}", ans.iter().join(" "));
}
