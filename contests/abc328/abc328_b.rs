use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, d: [usize; n]);

    let ans = d
        .iter()
        .zip(1..)
        .map(|(&d, i)| {
            (1..=d)
                .filter(|&j| i % 10 == j % 10 && i.to_string().chars().all_equal() && j.to_string().chars().all_equal())
                .count()
        })
        .sum::<usize>();

    println!("{ans}");
}
