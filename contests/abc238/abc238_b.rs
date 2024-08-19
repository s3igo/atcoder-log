use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let cutted = a
        .iter()
        .scan(0, |acc, a| {
            *acc = (*acc + a) % 360;
            Some(*acc)
        })
        .chain([0, 360])
        .sorted();

    let ans = cutted.tuple_windows().map(|(l, r)| r - l).max().unwrap();

    println!("{ans}");
}
