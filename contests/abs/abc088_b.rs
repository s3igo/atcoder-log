use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let ans = a
        .iter()
        .sorted()
        .rev()
        .batching(|it| it.next().map(|x| (x, it.next().unwrap_or(&0))))
        .fold(0, |acc, (x, y)| acc + x - y);

    println!("{ans}");
}
