use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, xy: [(isize, isize); n]);

    let ans = xy.iter().tuple_combinations().map(|(&a, &b)| euclidean_dst(a, b)).fold(0.0, |acc, x| x.max(acc));

    println!("{ans}");
}

fn euclidean_dst((ax, ay): (isize, isize), (bx, by): (isize, isize)) -> f64 {
    (((ax - bx).pow(2) + (ay - by).pow(2)) as f64).sqrt()
}
