use itertools::Itertools;
use proconio::input;

fn main() {
    input!(_: usize, _: usize, n: usize, ab: [(usize, usize); n]);

    let (a, b): (Vec<_>, Vec<_>) = ab.iter().cloned().unzip();

    for (a, b) in compress(&a).iter().zip(compress(&b)) {
        println!("{a} {b}");
    }
}

fn compress(v: &[usize]) -> Vec<usize> {
    let asc: Vec<_> = v.iter().sorted().dedup().collect();
    v.iter().map(|x| asc.binary_search(&x).unwrap() + 1).collect()
}
