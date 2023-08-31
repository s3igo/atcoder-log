use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        n: usize,
        p: usize,
        q: usize,
        r: usize,
        s: usize,
        mut a: [usize; n],
    }

    for (l, r) in (p - 1..q).zip(r - 1..s) {
        a.swap(l, r);
    }

    println!("{}", a.iter().join(" "));
}
