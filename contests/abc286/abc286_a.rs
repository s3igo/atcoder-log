use itertools::Itertools;
use proconio::{input, marker::Usize1};

fn main() {
    input! {
        n: usize,
        p: Usize1,
        q: Usize1,
        r: Usize1,
        s: Usize1,
        mut a: [usize; n],
    }

    for (l, r) in (p..=q).zip(r..=s) {
        a.swap(l, r);
    }

    println!("{}", a.iter().join(" "));
}
