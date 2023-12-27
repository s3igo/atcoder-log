use itertools::Itertools;
use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, p: [Usize1; n]);

    let mut q = vec![0; n];
    for (&p, i) in p.iter().zip(1..) {
        q[p] = i;
    }

    println!("{}", q.iter().join(" "));
}
