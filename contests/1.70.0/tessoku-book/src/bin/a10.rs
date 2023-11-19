use std::cmp::max;

use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, a: [usize; n], d: usize, lr: [(Usize1, Usize1); d]);

    let cummax_l = a
        .iter()
        .scan(0, |acc, &x| {
            *acc = max(*acc, x);
            Some(*acc)
        })
        .collect::<Vec<_>>();
    let binding = a
        .iter()
        .rev()
        .scan(0, |acc, &x| {
            *acc = max(*acc, x);
            Some(*acc)
        })
        .collect::<Vec<_>>();
    let cummax_r = binding.iter().rev().collect::<Vec<_>>();

    for (l, r) in lr {
        let ans = max(cummax_l[l - 1], *cummax_r[r + 1]);

        println!("{}", ans);
    }
}
