use itertools_num::ItertoolsNum;
use proconio::{input, marker::Usize1};

fn main() {
    input! {
        n: usize,
        q: usize,
        a: [usize; n],
        lr: [(Usize1, Usize1); q],
    }

    let cumsum = a.iter().cumsum().collect::<Vec<usize>>();

    for (l, r) in lr {
        println!("{}", cumsum[r] - cumsum[l]);
    }
}
