use itertools::Itertools;
use itertools_num::ItertoolsNum;
use proconio::{input, marker::Usize1};

fn main() {
    input! {
        d: usize,
        n: usize,
        lr: [(Usize1, Usize1); n],
    }

    // 前日比
    let mut inc: Vec<isize> = vec![0; d];
    for (l, r) in lr {
        *inc.get_mut(l).unwrap() += 1;
        if let Some(e) = inc.get_mut(r + 1) {
            *e -= 1;
        }
    }

    let cumsum = inc.iter().cumsum().collect::<Vec<isize>>();

    println!("{}", cumsum.iter().join("\n"));
}
