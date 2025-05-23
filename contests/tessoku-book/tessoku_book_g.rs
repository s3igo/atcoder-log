use itertools::Itertools;
use itertools_num::ItertoolsNum;
use proconio::{input, marker::Usize1};

fn main() {
    input!(d: usize, n: usize, lr: [(Usize1, Usize1); n]);

    // 前日比
    let inc = lr.iter().fold(vec![0; d], |mut acc, (l, r)| {
        acc[*l] += 1;
        if let Some(e) = acc.get_mut(r + 1) {
            *e -= 1;
        }
        acc
    });

    let cumsum = inc.iter().cumsum().collect::<Vec<isize>>();

    println!("{}", cumsum.iter().join("\n"));
}
