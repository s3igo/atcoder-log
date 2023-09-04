use itertools::Itertools;
use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
    input! {
        t: usize,
        n: usize,
        lr: [(usize, usize); n],
    }

    let mut table = vec![0; t + 1];
    for (l, r) in lr {
        table[l] += 1;
        table[r] -= 1;
    }
    let table = table;

    let cumsum = table.iter().dropping_back(1).cumsum().collect::<Vec<isize>>();

    println!("{}", cumsum.iter().join("\n"));
}
