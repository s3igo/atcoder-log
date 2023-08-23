use itertools::Itertools;
use proconio::{input, marker::Chars};

// TODO: wip
fn main() {
    input! {
        h: usize,
        _w: usize,
        s: [Chars; h],
    }

    let s = s
        .iter()
        .enumerate()
        .map(|(i, row)| row.iter().enumerate().map(|(j, c)| ((i, j), c)).collect_vec())
        .collect_vec();
    let mut strs = vec![];
    for row in s {
        strs.push(row);
    }

    // for (i, j) in iproduct!(0..h, 0..w) {
    //     let
    // }
}
