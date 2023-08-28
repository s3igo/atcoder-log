use itertools::{iproduct, Itertools};
use proconio::{input, marker::Chars};

fn main() {
    input! {
        r: usize,
        c: usize,
        b: [Chars; r],
    }

    let mut ans = b.clone();
    for (i, j) in iproduct!(0..r, 0..c) {
        if b[i][j].is_ascii_digit() {
            let might = b[i][j].to_digit(10).unwrap() as usize;
            for (k, l) in iproduct!(0..r, 0..c) {
                if manhattan_distance((i, j), (k, l)) <= might {
                    ans[k][l] = '.'
                }
            }
        }
    }

    println!("{}", ans.iter().map(|row| row.iter().join("")).join("\n"));
}

// to support before v1.51.0; `unsigned_abs()` is not available
#[allow(clippy::cast_abs_to_unsigned)]
fn manhattan_distance((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    (ax as i64 - bx as i64).abs() as usize + (ay as i64 - by as i64).abs() as usize
}
