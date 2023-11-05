use std::collections::HashSet;

use itertools::{iproduct, Itertools};
use proconio::input;

fn main() {
    input!(a: [[usize; 9]; 9]);

    let cmp = (1..=9).collect_vec();
    let cmp = cmp.iter().collect::<HashSet<&usize>>();

    let cond = a.iter().all(|row| row.iter().collect::<HashSet<&usize>>() == cmp)
        && rotate(&a, 1).iter().all(|row| row.iter().collect::<HashSet<&usize>>() == cmp);

    let mut third = true;
    for (i, j) in iproduct!([0, 3, 6], [0, 3, 6]) {
        if [
            a[i][j],
            a[i][j + 1],
            a[i][j + 2],
            a[i + 1][j],
            a[i + 1][j + 1],
            a[i + 1][j + 2],
            a[i + 2][j],
            a[i + 2][j + 1],
            a[i + 2][j + 2],
        ]
        .iter()
        .collect::<HashSet<&usize>>()
            != cmp
        {
            third = false;
            break;
        }
    }

    let cond = cond && third;

    println!("{}", if cond { "Yes" } else { "No" });
}

fn rotate<T: Copy>(matrix: &Vec<Vec<T>>, times: usize) -> Vec<Vec<T>> {
    let (n, m) = (matrix.len(), matrix[0].len());
    assert!(matrix.iter().map(|row| row.len()).all(|x| x == m));
    match times {
        0 => matrix.clone(),
        _ => rotate(
            &(0..m).map(|i| (0..n).map(|j| matrix[n - 1 - j][i]).collect()).collect(),
            times - 1,
        ),
    }
}
