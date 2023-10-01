use itertools::{iproduct, Itertools};
use proconio::{input, marker::Usize1};

fn main() {
    input! {
        h: usize,
        w: usize,
        n: usize,
        abcd: [(Usize1, Usize1, Usize1, Usize1); n],
    }

    let mut table: Vec<Vec<isize>> = vec![vec![0; w]; h];
    for (a, b, c, d) in abcd {
        table[a][b] += 1;
        if let Some(e) = table[a].get_mut(d + 1) {
            *e -= 1;
        }
        if let Some(row) = table.get_mut(c + 1) {
            row[b] -= 1;
        }
        if let Some(row) = table.get_mut(c + 1) {
            if let Some(e) = row.get_mut(d + 1) {
                *e += 1;
            }
        }
    }
    let table = table;

    let mut cumsum = vec![vec![0; w]; h];
    for (i, j) in iproduct!(0..h, 0..w) {
        cumsum[i][j] = cumsum[i].get((j as isize - 1) as usize).unwrap_or(&0) + table[i][j];
    }
    for (i, j) in iproduct!(0..h, 0..w) {
        cumsum[i][j] =
            cumsum.get((i as isize - 1) as usize).and_then(|row| row.get(j)).unwrap_or(&0)
                + cumsum[i][j];
    }
    let cumsum = cumsum;

    for row in cumsum {
        println!("{}", row.iter().join(" "));
    }
}
