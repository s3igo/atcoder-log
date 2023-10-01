use itertools::iproduct;
use proconio::{input, marker::Usize1};

fn main() {
    input! {
        n: usize,
        xy: [(Usize1, Usize1); n],
        q: usize,
        abcd: [(Usize1, Usize1, Usize1, Usize1); q],
    }
    let width = 1500;

    let mut table = vec![vec![0; width]; width];
    for (x, y) in xy {
        table[x][y] += 1;
    }
    let table = table;

    let mut cumsum = vec![vec![0; width]; width];
    for (i, j) in iproduct!(0..width, 0..width) {
        cumsum[i][j] = cumsum[i].get((j as isize - 1) as usize).unwrap_or(&0) + table[i][j];
    }
    for (i, j) in iproduct!(0..width, 0..width) {
        cumsum[i][j] =
            cumsum.get((i as isize - 1) as usize).and_then(|row| row.get(j)).unwrap_or(&0)
                + cumsum[i][j];
    }
    let cumsum = cumsum;

    for (a, b, c, d) in abcd {
        let br = cumsum[c][d];
        let tl = cumsum
            .get((a as isize - 1) as usize)
            .and_then(|row| row.get((b as isize - 1) as usize))
            .unwrap_or(&0);
        let tr = cumsum.get((a as isize - 1) as usize).and_then(|row| row.get(d)).unwrap_or(&0);
        let bl = cumsum[c].get((b as isize - 1) as usize).unwrap_or(&0);

        println!("{}", br + tl - tr - bl);
    }
}
