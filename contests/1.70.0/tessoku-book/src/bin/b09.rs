use itertools::iproduct;
use proconio::input;

fn main() {
    input!(n: usize, abcd: [(usize, usize, usize, usize); n]);
    let width = 1501;

    let mut table = vec![vec![0; width]; width];
    for (a, b, c, d) in abcd {
        table[a][b] += 1;
        table[a][d] -= 1;
        table[c][b] -= 1;
        table[c][d] += 1;
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

    let ans = cumsum.iter().flatten().filter(|&&e| e > 0).count();

    println!("{ans}");
}
