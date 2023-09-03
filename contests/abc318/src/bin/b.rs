use itertools::iproduct;
use proconio::input;

fn main() {
    input! {
        n: usize,
        abcd: [(usize, usize, usize, usize); n],
    }

    let mut area = vec![vec![0; 100]; 100];
    for (a, b, c, d) in abcd {
        for (x, y) in iproduct!(a..b, c..d) {
            area[x][y] = 1;
        }
    }

    let ans = area.iter().flatten().sum::<usize>();

    println!("{}", ans);
}
