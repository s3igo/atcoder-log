use itertools::Itertools;
use proconio::input;

fn main() {
    input!(h: usize, w: usize, n: usize);

    let mut grid = vec![vec![false; w]; h];
    let mut direction = 0;
    let mut cur = (0, 0);
    for _ in 0..n {
        let (i, j) = cur;
        grid[i][j] = !grid[i][j];
        direction = (direction + if grid[i][j] { 1 } else { 3 }) % 4;
        cur = move_forward(direction, cur, (h, w));
    }

    for row in grid {
        println!("{}", row.iter().map(|&is| if is { '#' } else { '.' }).join(""));
    }
}

fn move_forward(direction: usize, (i, j): (usize, usize), (h, w): (usize, usize)) -> (usize, usize) {
    match direction {
        0 => ((i + h - 1) % h, j),
        1 => (i, (j + 1) % w),
        2 => ((i + 1) % h, j),
        3 => (i, (j + w - 1) % w),
        _ => unreachable!(),
    }
}
