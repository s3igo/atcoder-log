use itertools::Itertools;
use proconio::input;

fn main() {
    input!(h: isize, w: isize, n: isize);

    let mut grid = vec![vec!['.'; w as usize]; h as usize];
    let mut direction = 0;
    let mut cur = (0, 0);
    for _ in 0..n {
        let (i, j) = cur;
        if grid[i as usize][j as usize] == '.' {
            grid[i as usize][j as usize] = '#';
            direction = (direction + 1) % 4;
            cur = move_forward(direction, cur, (h, w));
        } else {
            grid[i as usize][j as usize] = '.';
            direction = (direction + 3) % 4;
            cur = move_forward(direction, cur, (h, w));
        }
    }

    println!("{}", grid.iter().map(|row| row.iter().join("")).join("\n"));
}

fn move_forward(direction: isize, cur: (isize, isize), max: (isize, isize)) -> (isize, isize) {
    let (i, j) = cur;
    let (h, w) = max;
    match direction {
        0 => (if i - 1 < 0 { h - 1 } else { i - 1 }, j),
        1 => (i, if j + 1 == w { 0 } else { j + 1 }),
        2 => (if i + 1 == h { 0 } else { i + 1 }, j),
        3 => (i, if j - 1 < 0 { w - 1 } else { j - 1 }),
        _ => unreachable!(),
    }
}
