use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize);

    let mut grid = vec![vec![None; n]; n];
    grid[0][0] = Some(1);
    grid[n / 2][n / 2] = Some(0);
    for i in 0.. {
        let idx = i / 4;
        let mut cur = if i != 0 && i % 4 == 0 {
            grid[idx][idx - 1].unwrap()
        } else {
            grid[idx][idx].unwrap()
        };
        grid[idx].iter_mut().for_each(|x| {
            if x.is_none() {
                cur += 1;
                *x = Some(cur);
            }
        });
        if grid.iter().flatten().all(|x| x.is_some()) {
            break;
        }
        grid = rotate(&grid, 3);
    }

    for row in grid {
        let row = row
            .iter()
            .map(|x| match x.unwrap() {
                0 => "T".to_string(),
                x => x.to_string(),
            })
            .join(" ");

        println!("{}", row);
    }
}

fn rotate<T: Copy>(matrix: &Vec<Vec<T>>, times: usize) -> Vec<Vec<T>> {
    let (n, m) = (matrix.len(), matrix[0].len());
    assert!(matrix.iter().all(|row| row.len() == m));
    match times {
        0 => matrix.clone(),
        _ => rotate(
            &(0..m).map(|i| (0..n).map(|j| matrix[n - 1 - j][i]).collect()).collect(),
            times - 1,
        ),
    }
}
