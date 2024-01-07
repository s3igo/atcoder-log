use proconio::input;

fn main() {
    input!(n: usize);

    let mut grid = vec![vec![None; n]; n];
    grid[0][0] = Some(1);
    grid[(n + 1) / 2 - 1][(n + 1) / 2 - 1] = Some(isize::MAX);
    for i in 0.. {
        let idx = i / 4;
        let mut cur = grid[idx][idx].unwrap();
        grid[idx].iter_mut().for_each(|x| {
            if x.is_none() {
                *x = Some(cur);
                cur += 1;
            }
        });
        if grid.iter().flatten().all(|x| x.is_some()) {
            break;
        }
        grid = rotate(&grid, 3);
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
