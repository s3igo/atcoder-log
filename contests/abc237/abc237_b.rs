use itertools::Itertools;
use proconio::input;

fn main() {
    input!(h: usize, w: usize, a: [[usize; w]; h]);

    let b = transpose(&a);

    println!("{}", b.iter().map(|row| row.iter().join(" ")).join("\n"));
}

fn transpose<T: Copy>(matrix: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let (n, m) = (matrix.len(), matrix[0].len());
    assert!(matrix.iter().map(|row| row.len()).all(|x| x == m));
    (0..m).map(|i| (0..n).map(|j| matrix[j][i]).collect()).collect()
}
