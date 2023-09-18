use proconio::{input, marker::Chars};

fn main() {
    input!(s: [Chars; 10]);

    let a = s.iter().position(|s| s.contains(&'#')).unwrap() + 1;
    let b = s.iter().rposition(|s| s.contains(&'#')).unwrap() + 1;
    let rotated = rotate(s, 1);
    let c = rotated.iter().position(|s| s.contains(&'#')).unwrap() + 1;
    let d = rotated.iter().rposition(|s| s.contains(&'#')).unwrap() + 1;

    println!("{} {}", a, b);
    println!("{} {}", c, d);
}

fn rotate<T: Copy>(matrix: Vec<Vec<T>>, times: usize) -> Vec<Vec<T>> {
    let n = matrix.len();
    assert!(matrix.iter().all(|row| row.len() == n));
    if times == 0 {
        matrix
    } else {
        let rotated = (0..n).map(|i| (0..n).map(|j| matrix[n - 1 - j][i]).collect()).collect();
        rotate(rotated, times - 1)
    }
}
