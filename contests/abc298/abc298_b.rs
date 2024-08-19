use proconio::input;

fn main() {
    input! {
        n: usize,
        a: [[usize; n]; n],
        b: [[usize; n]; n],
    }

    let cond = (0..4).any(|i| {
        rotate(a.clone(), i)
            .iter()
            .flatten()
            .zip(b.iter().flatten())
            .filter(|(&a, _)| a == 1)
            .all(|(_, &b)| b == 1)
    });

    println!("{}", if cond { "Yes" } else { "No" });
}

// rotate square matrix clockwise
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
