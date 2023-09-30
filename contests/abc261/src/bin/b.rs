use proconio::{input, marker::Chars};

fn main() {
    input! {
        n: usize,
        a: [Chars; n],
    }

    let cond = transpose(a.clone())
        .iter()
        .flat_map(|row| {
            row.iter().map(|c| match c {
                'W' => 'L',
                'L' => 'W',
                &c => c,
            })
        })
        .zip(a.into_iter().flatten())
        .all(|(c1, c2)| c1 == c2);

    println!("{}", if cond { "correct" } else { "incorrect" });
}

fn transpose<T: Copy>(matrix: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let n = matrix.len();
    assert!(matrix.iter().all(|row| row.len() == n));
    (0..n).map(|i| (0..n).map(|j| matrix[j][i]).collect()).collect()
}
