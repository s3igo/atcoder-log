use cargo_snippet::snippet;

#[snippet(name = ";matrix_rotate")]
// TODO: 正則でない行列に対応
fn rotate<T: Copy>(matrix: Vec<Vec<T>>, times: usize) -> Vec<Vec<T>> {
    let n = matrix.len();
    assert!(matrix.iter().all(|row| row.len() == n));
    match times {
        0 => matrix,
        _ => rotate(
            (0..n).map(|i| (0..n).map(|j| matrix[n - 1 - j][i]).collect()).collect(),
            times - 1,
        ),
    }
}

#[test]
fn test_rotate() {
    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];
    assert_eq!(rotate(matrix.clone(), 0), vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9],]);
    assert_eq!(rotate(matrix.clone(), 1), vec![vec![7, 4, 1], vec![8, 5, 2], vec![9, 6, 3],]);
    assert_eq!(rotate(matrix.clone(), 2), vec![vec![9, 8, 7], vec![6, 5, 4], vec![3, 2, 1],]);
    assert_eq!(rotate(matrix.clone(), 3), vec![vec![3, 6, 9], vec![2, 5, 8], vec![1, 4, 7],]);
    assert_eq!(rotate(matrix, 4), vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9],]);
}

#[snippet(name = ";matrix_transpose")]
fn transpose<T: Copy>(matrix: Vec<Vec<T>>) -> Vec<Vec<T>> {
    let n = matrix.len();
    assert!(matrix.iter().all(|row| row.len() == n));
    (0..n).map(|i| (0..n).map(|j| matrix[j][i]).collect()).collect()
}

#[test]
fn test_transpose() {
    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];
    assert_eq!(transpose(matrix), vec![vec![1, 4, 7], vec![2, 5, 8], vec![3, 6, 9],]);
    let matrix = vec![vec![1, 2], vec![3, 4]];
    assert_eq!(transpose(matrix), vec![vec![1, 3], vec![2, 4],]);
}
