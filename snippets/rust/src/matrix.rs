use cargo_snippet::snippet;
use itertools::iproduct;

#[snippet(name = ";matrix_rotate")]
fn rotate<T: Copy>(matrix: &Vec<Vec<T>>, times: usize) -> Vec<Vec<T>> {
    let (n, m) = (matrix.len(), matrix[0].len());
    assert!(matrix.iter().all(|row| row.len() == m));

    match times {
        0 => matrix.clone(),
        _ => rotate(&(0..m).map(|i| (0..n).map(|j| matrix[n - 1 - j][i]).collect()).collect(), times - 1),
    }
}

#[test]
fn test_rotate() {
    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];
    assert_eq!(rotate(&matrix, 0), vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9],]);
    assert_eq!(rotate(&matrix, 1), vec![vec![7, 4, 1], vec![8, 5, 2], vec![9, 6, 3],]);
    assert_eq!(rotate(&matrix, 2), vec![vec![9, 8, 7], vec![6, 5, 4], vec![3, 2, 1],]);
    assert_eq!(rotate(&matrix, 3), vec![vec![3, 6, 9], vec![2, 5, 8], vec![1, 4, 7],]);
    assert_eq!(rotate(&matrix, 4), vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9],]);

    let matrix = vec![vec![1, 2], vec![3, 4]];
    assert_eq!(rotate(&matrix, 1), vec![vec![3, 1], vec![4, 2],]);

    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6]];
    assert_eq!(rotate(&matrix, 1), vec![vec![4, 1], vec![5, 2], vec![6, 3],]);

    let matrix = vec![vec![1, 2], vec![3, 4], vec![5, 6]];
    assert_eq!(rotate(&matrix, 1), vec![vec![5, 3, 1], vec![6, 4, 2],]);
}

#[snippet(name = ";matrix_transpose")]
fn transpose<T: Copy>(matrix: &Vec<Vec<T>>) -> Vec<Vec<T>> {
    let (n, m) = (matrix.len(), matrix[0].len());
    assert!(matrix.iter().all(|row| row.len() == m));

    (0..m).map(|i| (0..n).map(|j| matrix[j][i]).collect()).collect()
}

#[test]
fn test_transpose() {
    // [
    //   [1, 2, 3],
    //   [4, 5, 6],
    //   [7, 8, 9],
    // ] => [
    //   [1, 4, 7],
    //   [2, 5, 8],
    //   [3, 6, 9],
    // ]
    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];
    assert_eq!(transpose(&matrix), vec![vec![1, 4, 7], vec![2, 5, 8], vec![3, 6, 9],]);

    // [
    //   [1, 2],
    //   [3, 4],
    // ] => [
    //   [1, 3],
    //   [2, 4],
    // ]
    let matrix = vec![vec![1, 2], vec![3, 4]];
    assert_eq!(transpose(&matrix), vec![vec![1, 3], vec![2, 4],]);

    // [
    //   [1, 2, 3],
    //   [4, 5, 6],
    // ] => [
    //   [1, 4],
    //   [2, 5],
    //   [3, 6],
    // ]
    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6]];
    assert_eq!(transpose(&matrix), vec![vec![1, 4], vec![2, 5], vec![3, 6],]);

    // [
    //   [1, 2],
    //   [3, 4],
    //   [5, 6],
    // ] => [
    //   [1, 3, 5],
    //   [2, 4, 6],
    // ]
    let matrix = vec![vec![1, 2], vec![3, 4], vec![5, 6]];
    assert_eq!(transpose(&matrix), vec![vec![1, 3, 5], vec![2, 4, 6],]);
}

#[snippet(name = ";matrix_chunks")]
fn chunks<T: Copy>(matrix: &Vec<Vec<T>>, side: usize) -> Vec<Vec<Vec<T>>> {
    let (n, m) = (matrix.len(), matrix[0].len());
    assert!(matrix.iter().all(|row| row.len() == m));
    assert!(n % side == 0 && m % side == 0);

    iproduct!((0..n).step_by(side), (0..m).step_by(side))
        .map(|(i, j)| (0..side).map(|k| matrix[i + k][j..j + side].to_vec()).collect())
        .collect()
}

#[test]
fn test_chunks() {
    // [
    //   [1, 2, 3, 4],
    //   [5, 6, 7, 8],
    //   [9, 10, 11, 12],
    //   [13, 14, 15, 16],
    // ] => [
    //   [
    //     [1, 2],
    //     [5, 6],
    //   ], [
    //     [3, 4],
    //     [7, 8],
    //   ], [
    //     [9, 10],
    //     [13, 14],
    //   ], [
    //     [11, 12],
    //     [15, 16],
    //   ],
    // ]
    let matrix = vec![vec![1, 2, 3, 4], vec![5, 6, 7, 8], vec![9, 10, 11, 12], vec![13, 14, 15, 16]];
    assert_eq!(
        chunks(&matrix, 2),
        vec![
            vec![vec![1, 2], vec![5, 6]],
            vec![vec![3, 4], vec![7, 8]],
            vec![vec![9, 10], vec![13, 14]],
            vec![vec![11, 12], vec![15, 16]],
        ]
    );

    // [
    //   [1, 2, 3, 4, 5, 6],
    //   [7, 8, 9, 10, 11, 12],
    //   [13, 14, 15, 16, 17, 18],
    // ] => [
    //   [
    //     [1, 2, 3],
    //     [7, 8, 9],
    //     [13, 14, 15],
    //   ], [
    //     [4, 5, 6],
    //     [10, 11, 12],
    //     [16, 17, 18],
    //   ],
    // ]
    let matrix = vec![vec![1, 2, 3, 4, 5, 6], vec![7, 8, 9, 10, 11, 12], vec![13, 14, 15, 16, 17, 18]];
    assert_eq!(
        chunks(&matrix, 3),
        vec![
            vec![vec![1, 2, 3], vec![7, 8, 9], vec![13, 14, 15]],
            vec![vec![4, 5, 6], vec![10, 11, 12], vec![16, 17, 18]],
        ]
    );
}

#[snippet(name = ";matrix_windows")]
fn windows<T: Copy>(matrix: &Vec<Vec<T>>, side: usize) -> Vec<Vec<Vec<T>>> {
    let (n, m) = (matrix.len(), matrix[0].len());
    assert!(matrix.iter().all(|row| row.len() == m));
    assert!(n >= side && m >= side);

    iproduct!(0..n - side + 1, 0..m - side + 1)
        .map(|(i, j)| (0..side).map(|k| matrix[i + k][j..j + side].to_vec()).collect())
        .collect()
}

#[test]
fn test_windows() {
    // [
    //   [1, 2, 3, 4],
    //   [5, 6, 7, 8],
    //   [9, 10, 11, 12],
    //   [13, 14, 15, 16],
    // ]
    let matrix = vec![vec![1, 2, 3, 4], vec![5, 6, 7, 8], vec![9, 10, 11, 12], vec![13, 14, 15, 16]];
    // windows(2) => [
    //   [
    //      [1, 2],
    //      [5, 6],
    //   ], [
    //      [2, 3],
    //      [6, 7],
    //   ], [
    //      [3, 4],
    //      [7, 8],
    //   ], [
    //      [5, 6],
    //      [9, 10],
    //   ], [
    //      [6, 7],
    //      [10, 11],
    //   ], [
    //      [7, 8],
    //      [11, 12],
    //   ], [
    //      [9, 10],
    //      [13, 14],
    //   ], [
    //      [10, 11],
    //      [14, 15],
    //   ], [
    //      [11, 12],
    //      [15, 16],
    //   ],
    // ]
    assert_eq!(
        windows(&matrix, 2),
        vec![
            vec![vec![1, 2], vec![5, 6]],
            vec![vec![2, 3], vec![6, 7]],
            vec![vec![3, 4], vec![7, 8]],
            vec![vec![5, 6], vec![9, 10]],
            vec![vec![6, 7], vec![10, 11]],
            vec![vec![7, 8], vec![11, 12]],
            vec![vec![9, 10], vec![13, 14]],
            vec![vec![10, 11], vec![14, 15]],
            vec![vec![11, 12], vec![15, 16]],
        ]
    );
    // windows(3) => [
    //  [
    //     [1, 2, 3],
    //     [5, 6, 7],
    //     [9, 10, 11],
    //  ], [
    //     [2, 3, 4],
    //     [6, 7, 8],
    //     [10, 11, 12],
    //  ], [
    //     [5, 6, 7],
    //     [9, 10, 11],
    //     [13, 14, 15],
    //  ], [
    //     [6, 7, 8],
    //     [10, 11, 12],
    //     [14, 15, 16],
    //  ],
    // ]
    assert_eq!(
        windows(&matrix, 3),
        vec![
            vec![vec![1, 2, 3], vec![5, 6, 7], vec![9, 10, 11]],
            vec![vec![2, 3, 4], vec![6, 7, 8], vec![10, 11, 12]],
            vec![vec![5, 6, 7], vec![9, 10, 11], vec![13, 14, 15]],
            vec![vec![6, 7, 8], vec![10, 11, 12], vec![14, 15, 16]],
        ]
    );
    // [
    //   [1, 2, 3, 4],
    //   [5, 6, 7, 8],
    //   [9, 10, 11, 12],
    // ]
    let matrix = vec![vec![1, 2, 3, 4], vec![5, 6, 7, 8], vec![9, 10, 11, 12]];
    // windows(2) => [
    //   [
    //      [1, 2],
    //      [5, 6],
    //   ], [
    //      [2, 3],
    //      [6, 7],
    //   ], [
    //      [3, 4],
    //      [7, 8],
    //   ], [
    //      [5, 6],
    //      [9, 10],
    //   ], [
    //      [6, 7],
    //      [10, 11],
    //   ], [
    //      [7, 8],
    //      [11, 12],
    //   ],
    // ]
    assert_eq!(
        windows(&matrix, 2),
        vec![
            vec![vec![1, 2], vec![5, 6]],
            vec![vec![2, 3], vec![6, 7]],
            vec![vec![3, 4], vec![7, 8]],
            vec![vec![5, 6], vec![9, 10]],
            vec![vec![6, 7], vec![10, 11]],
            vec![vec![7, 8], vec![11, 12]],
        ]
    );
    // windows(3) => [
    //  [
    //     [1, 2, 3],
    //     [5, 6, 7],
    //     [9, 10, 11],
    //  ], [
    //     [2, 3, 4],
    //     [6, 7, 8],
    //     [10, 11, 12],
    //  ],
    // ]
    assert_eq!(
        windows(&matrix, 3),
        vec![vec![vec![1, 2, 3], vec![5, 6, 7], vec![9, 10, 11]], vec![vec![2, 3, 4], vec![6, 7, 8], vec![10, 11, 12]],]
    );
}
