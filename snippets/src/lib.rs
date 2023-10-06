#![allow(dead_code)]

use cargo_snippet::snippet;

#[snippet(name = ";rotate")]
// TODO: 正則でない行列に対応
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

#[test]
fn test_rotate() {
    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9]];
    assert_eq!(rotate(matrix.clone(), 0), vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9],]);
    assert_eq!(rotate(matrix.clone(), 1), vec![vec![7, 4, 1], vec![8, 5, 2], vec![9, 6, 3],]);
    assert_eq!(rotate(matrix.clone(), 2), vec![vec![9, 8, 7], vec![6, 5, 4], vec![3, 2, 1],]);
    assert_eq!(rotate(matrix.clone(), 3), vec![vec![3, 6, 9], vec![2, 5, 8], vec![1, 4, 7],]);
    assert_eq!(rotate(matrix.clone(), 4), vec![vec![1, 2, 3], vec![4, 5, 6], vec![7, 8, 9],]);
}

#[snippet(name = ";distance_manhattan")]
fn manhattan_distance((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    (ax as isize - bx as isize).abs() as usize + (ay as isize - by as isize).abs() as usize
}

#[test]
fn test_manhattan_distance() {
    assert_eq!(manhattan_distance((0, 0), (0, 0)), 0);
    assert_eq!(manhattan_distance((0, 0), (1, 0)), 1);
    assert_eq!(manhattan_distance((0, 0), (0, 1)), 1);
    assert_eq!(manhattan_distance((0, 0), (1, 1)), 2);
    assert_eq!(manhattan_distance((0, 0), (2, 3)), 5);
    assert_eq!(manhattan_distance((1, 2), (3, 4)), 4);
}

#[snippet(name = ";distance_chebyshev")]
fn chebyshev_distance((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    ax.abs_diff(bx).max(ay.abs_diff(by))
}

#[test]
fn test_chebyshev_distance() {
    assert_eq!(chebyshev_distance((0, 0), (0, 0)), 0);
    assert_eq!(chebyshev_distance((0, 0), (1, 0)), 1);
    assert_eq!(chebyshev_distance((0, 0), (0, 1)), 1);
    assert_eq!(chebyshev_distance((0, 0), (1, 1)), 1);
    assert_eq!(chebyshev_distance((0, 0), (2, 3)), 3);
    assert_eq!(chebyshev_distance((1, 2), (3, 4)), 2);
}
