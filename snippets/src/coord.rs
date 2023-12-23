use std::collections::HashMap;

use cargo_snippet::snippet;
use itertools::Itertools;

#[snippet(name = ";coord_manhattan-distance")]
fn manhattan_dst((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    ax.abs_diff(bx) + ay.abs_diff(by)
}

#[test]
fn test_manhattan_dst() {
    assert_eq!(manhattan_dst((0, 0), (0, 0)), 0);
    assert_eq!(manhattan_dst((0, 0), (1, 0)), 1);
    assert_eq!(manhattan_dst((0, 0), (0, 1)), 1);
    assert_eq!(manhattan_dst((0, 0), (1, 1)), 2);
    assert_eq!(manhattan_dst((0, 0), (2, 3)), 5);
    assert_eq!(manhattan_dst((1, 2), (3, 4)), 4);
}

#[snippet(name = ";coord_chebyshev-distance")]
fn chebyshev_dst((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize {
    ax.abs_diff(bx).max(ay.abs_diff(by))
}

#[test]
fn test_chebyshev_dst() {
    assert_eq!(chebyshev_dst((0, 0), (0, 0)), 0);
    assert_eq!(chebyshev_dst((0, 0), (1, 0)), 1);
    assert_eq!(chebyshev_dst((0, 0), (0, 1)), 1);
    assert_eq!(chebyshev_dst((0, 0), (1, 1)), 1);
    assert_eq!(chebyshev_dst((0, 0), (2, 3)), 3);
    assert_eq!(chebyshev_dst((1, 2), (3, 4)), 2);
}

#[snippet(
    name = ";coord_compress",
    prefix = "use std::collections::HashMap;\nuse itertools::Itertools;"
)]

fn compress(v: &[usize]) -> Vec<usize> {
    let ranks: HashMap<_, _> = v.iter().sorted().dedup().zip(1..).collect();
    v.iter().map(|x| ranks[x]).collect()
}

#[test]
fn test_compress() {
    assert_eq!(compress(&[1, 2, 3, 4, 5]), vec![1, 2, 3, 4, 5]);
    assert_eq!(compress(&[1, 1, 1, 1, 1]), vec![1, 1, 1, 1, 1]);
    assert_eq!(compress(&[1, 2, 3, 4, 5, 1, 2, 3, 4, 5]), vec![1, 2, 3, 4, 5, 1, 2, 3, 4, 5]);
    assert_eq!(compress(&[1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6]), vec![1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6]);
    assert_eq!(compress(&[1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6]), vec![1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6]);
    assert_eq!(compress(&[1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6]), vec![1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6]);
}
