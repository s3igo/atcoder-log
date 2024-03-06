use std::collections::HashMap;

use cargo_snippet::snippet;
use itertools::Itertools;

#[snippet(name = ";coord_manhattan-distance")]
fn manhattan_dst((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize { ax.abs_diff(bx) + ay.abs_diff(by) }

#[snippet(name = ";coord_chebyshev-distance")]
fn chebyshev_dst((ax, ay): (usize, usize), (bx, by): (usize, usize)) -> usize { ax.abs_diff(bx).max(ay.abs_diff(by)) }

#[snippet(name = ";coord_compress", prefix = "use std::collections::HashMap;\nuse itertools::Itertools;")]
fn compress(v: &[usize]) -> Vec<usize> {
    let ranks: HashMap<_, _> = v.iter().sorted().dedup().zip(1..).collect();
    v.iter().map(|x| ranks[x]).collect()
}

#[cfg(test)]
mod tests {
    use rstest::rstest;

    use super::*;

    #[rstest]
    #[test]
    #[case((0, 0), (0, 0), 0)]
    #[case((0, 0), (1, 0), 1)]
    #[case((0, 0), (0, 1), 1)]
    #[case((0, 0), (1, 1), 2)]
    #[case((0, 0), (2, 3), 5)]
    #[case((1, 2), (3, 4), 4)]
    fn test_manhattan_dst(#[case] a: (usize, usize), #[case] b: (usize, usize), #[case] expected: usize) {
        assert_eq!(manhattan_dst(a, b), expected);
    }

    #[rstest]
    #[test]
    #[case((0, 0), (0, 0), 0)]
    #[case((0, 0), (1, 0), 1)]
    #[case((0, 0), (0, 1), 1)]
    #[case((0, 0), (1, 1), 1)]
    #[case((0, 0), (2, 3), 3)]
    #[case((1, 2), (3, 4), 2)]
    fn test_chebyshev_dst(#[case] a: (usize, usize), #[case] b: (usize, usize), #[case] expected: usize) {
        assert_eq!(chebyshev_dst(a, b), expected);
    }

    #[rstest]
    #[test]
    #[case(&[1, 2, 3, 4, 5], vec![1, 2, 3, 4, 5])]
    #[case(&[1, 1, 1, 1, 1], vec![1, 1, 1, 1, 1])]
    #[case(&[1, 2, 3, 4, 5, 1, 2, 3, 4, 5], vec![1, 2, 3, 4, 5, 1, 2, 3, 4, 5])]
    #[case(&[1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6], vec![1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6])]
    #[case(&[1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6], vec![1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6])]
    #[case(&[1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6], vec![1, 2, 3, 4, 5, 1, 2, 3, 4, 5, 6])]
    fn test_compress(#[case] v: &[usize], #[case] expected: Vec<usize>) {
        assert_eq!(compress(v), expected);
    }
}
