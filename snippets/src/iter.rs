use cargo_snippet::snippet;
use itertools::Itertools;

#[snippet(name = ";iter_split-adjacent", prefix = "use itertools::Itertools;")]
fn split_adjacent<I, T>(iter: I) -> Vec<Vec<T>>
where
    I: Iterator<Item = T>,
    T: PartialEq + Copy,
{
    iter.group_by(|&s| s).into_iter().map(|(_, group)| group.collect()).collect()
}

#[test]
fn test_split_adjacent() {
    assert_eq!(
        split_adjacent([1, 1, 2, 2, 2, 3, 3, 3, 3].into_iter()),
        vec![vec![1, 1], vec![2, 2, 2], vec![3, 3, 3, 3]]
    );
    assert_eq!(split_adjacent([1, 2, 3].into_iter()), vec![vec![1], vec![2], vec![3]]);
    assert_eq!(split_adjacent([1, 1, 1].into_iter()), vec![vec![1, 1, 1]]);
    assert_eq!(split_adjacent("foo".chars()), vec![vec!['f'], vec!['o', 'o']]);
}
