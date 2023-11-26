use cargo_snippet::snippet;
use itertools::Itertools;

#[snippet(name = "string_split-adjacent")]
fn split_adjacent<T: PartialEq>(v: &[T]) -> Vec<Vec<&T>> {
    v.iter().group_by(|&s| s).into_iter().map(|(_, group)| group.collect()).collect()
}

#[test]
fn test_split_adjacent() {
    assert_eq!(
        split_adjacent(&[1, 1, 2, 2, 2, 3, 3, 3, 3]),
        vec![vec![&1, &1], vec![&2, &2, &2], vec![&3, &3, &3, &3]]
    );
    assert_eq!(split_adjacent(&[1, 2, 3]), vec![vec![&1], vec![&2], vec![&3]]);
    assert_eq!(split_adjacent(&[1, 1, 1]), vec![vec![&1, &1, &1]]);
    assert_eq!(
        split_adjacent("foo".chars().collect::<Vec<_>>().as_slice()),
        vec![vec![&'f'], vec![&'o', &'o']]
    );
}
