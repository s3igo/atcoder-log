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

#[cfg(test)]
mod tests {
    use std::fmt::Debug;

    use rstest::rstest;

    use super::*;

    #[rstest]
    #[test]
    #[case([1, 1, 2, 2, 2, 3, 3, 3, 3].into_iter(), vec![vec![1, 1], vec![2, 2, 2], vec![3, 3, 3, 3]])]
    #[case([1, 2, 3].into_iter(), vec![vec![1], vec![2], vec![3]])]
    #[case([1, 1, 1].into_iter(), vec![vec![1, 1, 1]])]
    #[case("foo".chars(), vec![vec!['f'], vec!['o', 'o']])]
    fn test_split_adjacent<I, T>(#[case] iter: I, #[case] expected: Vec<Vec<T>>)
    where
        I: Iterator<Item = T>,
        T: PartialEq + Copy + Debug,
    {
        assert_eq!(split_adjacent(iter), expected);
    }
}
