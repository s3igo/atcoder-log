use itertools::Itertools;
use proconio::{input, marker::Chars};

fn main() {
    input!(_: usize, s: Chars);

    let ans = split_adjacent(&s).iter().sorted().rev().dedup_by(|l, r| l[0] == r[0]).map(|v| v.len()).sum::<usize>();

    println!("{ans}");
}

fn split_adjacent<T: PartialEq>(v: &[T]) -> Vec<Vec<&T>> {
    v.iter().group_by(|&s| s).into_iter().map(|(_, group)| group.collect()).collect()
}
