use std::cmp::Reverse;

use itertools::{izip, Itertools};
use proconio::input;

fn main() {
    input!(n: usize, x: usize, y: usize, z: usize, a: [usize; n], b: [usize; n]);

    let mut students = izip!(1.., a, b).collect_vec();

    students.sort_by_key(|&(i, a, _)| (Reverse(a), i));
    students[x..].sort_by_key(|&(i, _, b)| (Reverse(b), i));
    students[x + y..].sort_by_key(|&(i, a, b)| (Reverse(a + b), i));

    let ans = students.iter().take(x + y + z).map(|(i, ..)| i).sorted().collect_vec();

    println!("{}", ans.iter().join("\n"));
}
