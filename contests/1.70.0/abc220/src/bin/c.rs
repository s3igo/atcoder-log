use itertools::Itertools;
use itertools_num::ItertoolsNum;

use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n], x: usize);

    let sum: usize = a.iter().sum();
    let cumsum: Vec<_> = a.iter().cumsum::<usize>().collect();

    let ans = x / sum * n + cumsum.iter().take_while_inclusive(|&&c| c <= x % sum).count();

    println!("{ans}");
}
