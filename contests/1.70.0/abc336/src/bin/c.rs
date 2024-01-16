use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize);

    let ans: Vec<_> = to_radix(n - 1, 5).iter().map(|x| x * 2).collect();

    println!("{}", ans.iter().join(""));
}

fn to_radix(mut n: usize, radix: usize) -> Vec<usize> {
    if n == 0 {
        return vec![0];
    }
    let mut digits = vec![];
    while n != 0 {
        digits.push(n % radix);
        n /= radix;
    }
    digits.into_iter().rev().collect()
}
