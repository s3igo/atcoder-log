use itertools::Itertools;
use proconio::input;

fn main() {
    input!(mut n: usize);

    let mut nums = vec![];
    while n > 0 {
        nums.push(n % 5);
        n /= 5;
    }

    let digits: Vec<_> = nums.iter().map(|x| x * 2).collect();

    let digits: Vec<_> = digits
        .iter()
        .scan(2, |acc, i| match i.checked_sub(*acc) {
            Some(n) => {
                *acc = 0;
                Some(n)
            },
            None => {
                *acc = 2;
                Some(8)
            },
        })
        .collect();

    let ans = digits.iter().rev().join("");

    println!("{ans}");
}
