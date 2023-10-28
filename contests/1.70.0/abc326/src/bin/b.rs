use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize);

    let ans = (n..)
        .find(|i| {
            let s = i.to_string().chars().collect_vec();
            s[0].to_digit(10).unwrap() * s[1].to_digit(10).unwrap() == s[2].to_digit(10).unwrap()
        })
        .unwrap();

    println!("{ans}");
}
