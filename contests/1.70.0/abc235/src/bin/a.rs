use itertools::Itertools;
use proconio::input;

fn main() {
    input!(abc: String);

    let [a, b, c] = abc.chars().filter_map(|c| c.to_digit(10)).collect_vec()[..] else { unreachable!() };

    let to_num = |a, b, c| a * 100 + b * 10 + c;
    let ans = to_num(a, b, c) + to_num(b, c, a) + to_num(c, a, b);

    println!("{ans}");
}
