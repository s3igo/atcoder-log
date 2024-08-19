use proconio::input;

fn main() {
    input!(n: usize, a: [i64; n]);

    let ans = a.iter().fold(0, |acc, a| 0.max(acc + a));

    println!("{ans}");
}
