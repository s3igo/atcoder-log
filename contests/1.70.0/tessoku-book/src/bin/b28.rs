use proconio::input;

fn main() {
    input!(n: usize);
    let div = 1_000_000_007;

    let (_, ans) = (3..=n).fold((1, 1), |(prev, cur), _| (cur, (prev + cur) % div));

    println!("{ans}");
}
