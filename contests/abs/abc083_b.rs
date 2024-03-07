use proconio::input;

fn main() {
    input!(n: u32, a: u32, b: u32);

    let ans = (1..=n)
        .filter(|i| (a..=b).contains(&i.to_string().chars().map(|c| c.to_digit(10).unwrap()).sum()))
        .map(|i| i as usize)
        .sum::<usize>();

    println!("{ans}");
}
