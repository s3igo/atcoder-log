use proconio::input;

fn main() {
    input!(n: usize, s: String);

    let cond = s.chars().nth(n - 1).unwrap() == 'o';

    println!("{}", if cond { "Yes" } else { "No" });
}
