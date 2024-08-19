use proconio::input;

fn main() {
    input!(a: String, b: String);

    let (a, b) = (format!("{a:0>18}"), format!("{b:0>18}"));

    let cond = a.chars().zip(b.chars()).all(|(a, b)| a.to_digit(10).unwrap() + b.to_digit(10).unwrap() < 10);

    println!("{}", if cond { "Easy" } else { "Hard" });
}
