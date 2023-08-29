use proconio::input;

fn main() {
    input!(s: String);

    let ans = s.chars().position(|c| c.is_uppercase()).unwrap() + 1;

    println!("{}", ans);
}
