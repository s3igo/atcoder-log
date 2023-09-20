use proconio::input;

fn main() {
    input!(s: String);
    let week = vec!["Friday", "Thursday", "Wednesday", "Tuesday", "Monday"];

    let ans = week.iter().position(|&x| x == s).unwrap() + 1;

    println!("{}", ans);
}
