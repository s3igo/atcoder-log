use proconio::input;

fn main() {
    input!(s: String);

    let ans = s.chars().cycle().take(6).collect::<String>();

    println!("{}", ans);
}
