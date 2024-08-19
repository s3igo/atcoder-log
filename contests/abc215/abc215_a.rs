use proconio::input;

fn main() {
    input!(s: String);

    let ans = if s == "Hello,World!" { "AC" } else { "WA" };

    println!("{ans}");
}
