use proconio::input;

fn main() {
    input!(s: String);

    let ans = if s.ends_with('r') { "er" } else { "ist" };

    println!("{ans}");
}
