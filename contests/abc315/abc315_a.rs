use proconio::input;

fn main() {
    input!(mut s: String);
    let cases = vec!['a', 'e', 'i', 'o', 'u'];

    s.retain(|c| !cases.contains(&c));
    println!("{}", s);
}
