use proconio::input;

fn main() {
    input!(s1: String, s2: String);

    let cond = !(s1 == "#." && s2 == ".#" || s1 == ".#" && s2 == "#.");

    println!("{}", if cond { "Yes" } else { "No" });
}
