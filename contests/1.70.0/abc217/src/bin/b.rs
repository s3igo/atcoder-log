use proconio::input;

fn main() {
    input!(s: [String; 3]);

    let cases = ["ABC", "ARC", "AGC", "AHC"];
    let ans = cases.iter().find(|c| !s.contains(&c.to_string())).unwrap();

    println!("{ans}");
}
