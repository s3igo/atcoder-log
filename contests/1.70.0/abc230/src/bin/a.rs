use proconio::input;

fn main() {
    input!(n: usize);

    let ans = if n >= 42 { n + 1 } else { n };

    println!("AGC{ans:03}");
}
