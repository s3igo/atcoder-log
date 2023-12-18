use proconio::input;

fn main() {
    input!(a: usize, b: usize);

    let ans = (0..).find(|&c| a ^ c == b).unwrap();

    println!("{ans}");
}
