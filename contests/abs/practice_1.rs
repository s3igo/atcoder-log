use proconio::input;

fn main() {
    input!(a: usize, b: usize, c: usize, s: String);

    println!("{} {s}", a + b + c);
}
