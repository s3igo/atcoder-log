use proconio::input;

fn main() {
    input!(a: usize, b: usize, c: usize, x: usize);

    let ans = match x {
        x if x <= a => 1.0,
        x if x <= b => c as f64 / (b - a) as f64,
        _ => 0.0,
    };

    println!("{ans}");
}
