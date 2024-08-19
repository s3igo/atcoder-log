use proconio::input;

fn main() {
    input!(a: usize, b: usize);

    let d = ((a * a + b * b) as f64).sqrt();
    let (x, y) = (a as f64 / d, b as f64 / d);

    println!("{} {}", x, y);
}
