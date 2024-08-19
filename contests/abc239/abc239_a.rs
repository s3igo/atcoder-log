use proconio::input;

fn main() {
    input!(h: usize);

    let ans = ((h * (12_800_000 + h)) as f64).sqrt();

    println!("{ans}");
}
