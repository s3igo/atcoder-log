use proconio::input;

fn main() {
    input!(a: usize, b: usize);

    println!("{}", gcd(a, b));
}

fn gcd(a: usize, b: usize) -> usize {
    match b {
        0 => a,
        _ => gcd(b, a % b),
    }
}
