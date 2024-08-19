use proconio::input;

fn main() {
    input!(n: usize);

    println!("{}", f(n));
}

fn f(x: usize) -> usize {
    match x {
        0 => 1,
        _ => x * f(x - 1),
    }
}
