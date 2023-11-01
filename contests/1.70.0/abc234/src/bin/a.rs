use proconio::input;

fn main() {
    input!(t: usize);

    let f = |x| x * x + 2 * x + 3;
    let ans = f(f(f(t) + t) + f(f(t)));

    println!("{ans}");
}
