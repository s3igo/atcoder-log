use proconio::input;

fn main() {
    input!(a: usize, b: usize);

    let cond = matches!(a.abs_diff(b), 1 | 9);

    println!("{}", if cond { "Yes" } else { "No" });
}
