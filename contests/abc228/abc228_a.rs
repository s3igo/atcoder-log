use proconio::input;

fn main() {
    input!(s: usize, t: usize, x: usize);

    let cond = if s < t { (s..t).contains(&x) } else { s <= x || x < t };

    println!("{}", if cond { "Yes" } else { "No" });
}
