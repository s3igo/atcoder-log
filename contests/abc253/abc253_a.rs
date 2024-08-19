use proconio::input;

fn main() {
    input!(a: usize, b: usize, c: usize);
    let cond = (a.min(c)..=a.max(c)).contains(&b);

    println!("{}", if cond { "Yes" } else { "No" });
}
