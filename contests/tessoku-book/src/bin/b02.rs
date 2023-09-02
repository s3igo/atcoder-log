use proconio::input;

fn main() {
    input! {
        a: usize,
        b: usize,
    }

    let cond = (a..=b).any(|n| 100 % n == 0);

    println!("{}", if cond { "Yes" } else { "No" });
}
