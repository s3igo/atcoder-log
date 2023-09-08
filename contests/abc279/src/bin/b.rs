use proconio::input;

fn main() {
    input! {
        s: String,
        t: String,
    }

    let cond = s.matches(&t).count() >= 1;

    println!("{}", if cond { "Yes" } else { "No" });
}
