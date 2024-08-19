use proconio::input;

fn main() {
    input! {
        s: String,
        t: String,
    }

    let cond = t.starts_with(&s);

    println!("{}", if cond { "Yes" } else { "No" });
}
