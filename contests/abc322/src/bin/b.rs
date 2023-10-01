use proconio::input;

fn main() {
    input! {
        _: usize,
        _: usize,
        s: String,
        t: String,
    }

    let ans = if t.starts_with(&s) && t.ends_with(&s) {
        0
    } else if t.starts_with(&s) && !t.ends_with(&s) {
        1
    } else if t.ends_with(&s) && !t.starts_with(&s) {
        2
    } else {
        3
    };

    println!("{}", ans);
}
