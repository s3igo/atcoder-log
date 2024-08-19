use proconio::input;

fn main() {
    input!(_: [usize; 2], s: String, t: String);

    let ans = if t.starts_with(&s) {
        if t.ends_with(&s) {
            0
        } else {
            1
        }
    } else if t.ends_with(&s) {
        2
    } else {
        3
    };

    println!("{}", ans);
}
