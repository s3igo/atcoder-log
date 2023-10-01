use proconio::input;

fn main() {
    input!(s: String);

    let ans = match s.rfind('a') {
        Some(i) => i as isize + 1,
        None => -1,
    };

    println!("{}", ans);
}
