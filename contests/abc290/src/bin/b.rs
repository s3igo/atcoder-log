use proconio::input;

fn main() {
    input! {
        _: usize,
        k: usize,
        s: String,
    }

    let cnt = s.chars().filter(|&c| c == 'o').count();
    let ans = s
        .chars()
        .rev()
        .collect::<String>()
        .replacen('o', "x", cnt - k)
        .chars()
        .rev()
        .collect::<String>();

    println!("{}", ans);
}
