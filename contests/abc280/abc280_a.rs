use proconio::{input, marker::Chars};

fn main() {
    input! {
        h: usize,
        _: usize,
        s: [Chars; h],
    }

    let ans = s.iter().flatten().filter(|&&c| c == '#').count();

    println!("{}", ans);
}
