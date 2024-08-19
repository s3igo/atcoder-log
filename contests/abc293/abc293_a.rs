use proconio::{input, marker::Chars};

fn main() {
    input!(s: Chars);

    let ans = s.chunks(2).flat_map(|e| vec![e[1], e[0]]).collect::<String>();

    println!("{}", ans);
}
