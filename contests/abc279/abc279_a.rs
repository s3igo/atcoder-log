use proconio::input;

fn main() {
    input!(s: String);

    let ans = s.chars().fold(0, |acc, c| match c {
        'v' => acc + 1,
        'w' => acc + 2,
        _ => unreachable!(),
    });

    println!("{}", ans);
}
