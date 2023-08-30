use proconio::input;

fn main() {
    input!(s: String);

    let ans = s
        .chars()
        .map(|c| match c {
            '1' => '0',
            '0' => '1',
            _ => unreachable!(),
        })
        .collect::<String>();

    println!("{}", ans);
}
