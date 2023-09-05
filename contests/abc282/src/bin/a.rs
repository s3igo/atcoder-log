use proconio::input;

fn main() {
    input!(k: u8);

    let ans = (b'A'..b'A' + k).map(|c| c as char).collect::<String>();

    println!("{}", ans);
}
