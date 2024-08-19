use proconio::input;

fn main() {
    input!(p: [u8; 26]);

    let ans: String = p.iter().map(|&i| (b'a' + i - 1) as char).collect();

    println!("{ans}");
}
