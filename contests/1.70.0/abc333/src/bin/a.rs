use proconio::input;

fn main() {
    input!(n: usize);

    let ans = n.to_string().repeat(n);

    println!("{ans}");
}
