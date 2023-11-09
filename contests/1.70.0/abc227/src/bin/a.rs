use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, k: usize, a: Usize1);

    let ans = (a + k - 1) % n + 1;

    println!("{ans}");
}
