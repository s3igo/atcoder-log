use proconio::{input, marker::Usize1};

fn main() {
    input! {
        l: Usize1,
        r: Usize1,
    }
    let str = "atcoder";

    println!("{}", &str[l..=r]);
}
