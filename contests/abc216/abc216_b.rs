use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, st: [(String, String); n]);

    println!("{}", if st.iter().all_unique() { "No" } else { "Yes" });
}
