use proconio::input;

fn main() {
    input!(n: usize);

    let cond = !(2..5).contains(&n);

    println!("{}", if cond { "Yes" } else { "No" });
}
