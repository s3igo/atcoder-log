use proconio::input;

fn main() {
    input!(n: isize);

    let part = 2_isize.pow(31);
    let cond = (-part..part).contains(&n);

    println!("{}", if cond { "Yes" } else { "No" });
}
