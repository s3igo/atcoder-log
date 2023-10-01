use proconio::input;

fn main() {
    input!(n: isize);
    let divisor = 998244353;

    let ans = match n % divisor {
        x if x < 0 => x + divisor,
        x => x,
    };

    println!("{}", ans);
}
