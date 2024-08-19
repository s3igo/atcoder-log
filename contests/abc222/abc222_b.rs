use proconio::input;

fn main() {
    input!(n: usize, p: usize, a: [usize; n]);

    let ans = a.iter().filter(|&&x| x < p).count();

    println!("{}", ans);
}
