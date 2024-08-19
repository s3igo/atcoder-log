use proconio::input;

fn main() {
    input!(h: usize, w: usize, r: usize, c: usize);

    let ans = [(r, 1), (r, h), (c, 1), (c, w)].iter().filter(|(a, b)| a != b).count();

    println!("{}", ans);
}
