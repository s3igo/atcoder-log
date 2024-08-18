use proconio::input;

fn main() {
    input!(n: usize, a: usize, t: [usize; n]);

    let mut prev = 0;
    for t in t {
        prev = prev.max(t) + a;
        println!("{prev}");
    }
}
