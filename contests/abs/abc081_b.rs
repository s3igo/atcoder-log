use num::Integer;
use proconio::input;

fn main() {
    input!(n: usize, mut a: [usize; n]);

    let mut cnt = 0;
    while a.iter().all(|x| x.is_even()) {
        a = a.iter().map(|&x| x / 2).collect();
        cnt += 1;
    }

    println!("{cnt}");
}
