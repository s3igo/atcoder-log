use num::Integer;
use proconio::input;

fn main() {
    input!(n: usize, mut a: [usize; n]);

    let mut cnt = 0;
    while a.iter().all(|x| x.is_even()) {
        a.iter_mut().for_each(|x| *x /= 2);
        cnt += 1;
    }

    println!("{cnt}");
}
