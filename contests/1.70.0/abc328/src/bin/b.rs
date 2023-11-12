use itertools::Itertools;
use proconio::input;

fn main() {
    input!(n: usize, d: [usize; n]);

    let mut ans = 0;
    for (&d, i) in d.iter().zip(1..) {
        for j in 1..=d {
            if i.to_string().chars().all_equal()
                && j.to_string().chars().all_equal()
                && i.to_string().chars().next().unwrap() == j.to_string().chars().next().unwrap()
            {
                ans += 1;
            }
        }
    }

    println!("{ans}");
}
