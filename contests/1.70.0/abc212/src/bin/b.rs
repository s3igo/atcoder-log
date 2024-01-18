use itertools::Itertools;
use proconio::input;

fn main() {
    input!(x: String);

    let is_weak = x.chars().all_equal()
        || x.chars().map(|c| c.to_digit(10).unwrap()).tuple_windows().all(|(l, r)| (l + 1) % 10 == r);

    println!("{}", if is_weak { "Weak" } else { "Strong" });
}
