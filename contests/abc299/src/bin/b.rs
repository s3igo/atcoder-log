use itertools::{izip, Itertools};
use proconio::input;

fn main() {
    input! {
        n: usize,
        t: usize,
        c: [usize; n],
        r: [usize; n],
    }

    let data = izip!(1..=n, c.iter(), r.iter()).collect_vec();
    let cmp = if c.contains(&t) { t } else { c[0] };
    let (ans, ..) =
        data.iter().filter(|(_, &c, _)| c == cmp).max_by(|(.., rl), (.., rr)| rl.cmp(rr)).unwrap();

    println!("{}", ans);
}
