use itertools::*;
use proconio::input;

fn main() {
    input!(n: usize);
    let mut data = vec![(0, 0, vec![]); n];
    for (i, datum) in data.iter_mut().enumerate() {
        input! {
            ci: usize,
            ai: [usize; ci],
        }
        // (index, count, array)
        *datum = (i + 1, ci, ai);
    }
    input!(x: usize);

    data.retain(|(_, _, a)| a.contains(&x));
    let c_min = data.iter().map(|(_, c, _)| *c).min().unwrap_or(0);
    data.retain(|(_, c, _)| *c == c_min);

    println!("{}", data.len());
    println!("{}", data.iter().map(|(i, _, _)| i).join(" "));
}
