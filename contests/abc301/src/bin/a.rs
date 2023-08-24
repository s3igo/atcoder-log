use std::cmp::Ordering;

use proconio::input;

fn main() {
    input! {
        _: usize,
        s: String,
    }

    let (t, a) = s.chars().fold((0, 0), |(t, a), c| match c {
        'T' => (t + 1, a),
        'A' => (t, a + 1),
        _ => unreachable!(),
    });
    let ans = match t.cmp(&a) {
        Ordering::Equal => match s.chars().last().unwrap() {
            'T' => 'A',
            'A' => 'T',
            _ => unreachable!(),
        },
        Ordering::Greater => 'T',
        Ordering::Less => 'A',
    };

    println!("{}", ans);
}
