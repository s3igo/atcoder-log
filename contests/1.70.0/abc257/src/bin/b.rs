use itertools::Itertools;
use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, k: usize, q: usize, a: [Usize1; k], l: [usize; q]);

    let mut row = vec![0; n];
    for (i, &a) in a.iter().enumerate() {
        row[a] = i + 1;
    }

    for i in l {
        match row.iter().tuple_windows().find_position(|(&l, _)| l == i) {
            Some((idx, (_, &r))) if r == 0 => {
                row[idx + 1] = i;
                row[idx] = 0;
            },
            Some(_) | None => continue,
        };
    }

    let ans = row.iter().positions(|&e| e != 0).map(|e| e + 1).collect_vec();

    println!("{}", ans.iter().join(" "));
}
