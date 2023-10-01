use proconio::{input, marker::Usize1};

fn main() {
    input! {
        n: usize,
        q: usize,
    }

    let mut a = vec![];
    for _ in 0..n {
        input! {
            l: usize,
            a_in: [usize; l],
        }
        a.push(a_in);
    }

    for _ in 0..q {
        input! {
            s: Usize1,
            t: Usize1,
        }
        println!("{}", a[s][t]);
    }
}
