use proconio::input;
use superslice::Ext;

fn main() {
    input! {
        n: usize,
        mut a: [usize; n],
        q: usize,
        x: [usize; q],
    }

    a.sort();
    let a = a;

    for x in x {
        println!("{}", a.lower_bound(&x));
    }
}
