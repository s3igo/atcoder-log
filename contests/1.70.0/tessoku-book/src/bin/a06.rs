use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
    input!(n: usize, q: usize, a: [usize; n], lr: [(usize, usize); q]);

    let cumsum = std::iter::once(0).chain(a).cumsum().collect::<Vec<usize>>();

    for (l, r) in lr {
        println!("{}", cumsum[r] - cumsum[l - 1]);
    }
}
