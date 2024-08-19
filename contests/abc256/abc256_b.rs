use itertools_num::ItertoolsNum;
use proconio::input;

fn main() {
    input!(n: usize, a: [usize; n]);

    let ans = a.iter().rev().cumsum::<usize>().skip_while(|&x| x < 4).count();

    println!("{}", ans);
}
