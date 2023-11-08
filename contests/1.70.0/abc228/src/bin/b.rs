use proconio::{input, marker::Usize1};

fn main() {
    input!(n: usize, x: Usize1, a: [Usize1; n]);

    let ans = trace(&a, &mut vec![false; n], x, 0);

    println!("{ans}");
}

fn trace(a: &[usize], checked: &mut [bool], x: usize, cnt: usize) -> usize {
    if checked[x] {
        cnt
    } else {
        checked[x] = true;
        trace(a, checked, a[x], cnt + 1)
    }
}
