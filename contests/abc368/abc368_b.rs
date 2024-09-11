use proconio::input;

fn main() {
    input!(n: usize, mut a: [usize; n]);

    let mut cnt = 0;
    loop {
        a.sort_unstable_by(|a, b| b.cmp(a));
        if a[0] == 0 || a[1] == 0 {
            break;
        }
        a[0] -= 1;
        a[1] -= 1;
        cnt += 1;
    }

    println!("{cnt}");
}
