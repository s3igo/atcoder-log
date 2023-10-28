use proconio::input;

fn main() {
    input!(n: usize, m: usize, mut a: [usize; n]);

    if n == 1 {
        println!("1");
        return;
    }

    a.sort();
    let a = a;

    let mut ans = 0;
    let (mut l, mut r) = (0, 0);
    loop {
        if a.get(r).unwrap_or(&a[n - 1]) - a[l] < m {
            r += 1;
        } else {
            ans = ans.max(r - l);
            l += 1;
        }
        if r >= n - 1 && l >= r - 1 {
            break;
        }
    }

    println!("{ans}");
}
