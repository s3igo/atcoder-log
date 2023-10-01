use proconio::input;

fn main() {
    input! {
        n: usize,
        mut a: [usize; n],
    }

    let mut ans = 0;
    loop {
        if a.iter().all(|&x| x % 2 == 0) {
            ans += 1;
            for x in a.iter_mut() {
                *x /= 2;
            }
        } else {
            break;
        }
    }

    println!("{}", ans);
}
