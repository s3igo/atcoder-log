use proconio::input;

fn main() {
    input!(n: usize, k: usize, a: [usize; n]);

    let (mut l, mut r, mut cnt) = (0, 0, 0);
    while l < n - 1 {
        while r < n - 1 && a[r + 1] - a[l] <= k {
            r += 1;
        }
        cnt += r - l;
        l += 1;
    }

    println!("{cnt}");
}
