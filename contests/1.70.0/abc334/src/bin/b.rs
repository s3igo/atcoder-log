use proconio::input;

fn main() {
    input!(a: isize, m: usize, l: isize, r: isize);

    // let ans = if a.abs_diff(l) % m == 0 || a.abs_diff(r) % m == 0 {
    //     (l.abs_diff(r) + m - 1) / m
    // } else {
    //     l.abs_diff(r) / m
    // };

    let offset = if a.abs_diff(l) % m == 0 { 0 } else { m - a.abs_diff(l) % m };
    let ans = (l.abs_diff(r) + m - offset - 1) / m;

    println!("{ans}");
}
