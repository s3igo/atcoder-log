use num_integer::Integer;
use proconio::input;

fn main() {
    input!(a: isize, m: isize, l: isize, r: isize);

    let l = l - a;
    let r = r - a;

    let ans = Integer::div_floor(&r, &m) - Integer::div_floor(&(l - 1), &m);

    println!("{ans}");
}
