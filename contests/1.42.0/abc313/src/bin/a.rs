use proconio::input;

fn main() {
    input! {
        n: i32,
        first: i32,
        mut p: [i32; n - 1],
    }

    p.sort();
    p.reverse();

    println!("{}", if first > p[0] { 0 } else { p[0] - first + 1 }); // RE
    println!("{}", (p[0] - first + 1).max(0)); // RE
}
