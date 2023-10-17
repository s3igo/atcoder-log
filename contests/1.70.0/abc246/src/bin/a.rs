use proconio::input;

fn main() {
    input!(xy: [(isize, isize); 3]);

    let (x, y) = xy.into_iter().reduce(|(acc_x, acc_y), (x, y)| (acc_x ^ x, acc_y ^ y)).unwrap();

    println!("{} {}", x, y);
}
