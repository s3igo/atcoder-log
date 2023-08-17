use proconio::input;

fn main() {
    input! {
        n: usize,
        mut a: [usize; n],
    }

    a.sort();
    a.reverse();
    let mut a_sum = 0_usize;
    let mut b_sum = 0_usize;
    for (i, &x) in a.iter().enumerate() {
        if i % 2 == 0 {
            a_sum += x;
        } else {
            b_sum += x;
        }
    }

    println!("{}", a_sum - b_sum);
}
