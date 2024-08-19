use proconio::input;

fn main() {
    input! {
        a: [usize; 64],
    }

    let ans = a.iter().enumerate().map(|(i, x)| x * 2_usize.pow(i as u32)).sum::<usize>();

    println!("{}", ans);
}
