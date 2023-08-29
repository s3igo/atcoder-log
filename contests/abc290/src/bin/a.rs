use proconio::input;

fn main() {
    input! {
        n: usize,
        m: usize,
        a: [usize; n],
        b: [usize; m],
    }

    let ans = a.iter().zip(1..).filter(|(_, i)| b.contains(i)).map(|(e, _)| e).sum::<usize>();

    println!("{}", ans);
}
