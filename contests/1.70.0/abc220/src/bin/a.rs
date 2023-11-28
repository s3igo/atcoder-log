use proconio::input;

fn main() {
    input!(a: usize, b: usize, c: usize);

    let ans = (a..=b).find(|x| x % c == 0).map_or("-1".to_string(), |x| x.to_string());

    println!("{ans}");
}
