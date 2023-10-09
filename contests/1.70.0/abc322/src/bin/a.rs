use proconio::input;

fn main() {
    input!(_: usize, s: String,);

    let ans = s.find("ABC").map_or("-1".to_string(), |i| (i + 1).to_string());

    println!("{}", ans);
}
