use proconio::input;

fn main() {
    input!(_: usize, s: String,);

    let ans = match s.find("ABC") {
        Some(i) => (i + 1).to_string(),
        None => "-1".to_string(),
    };

    println!("{}", ans);
}
