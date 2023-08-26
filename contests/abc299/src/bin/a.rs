use proconio::input;

fn main() {
    input! {
        _: usize,
        s: String
    }

    let cond =
        s.chars().skip_while(|&e| e != '|').skip(1).take_while(|&e| e != '|').any(|e| e == '*');

    println!("{}", if cond { "in" } else { "out" });
}
