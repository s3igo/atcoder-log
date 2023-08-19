use proconio::input;

fn main() {
    input!(s: String);
    let cases = vec!["ACE", "BDF", "CEG", "DFA", "EGB", "FAC", "GBD"];

    println!("{}", if cases.contains(&s.as_str()) { "Yes" } else { "No" });
}
