use proconio::input;

fn main() {
    input!(s: String);

    let t = "oxx".to_string().chars().cycle().take(20).collect::<String>();
    let cond = t.contains(&s);

    println!("{}", if cond { "Yes" } else { "No" });
}
