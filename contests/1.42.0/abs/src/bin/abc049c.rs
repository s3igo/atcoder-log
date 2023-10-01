use proconio::input;

fn main() {
    input! {
        mut s: String,
    }
    let cases = vec!["dream", "dreamer", "erase", "eraser"];

    loop {
        if s.ends_with(cases[0]) {
            s = s[..s.len() - cases[0].len()].to_string();
        } else if s.ends_with(cases[1]) {
            s = s[..s.len() - cases[1].len()].to_string();
        } else if s.ends_with(cases[2]) {
            s = s[..s.len() - cases[2].len()].to_string();
        } else if s.ends_with(cases[3]) {
            s = s[..s.len() - cases[3].len()].to_string();
        } else {
            break;
        }
    }

    if s.is_empty() {
        println!("YES");
    } else {
        println!("NO");
    }
}
