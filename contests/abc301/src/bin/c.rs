use std::collections::HashMap;

use proconio::input;

fn main() {
    input! {
        s: String,
        t: String,
    }

    let mut s_dict = HashMap::new();
    s_dict.insert('@', 0);
    for c in s.chars() {
        s_dict.entry(c).and_modify(|cnt| *cnt += 1).or_insert(1);
    }
    let mut t_dict = HashMap::new();
    t_dict.insert('@', 0);
    for c in t.chars() {
        t_dict.entry(c).and_modify(|cnt| *cnt += 1).or_insert(1);
    }

    for c in "atcoder".to_string().chars() {
        let (s, t) = (*s_dict.get(&c).unwrap_or(&0), *t_dict.get(&c).unwrap_or(&0));
        match s {
            _ if s > t => {
                *t_dict.get_mut(&'@').unwrap() -= s - t;
                *t_dict.entry(c).or_insert(s) = s;
            },
            _ if s < t => {
                *s_dict.get_mut(&'@').unwrap() -= t - s;
                *s_dict.entry(c).or_insert(t) = t;
            },
            _ => continue,
        }
        if s_dict.get(&'@').unwrap() < &0 || t_dict.get(&'@').unwrap() < &0 {
            println!("No");
            return;
        }
    }

    let cond = s_dict == t_dict;
    println!("{}", if cond { "Yes" } else { "No" });
}
