use std::collections::HashMap;

use itertools::Itertools;
use proconio::input;

fn main() {
    input! {
        s: String,
        t: String,
    }

    let mut s_dict = HashMap::new();
    for c in s.chars() {
        s_dict.entry(c).and_modify(|cnt| *cnt += 1).or_insert(1);
    }
    let mut t_dict = HashMap::new();
    for c in t.chars() {
        t_dict.entry(c).and_modify(|cnt| *cnt += 1).or_insert(1);
    }

    for c in "atcoder".to_string().chars() {
        let max = s_dict.get(&c).unwrap_or(&0).max(t_dict.get(&c).unwrap_or(&0));
        if s_dict.get(&'@').unwrap_or(&0) < max - s_dict.get(&c).unwrap_or(&0)
            || t_dict.get(&'@').unwrap_or(&0) < max - t_dict.get(&c).unwrap_or(&0)
        {
            println!("No");
            return;
        }
    }
}
