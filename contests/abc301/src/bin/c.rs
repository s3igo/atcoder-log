use std::cmp::Ordering;

use maplit::hashmap;
use proconio::input;

fn main() {
    input! {
        s: String,
        t: String,
    }

    let (mut s_dict, mut t_dict) = (hashmap! { '@' => 0 }, hashmap! { '@' => 0 });
    for (s, t) in s.chars().zip(t.chars()) {
        s_dict.entry(s).and_modify(|cnt| *cnt += 1).or_insert(1);
        t_dict.entry(t).and_modify(|cnt| *cnt += 1).or_insert(1);
    }

    for c in "atcoder".to_string().chars() {
        let (s, t) = (*s_dict.get(&c).unwrap_or(&0), *t_dict.get(&c).unwrap_or(&0));
        match s.cmp(&t) {
            Ordering::Equal => continue,
            Ordering::Greater => {
                *t_dict.get_mut(&'@').unwrap() -= s - t;
                *t_dict.entry(c).or_insert(s) = s;
            },
            Ordering::Less => {
                *s_dict.get_mut(&'@').unwrap() -= t - s;
                *s_dict.entry(c).or_insert(t) = t;
            },
        }
        if s_dict.get(&'@').unwrap() < &0 || t_dict.get(&'@').unwrap() < &0 {
            println!("No");
            return;
        }
    }

    println!("{}", if s_dict == t_dict { "Yes" } else { "No" });
}
