use hashbag::HashBag;
use proconio::input;

fn main() {
    input!(n: usize, st: [(String, String); n]);

    let bag = st.iter().flat_map(|(s, t)| [s, t]).collect::<HashBag<_>>();
    let cond = st.iter().all(|(s, t)| {
        let mut bag = bag.clone();
        bag.remove(s);
        bag.remove(t);
        bag.get(s).is_none() || bag.get(t).is_none()
    });

    println!("{}", if cond { "Yes" } else { "No" });
}
