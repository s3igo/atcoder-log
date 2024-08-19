use hashbag::HashBag;
use proconio::input;

fn main() {
    input!(n: usize, m: usize, a: [usize; n], b: [usize; m]);

    let mut bag = a.iter().collect::<HashBag<_>>();
    let cond = b
        .iter()
        .try_for_each(|b| match bag.get(b) {
            Some(_) => {
                bag.remove(b);
                Ok(())
            },
            None => Err(()),
        })
        .is_ok();

    println!("{}", if cond { "Yes" } else { "No" });
}
