use proconio::input;

fn main() {
    input!(q: usize, query: [(usize, usize); q]);

    let mut a = vec![];
    for (idx, n) in query {
        match idx {
            1 => a.push(n),
            2 => println!("{}", a[a.len() - n]),
            _ => unreachable!(),
        }
    }
}
