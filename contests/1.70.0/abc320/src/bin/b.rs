use proconio::{input, marker::Chars};

fn main() {
    input!(s: Chars);

    for i in (0..=s.len()).rev() {
        for s in s.windows(i) {
            if s.iter().eq(s.iter().rev()) {
                println!("{}", s.len());
                return;
            }
        }
    }
}
