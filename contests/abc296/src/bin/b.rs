use proconio::input;

fn main() {
    input!(s: [String; 8]);

    let (row, a) = s.iter().rev().zip(1..).find(|(e, _)| e.contains('*')).unwrap();
    let b = row
        .chars()
        .zip(b'a'..)
        .find_map(|(c, idx)| {
            if c == '*' {
                Some(char::from(idx))
            } else {
                None
            }
        })
        .unwrap();

    println!("{}{}", b, a);
}
