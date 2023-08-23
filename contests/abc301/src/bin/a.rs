use proconio::input;

fn main() {
    input! {
        _: usize,
        s: String,
    }

    let (t, a) = s.chars().fold((0, 0), |(t, a), c| match c {
        'T' => (t + 1, a),
        'A' => (t, a + 1),
        _ => unreachable!(),
    });
    #[allow(clippy::comparison_chain)]
    let ans = if t == a {
        match s.chars().rev().next().unwrap() {
            'T' => 'A',
            'A' => 'T',
            _ => unreachable!(),
        }
    } else if t > a {
        'T'
    } else {
        'A'
    };

    println!("{}", ans);
}
