use proconio::input;

fn main() {
    input! {
        n: usize,
        s: [String; n],
    }

    let (f, a) = s.iter().fold((0, 0), |(f, a), e| match e.as_str() {
        "For" => (f + 1, a),
        "Against" => (f, a + 1),
        _ => unreachable!(),
    });

    println!("{}", if f > a { "Yes" } else { "No" });
}
