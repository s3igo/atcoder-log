use proconio::input;

fn main() {
    input!(s1: String, s2: String, s3: String, t: String);

    let ans: String = t
        .chars()
        .map(|c| match c {
            '1' => s1.clone(),
            '2' => s2.clone(),
            '3' => s3.clone(),
            _ => unreachable!(),
        })
        .collect();

    println!("{ans}");
}
