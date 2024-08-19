use proconio::input;

fn main() {
    input!(v: usize, a: usize, b: usize, c: usize);

    let ans = match v % (a + b + c) {
        r if r < a => 'F',
        r if r < a + b => 'M',
        r if r < a + b + c => 'T',
        _ => unreachable!(),
    };

    println!("{ans}");
}
