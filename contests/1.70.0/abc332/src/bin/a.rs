use proconio::input;

fn main() {
    input!(n: usize, s: usize, k: usize, pq: [(usize, usize); n]);

    let ans = match pq.iter().map(|&(p, q)| p * q).sum::<usize>() {
        sum if sum >= s => sum,
        sum => sum + k,
    };

    println!("{ans}");
}
