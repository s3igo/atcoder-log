use proconio::input;

fn main() {
    input! {
        m: usize,
        ds: [usize; m],
    }

    let center = ds.iter().sum::<usize>() / 2;
    let mut month = 0;
    let day = 1 + center
        - ds.iter()
            .enumerate()
            .take_while(|(i, d)| {
                month = *i + 1;
                center > ds[..*i].iter().sum::<usize>() + *d
            })
            .fold(0, |acc, (_, d)| acc + d);

    println!("{} {}", month, day); // NOTE: WA
}
