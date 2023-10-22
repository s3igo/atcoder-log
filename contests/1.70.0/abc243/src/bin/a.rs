use std::ops::ControlFlow;

use proconio::input;

fn main() {
    input!(v: isize, abc: [isize; 3]);

    let result = abc.iter().cycle().try_fold((v, 0), |(rest, cnt), x| {
        if rest < 0 {
            ControlFlow::Break(cnt)
        } else {
            ControlFlow::Continue((rest - x, cnt + 1))
        }
    });

    let ans = match result {
        ControlFlow::Break(cnt) if cnt % 3 == 1 => 'F',
        ControlFlow::Break(cnt) if cnt % 3 == 2 => 'M',
        ControlFlow::Break(cnt) if cnt % 3 == 0 => 'T',
        _ => unreachable!(),
    };

    println!("{ans}");
}
