use std::fs;

fn main() {
    let filename = "../input/d03.txt";

    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let text = contents.lines().collect::<Vec<_>>();
    let blen = text[0].len() as u16;
    let xs = text
        .iter()
        .map(|s| u16::from_str_radix(s, 2).unwrap())
        .collect::<Vec<_>>();

    println!("part1: {}", part1(&xs, blen));
    println!("part2: {}", part2(&xs, blen));
}

fn part1(xs: &Vec<u16>, blen: u16) -> i32 {
    let n = xs.len() as u16;
    let gamma = (0..blen)
        .rev()
        .map(|c| {
            let ones = count_one(xs, c);
            (ones > n - ones) as i32
        })
        .fold(0, bin2int);
    let eps = (0..blen)
        .rev()
        .map(|c| !((gamma & (1 << c)) != 0) as i32)
        .fold(0, bin2int);
    gamma * eps
}

fn part2(xs: &Vec<u16>, blen: u16) -> i32 {
    let mut oxygen = xs.clone();
    let mut co2 = xs.clone();
    for c in (0..blen).rev() {
        let o_ones = count_one(oxygen.iter(), c);
        let co2_ones = count_one(co2.iter(), c);
        oxygen = oxygen
            .iter()
            .cloned()
            .filter(|i| ((*i & (1 << c)) != 0) == (o_ones >= (oxygen.len() as u16) - o_ones))
            .collect::<Vec<u16>>();
        if co2.len() > 1 {
            co2 = co2
                .iter()
                .cloned()
                .filter(|i| ((*i & (1 << c)) != 0) == (co2_ones < (co2.len() as u16) - co2_ones))
                .collect::<Vec<u16>>();
        }
    }
    (oxygen[0] as i32) * (co2[0] as i32)
}

fn count_one<'a>(xs: impl IntoIterator<Item = &'a u16>, col: u16) -> u16 {
    xs.into_iter()
        .map(|i| (i & (1 << col) != 0) as u16)
        .sum::<u16>()
}

fn bin2int(acc: i32, x: i32) -> i32 {
    2 * acc + x
}
