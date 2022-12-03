use std::fs;
use std::collections::HashSet;

fn main() {
    let filename = "../input/d03.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let nums = parse(contents);
    println!("part1: {}", part1(&nums));
    println!("part2: {}", part2(&nums));
}

fn parse(contents: String) -> Vec<String> {
    contents
            .lines()
            .map(String::from)
            .collect::<Vec<_>>()
}

fn prio(c: char) -> i32 {
    if c.is_ascii_uppercase() {
       (c as i32) - 0x40 + 26
    } else {
        (c as i32) - 0x60
    }
}

fn part1(xs: &Vec<String>) -> i32 {
    fn common(a: String, b: String) -> char {
        let seta: HashSet<char> = a.chars().collect();
        let setb: HashSet<char> = b.chars().collect();
        *seta.intersection(&setb).into_iter().next().unwrap()
    }
    xs.iter().map(|s| prio(common(s.chars().take(s.len() / 2).collect(), s.chars().skip(s.len() / 2).collect()))).sum()
}

fn part2(xs: &Vec<String>) -> i32 {
    let mut sum = 0;
    for i in (0..xs.len()).step_by(3) {
        let seta: HashSet<char> = xs[i + 0].chars().collect();
        let setb: HashSet<char> = xs[i + 1].chars().collect();
        let setc: HashSet<char> = xs[i + 2].chars().collect();
        let c = *seta.intersection(&setb).copied().collect::<HashSet<_>>().intersection(&setc).into_iter().next().unwrap();
        sum += prio(c);
    }
    sum
}
