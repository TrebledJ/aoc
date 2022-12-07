use std::fs;

type RPS = i32;

fn main() {
    let filename = "../input/d02.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let nums = parse(contents);
    println!("part1: {}", part1(&nums));
    println!("part2: {}", part2(&nums));
}

fn parse(contents: String) -> Vec<(RPS, RPS)> {
    contents
        .lines()
        .map(|s| {
            let v = s
                .split(' ')
                .map(|c| rps_from_str(c).unwrap())
                .collect::<Vec<_>>();
            (v[0], v[1])
        })
        .collect::<Vec<_>>()
}

fn part1(rps: &Vec<(RPS, RPS)>) -> i32 {
    rps.iter().map(|(a, b)| score(*a, *b)).sum()
}

fn part2(rps: &Vec<(RPS, RPS)>) -> i32 {
    rps.iter().map(|(a, b)| score(*a, predict(*a, *b))).sum()
}

fn rps_from_str(s: &str) -> Result<i32, String> {
    match s {
        "A" | "X" => Ok(0),
        "B" | "Y" => Ok(1),
        "C" | "Z" => Ok(2),
        _ => Err(String::from(s)),
    }
}

fn score(a: RPS, b: RPS) -> i32 {
    let choice = b + 1;
    let outcome = 3 * ((b - a + 4) % 3);
    choice + outcome
}

fn predict(a: RPS, outcome: i32) -> i32 {
    (a + (outcome - 1) + 3) % 3
}
