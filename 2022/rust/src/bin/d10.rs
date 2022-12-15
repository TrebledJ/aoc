use itertools::Itertools;
use std::fs;
use std::str::FromStr;

struct State {
    x: i32,
    cycle: u32,
}

#[derive(Debug, Clone, Copy)]
enum Inst {
    Noop,
    Addx(i32),
}

impl FromStr for Inst {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut splat = s.split_whitespace();
        match splat.next().unwrap() {
            "noop" => Ok(Inst::Noop),
            "addx" => Ok(Inst::Addx(splat.next().unwrap().parse::<i32>().unwrap())),
            _ => Err(()),
        }
    }
}

fn main() {
    let filename = "../input/d10.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents);
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

fn parse(contents: String) -> Vec<Inst> {
    contents.lines().map(|s| s.parse().unwrap()).collect()
}

fn part1(xs: &Vec<Inst>) -> i32 {
    xs.iter()
        .fold(
            (State { x: 1, cycle: 0 }, 0),
            |(State { x, cycle }, signal), inst| {
                let (x2, cycle2) = match inst {
                    Inst::Noop => (x, cycle + 1),
                    Inst::Addx(v) => (x + *v, cycle + 2),
                };
                (
                    State {
                        x: x2,
                        cycle: cycle2,
                    },
                    if (cycle + 20) / 40 != (cycle2 + 20) / 40 {
                        signal + (cycle2 - (cycle2 + 20) % 40) as i32 * x
                    } else {
                        signal
                    },
                )
            },
        )
        .1
}

fn part2(xs: &Vec<Inst>) -> String {
    fn push(s: String, x: i32) -> String {
        let i = (s.len() % 40) as i32;
        if x <= i && i <= x + 2 {
            s + "#"
        } else {
            s + "."
        }
    }

    xs.iter()
        .fold(
            (String::from(""), State { x: 0, cycle: 0 }),
            |(acc_str, State { x, cycle }), inst| match inst {
                Inst::Noop => (
                    push(acc_str, x),
                    State {
                        x,
                        cycle: cycle + 1,
                    },
                ),
                Inst::Addx(v) => (
                    push(push(acc_str, x), x),
                    State {
                        x: x + *v,
                        cycle: cycle + 2,
                    },
                ),
            },
        )
        .0
        .chars()
        .chunks(40)
        .into_iter()
        .map(|s| format!("\n{}", s.collect::<String>()))
        .collect()
}
