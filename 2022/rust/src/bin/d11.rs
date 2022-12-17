use std::fs;
use std::str::FromStr;

#[derive(Debug, Clone, Copy)]
enum Op {
    Add(u64),
    Mul(u64),
    Sqr,
}

#[derive(Debug, Clone, Copy)]
struct Decision {
    div: u64,
    iftrue: u64,
    iffalse: u64,
}

#[derive(Debug, Clone)]
struct Monkey {
    items: Vec<u64>,
    op: Op,
    test: Decision,
}

impl Monkey {
    pub fn push(&mut self, x: u64) {
        self.items.push(x);
    }

    pub fn send_to(&self, worry: u64) -> u64 {
        let Decision { div, iftrue, iffalse } = self.test;
        if worry % div == 0 { iftrue } else { iffalse }
    }

    fn operate(&self, x: u64) -> u64 {
        match self.op {
            Op::Add(v) => x + v,
            Op::Mul(v) => x * v,
            Op::Sqr => x * x,
        }
    }
}


impl FromStr for Monkey {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut ls = s.lines();
        ls.next();
        let (_, init_str) = ls.next().unwrap().split_once(": ").unwrap();
        let items = init_str
            .split(", ")
            .map(|s| s.parse::<u64>().unwrap())
            .collect::<Vec<_>>();

        let (_, op_str) = ls.next().unwrap().split_once("= ").unwrap();
        let op = match op_str {
            "old * old" => Op::Sqr,
            _ => {
                let res = op_str.split(" ").collect::<Vec<_>>();
                match res[1] {
                    "*" => Op::Mul(res[2].parse().unwrap()),
                    "+" => Op::Add(res[2].parse().unwrap()),
                    _ => panic!("unexpected op: {}", op_str),
                }
            }
        };

        let div = ls.next().unwrap().split_once("divisible by ").unwrap().1.parse::<u64>().unwrap();
        let iftrue = ls.next().unwrap().split_once("monkey ").unwrap().1.parse::<u64>().unwrap();
        let iffalse = ls.next().unwrap().split_once("monkey ").unwrap().1.parse::<u64>().unwrap();

        Ok(Monkey {
            items,
            op,
            test: Decision {
                div,
                iftrue,
                iffalse,
            },
        })
    }
}

fn main() {
    let filename = "../input/d11.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents);
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

fn parse(contents: String) -> Vec<Monkey> {
    contents.split("\n\n").map(|s| s.parse::<Monkey>().unwrap()).collect()
}

fn part1(xs: &Vec<Monkey>) -> i32 {
    let mut ms = xs.clone();
    let rounds = 20;
    let mut counts = vec![0; ms.len()];
    for _ in 0..rounds {
        for i in 0..ms.len() {
            counts[i] += ms[i].items.len() as i32;
            for worry in ms[i].items.clone() {
                let worry = ms[i].operate(worry) / 3;
                let target = ms[i].send_to(worry);
                ms[target as usize].push(worry);
            }
            ms[i].items.clear();
        }
    }

    counts.sort_by_key(|x| -x);
    counts.iter().take(2).product()
}

fn part2(xs: &Vec<Monkey>) -> i64 {
    let prod: u64 = xs.iter().map(|x| x.test.div).product();

    let mut ms = xs.clone();

    let rounds = 10000;
    let mut counts = vec![0; ms.len()];
    for _ in 0..rounds {
        for i in 0..ms.len() {
            counts[i] += ms[i].items.len() as i64;
            for worry in ms[i].items.clone() {
                let worry = ms[i].operate(worry) % prod;
                let target = ms[i].send_to(worry);
                ms[target as usize].push(worry);
            }
            ms[i].items.clear();
        }
    }

    counts.sort_by_key(|x| -x);
    counts.iter().take(2).product()
}
