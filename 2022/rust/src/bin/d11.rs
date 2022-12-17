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
        if worry % self.test.div == 0 {
            self.test.iftrue
        } else {
            self.test.iffalse
        }
    }

    fn operate(&self, x: u64) -> u64 {
        match self.op {
            Op::Add(v) => x + v,
            Op::Mul(v) => x * v,
            Op::Sqr => x * x,
        }
    }
}

fn monkey(s: &str) -> Option<Monkey> {
    let mut ls = s.lines();
    ls.next();
    let (_, init_str) = ls.next()?.split_once(": ")?;
    let items = init_str
        .split(", ")
        .map(|s| s.parse::<u64>().unwrap())
        .collect::<Vec<_>>();

    let (_, op_str) = ls.next()?.split_once("= ")?;
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

    let div = ls
        .next()?
        .split_once("divisible by ")?
        .1
        .parse::<u64>()
        .unwrap();
    let iftrue = ls.next()?.split_once("monkey ")?.1.parse::<u64>().unwrap();
    let iffalse = ls.next()?.split_once("monkey ")?.1.parse::<u64>().unwrap();

    Some(Monkey {
        items,
        op,
        test: Decision {
            div,
            iftrue,
            iffalse,
        },
    })
}

impl FromStr for Monkey {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        monkey(s).ok_or(())
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
    contents.split("\n\n").map(|s| monkey(s).unwrap()).collect()
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
