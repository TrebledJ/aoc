use std::fs;
use std::str::FromStr;

#[derive(Debug)]
enum Dir {
    Forward,
    Up,
    Down,
}

#[derive(Debug)]
struct Inst {
    dir: Dir,
    n: i32,
}

impl FromStr for Inst {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let split = s.split_whitespace().collect::<Vec<&str>>();
        Ok(Inst {
            dir: match split[0] {
                "forward" => Dir::Forward,
                "up" => Dir::Up,
                "down" => Dir::Down,
                _ => panic!("aaaaahhhhhhhh!!!"),
            },
            n: split[1].parse::<i32>().unwrap(),
        })
    }
}

fn part1(insts: &Vec<Inst>) -> i32 {
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    for inst in insts {
        match inst.dir {
            Dir::Forward => x += inst.n,
            Dir::Up => y -= inst.n,
            Dir::Down => y += inst.n,
        }
    }
    x * y
}

fn part2(insts: &Vec<Inst>) -> i32 {
    let mut x: i32 = 0;
    let mut y: i32 = 0;
    let mut aim: i32 = 0;
    for inst in insts {
        match inst.dir {
            Dir::Forward => {
                x += inst.n;
                y += aim * inst.n
            }
            Dir::Up => aim -= inst.n,
            Dir::Down => aim += inst.n,
        }
    }
    x * y
}

fn main() {
    let filename = "../input/d02.txt";

    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let text = contents.lines();
    let insts = text.map(|s| Inst::from_str(s).unwrap()).collect::<Vec<_>>();

    println!("part1: {}", part1(&insts));
    println!("part2: {}", part2(&insts));
}
