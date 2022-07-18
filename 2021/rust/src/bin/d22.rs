use regex::Regex;
use std::collections::HashSet;
use std::fs;

type Set = HashSet<(i32, i32, i32)>;

struct Command(bool, i32, i32, i32, i32, i32, i32);

fn main() {
    let filename = "../input/d22.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let cmds = parse(contents);

    println!("part1: {}", part1(&cmds));
    println!("part2: {}", part2(&cmds));
}

fn parse(contents: String) -> Vec<Command> {
    let re =
        Regex::new(r"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)").unwrap();
    contents
        .lines()
        .map(|s| {
            for cap in re.captures_iter(s) {
                return Command(
                    cap[1].eq("on"),
                    cap[2].parse::<i32>().unwrap(),
                    cap[3].parse::<i32>().unwrap(),
                    cap[4].parse::<i32>().unwrap(),
                    cap[5].parse::<i32>().unwrap(),
                    cap[6].parse::<i32>().unwrap(),
                    cap[7].parse::<i32>().unwrap(),
                );
            }
            unreachable!("aaaaaah")
        })
        .collect()
}

fn part1(cmds: &Vec<Command>) -> u32 {
    let mut set: Set = HashSet::new();
    let r = 50;
    for &Command(onoff, x1_, x2_, y1_, y2_, z1_, z2_) in cmds {
        let x1 = x1_.max(-r);
        let x2 = x2_.min(r);
        let y1 = y1_.max(-r);
        let y2 = y2_.min(r);
        let z1 = z1_.max(-r);
        let z2 = z2_.min(r);
        for i in x1..=x2 {
            for j in y1..=y2 {
                for k in z1..=z2 {
                    if onoff {
                        set.insert((i, j, k));
                    } else {
                        set.remove(&(i, j, k));
                    }
                }
            }
        }
    }

    set.len() as u32
}

fn part2(_cmds: &Vec<Command>) -> u32 {
    // Implemented in Haskell.
   0
}
