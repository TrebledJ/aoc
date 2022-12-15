use std::collections::HashSet;
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Dir {
    L,
    U,
    R,
    D,
}

impl FromStr for Dir {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "L" => Ok(Dir::L),
            "R" => Ok(Dir::R),
            "U" => Ok(Dir::U),
            "D" => Ok(Dir::D),
            _ => Err(()),
        }
    }
}

fn main() {
    let filename = "../input/d09.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents);
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

fn parse(contents: String) -> Vec<(Dir, u32)> {
    contents
        .lines()
        .map(|s| {
            let tup = s.split_once(" ").unwrap();
            (tup.0.parse::<Dir>().unwrap(), tup.1.parse::<u32>().unwrap())
        })
        .collect()
}

type Point = (i32, i32);

fn next((x, y): Point, dir: Dir) -> Point {
    match dir {
        Dir::L => (x - 1, y),
        Dir::R => (x + 1, y),
        Dir::U => (x, y - 1),
        Dir::D => (x, y + 1),
    }
}

fn follow((hx, hy): Point, (tx, ty): Point) -> Point {
    let xdiff = hx.abs_diff(tx);
    let ydiff = hy.abs_diff(ty);
    if xdiff > 1 || ydiff > 1 {
        (tx + (hx - tx).signum(), ty + (hy - ty).signum())
    } else {
        (tx, ty)
    }
}

fn part1(xs: &Vec<(Dir, u32)>) -> u32 {
    simulate(xs, 2)
}

fn part2(xs: &Vec<(Dir, u32)>) -> u32 {
    simulate(xs, 10)
}

/**
 * @brief   Simulates a rope with `joints` joints.
 * @return  Number of unique locations visited by the tail.
 */
fn simulate(xs: &Vec<(Dir, u32)>, joints: usize) -> u32 {
    // 1. Expand directions (e.g. R 3 => R R R).
    // 2. Flatten the iterator.
    // 3. Fold over a mutable hashset, inserting the tail point on each fold.
    // 4. Return the length of the hashset, which is the number of visited points.
    let init = vec![(0, 0); joints];
    let mut visited: HashSet<Point> = HashSet::new();
    xs.iter()
        .map(|(dir, count)| std::iter::repeat(dir).take(*count as usize))
        .flatten()
        .fold(init, |v, dir| {
            let mut v_next = Vec::new();
            v_next.push(next(v[0], *dir));
            v.iter().skip(1).for_each(|t| {
                v_next.push(follow(*v_next.last().unwrap(), *t));
            });
            visited.insert(*v_next.last().unwrap());
            v_next
        });
    visited.len() as u32
}
