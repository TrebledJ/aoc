use itertools::Itertools;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::str::FromStr;

#[derive(PartialEq, Eq, Debug, Clone, Copy)]
enum Dir {
    L,
    R,
    U,
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

// U --> tail will move either up-left, up, up-right, or not move at all.
// up-left: occurs only if h is at offset (-1, -1).
// up-right: occurs only if h is at offset (1, -1).
// up: occurs only if h is at offset (0, -1).
// At any other offset, the tail doesn't move.

fn next((x, y): Point, dir: Dir) -> Point {
    match dir {
        Dir::L => (x - 1, y),
        Dir::R => (x + 1, y),
        Dir::U => (x, y - 1),
        Dir::D => (x, y + 1),
    }
}

fn step((hx, hy): Point, (tx, ty): Point, dir: Dir) -> Point {
    let offsets: [Point; 3] = match dir {
        Dir::L => [(-1, 1), (-1, 0), (-1, -1)],
        Dir::R => [(1, -1), (1, 0), (1, 1)],
        Dir::U => [(-1, -1), (0, -1), (1, -1)],
        Dir::D => [(1, 1), (0, 1), (-1, 1)],
    };

    for (ox, oy) in offsets {
        if (tx + ox, ty + oy) == (hx, hy) {
            return (hx, hy); // Tail follows head.
        }
    }
    (tx, ty) // Tail doesn't move.
}

fn part1(xs: &Vec<(Dir, u32)>) -> u32 {
    // 1. Expand directions (e.g. R 3 => R R R).
    // 2. Flatten the iterator.
    // 3. Fold over a mutable hashset, inserting the tail point on each fold.
    // 4. Return the length of the hashset, which is the number of visited points.

    let mut visited: HashSet<Point> = HashSet::new();
    xs.iter()
        .map(|(dir, count)| std::iter::repeat(dir).take(*count as usize))
        .flatten()
        .fold((&mut visited, (0, 0), (0, 0)), |(visited, h, t), dir| {
            let h2 = next(h, *dir);
            let t2 = step(h, t, *dir);
            visited.insert(t2);
            (visited, h2, t2)
        });
    visited.len() as u32
}

fn part2(xs: &Vec<(Dir, u32)>) -> u32 {
    0
}
