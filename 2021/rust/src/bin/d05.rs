use std::cmp;
use std::fs;
use std::iter;

type Coor = (usize, usize);

fn make_seq(a: usize, b: usize) -> Vec<usize> {
    if a < b {
        (a..=b).collect()
    } else {
        (b..=a).rev().collect()
    }
}

fn part1(vents: &Vec<(Coor, Coor)>) -> u32 {
    let mut grid = generate_grid(&vents);
    for &vent in vents {
        let ((x1, y1), (x2, y2)) = vent;
        if x1 == x2 {
            for y in make_seq(y1, y2) {
                grid[y][x1] += 1
            }
        } else if y1 == y2 {
            for x in make_seq(x1, x2) {
                grid[y1][x] += 1
            }
        }
    }
    grid.iter()
        .map(|r| r.iter().filter(|x| **x >= 2).count() as u32)
        .sum()
}

fn part2(vents: &Vec<(Coor, Coor)>) -> u32 {
    let mut grid = generate_grid(&vents);
    for &vent in vents {
        let ((x1, y1), (x2, y2)) = vent;
        if x1 == x2 {
            for y in make_seq(y1, y2) {
                grid[y][x1] += 1
            }
        } else if y1 == y2 {
            for x in make_seq(x1, x2) {
                grid[y1][x] += 1
            }
        } else {
            for (&x, y) in make_seq(x1, x2).iter().zip(make_seq(y1, y2)) {
                grid[y][x] += 1
            }
        }
    }
    grid.iter()
        .map(|r| r.iter().filter(|&&x| x >= 2).count() as u32)
        .sum()
}

fn generate_grid(vents: &Vec<(Coor, Coor)>) -> Vec<Vec<i32>> {
    let maxx = vents.iter().map(|(c, d)| cmp::max(c.1, d.1)).max().unwrap();
    let maxy = vents.iter().map(|(c, d)| cmp::max(c.1, d.1)).max().unwrap();
    iter::repeat(iter::repeat(0).take(maxx + 1 as usize).collect::<Vec<_>>())
        .take(maxy + 1 as usize)
        .collect::<Vec<_>>()
}

fn parse(input: String) -> Vec<(Coor, Coor)> {
    input
        .lines()
        .map(|s| s.split_once(" -> ").unwrap())
        .map(|(c1, c2)| (c1.split_once(",").unwrap(), c2.split_once(",").unwrap()))
        .map(|((x1, y1), (x2, y2))| {
            (
                (x1.parse().unwrap(), y1.parse().unwrap()),
                (x2.parse().unwrap(), y2.parse().unwrap()),
            )
        })
        .collect()
}

fn main() {
    let filename = "../input/d05.txt";

    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let vents = parse(contents);

    println!("part1: {}", part1(&vents));
    println!("part2: {}", part2(&vents));
}
