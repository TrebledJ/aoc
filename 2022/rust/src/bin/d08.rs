use itertools::Itertools;
use std::fs;

fn main() {
    let filename = "../input/d08.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents);
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

fn parse(contents: String) -> Vec<Vec<u32>> {
    contents
        .lines()
        .map(|s| s.chars().map(|c| c.to_digit(10).unwrap()).collect())
        .collect()
}

fn part1(grid: &Vec<Vec<u32>>) -> u32 {
    fn scan_count<It>(grid: &Vec<Vec<u32>>, visited: &mut Vec<Vec<bool>>, it: It) -> u32
    where
        It: Iterator<Item = (usize, usize)>,
    {
        let mut curr = -1;
        let mut count = 0;
        for (x, y) in it {
            let val = grid[y][x] as i32;
            if val > curr {
                curr = val;
                if !visited[y][x] {
                    visited[y][x] = true;
                    count += 1;
                }
            }
        }
        count
    }

    let mut count = 0;
    let mut visited: Vec<Vec<bool>> = grid
        .iter()
        .map(|xs| xs.iter().map(|_| false).collect())
        .collect();

    let w = grid[0].len();
    let h = grid.len();
    for y in 0..h {
        count += scan_count(grid, &mut visited, (0..w).map(|x| (x, y)));
        count += scan_count(grid, &mut visited, (0..w).rev().map(|x| (x, y)));
    }

    for x in 0..w {
        count += scan_count(grid, &mut visited, (0..h).map(|y| (x, y)));
        count += scan_count(grid, &mut visited, (0..h).rev().map(|y| (x, y)));
    }

    count
}

fn part2(grid: &Vec<Vec<u32>>) -> u32 {
    fn scan_count_less_than<It>(grid: &Vec<Vec<u32>>, val: u32, it: It, n: usize) -> u32
    where
        It: Iterator<Item = (usize, usize)>,
    {
        let count = it.take_while(|&(x, y)| grid[y][x] < val).count();
        if count == n {
            count as u32 // Reached past edge.
        } else {
            count as u32 + 1 // Stopped early.
        }
    }

    let w = grid[0].len();
    let h = grid.len();
    let score_of = |x: usize, y: usize| -> u32 {
        let val = grid[y][x];
        let mut score = 1;
        score *= scan_count_less_than(grid, val, (x + 1..w).map(|xi| (xi, y)), w - x - 1);
        score *= scan_count_less_than(grid, val, (0..x).rev().map(|xi| (xi, y)), x);
        score *= scan_count_less_than(grid, val, (y + 1..h).map(|yi| (x, yi)), h - y - 1);
        score *= scan_count_less_than(grid, val, (0..y).rev().map(|yi| (x, yi)), y);
        score
    };

    (0..w)
        .cartesian_product(0..h)
        .map(|(x, y)| score_of(x, y))
        .max()
        .unwrap()
}
