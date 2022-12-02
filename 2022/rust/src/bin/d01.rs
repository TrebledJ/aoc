use std::fs;

fn main() {
    let filename = "../input/d01.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let nums = parse(contents);
    println!("part1: {}", part1(&nums));
    println!("part2: {}", part2(&nums));
}

fn parse(contents: String) -> Vec<i32> {
    contents
            .split("\n\n")
            .map(|s| s.lines().map(|x| x.parse::<i32>().unwrap()).sum())
            .collect::<Vec<_>>()
}

fn part1(nums: &Vec<i32>) -> i32 {
    *nums.iter().max().unwrap()
}

fn part2(nums: &Vec<i32>) -> i32 {
    let mut v = nums.clone();
    v.sort();
    v.iter().rev().take(3).sum()
}
