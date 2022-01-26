use std::fs;

fn part1(nums: &Vec<i32>) -> u32 {
    return nums.windows(2).map(|x| (x[0] < x[1]) as u32).sum();
}

fn part2(nums: &Vec<i32>) -> u32 {
    let collated = nums.windows(3).map(|x| x.iter().sum()).collect::<Vec<i32>>();
    return part1(&collated);
}

fn main() {
    let filename = "../input/d01.txt";

    let contents = fs::read_to_string(filename).expect("Something went wrong reading the file");
    let nums = contents
        .lines()
        .map(|x| x.parse::<i32>().unwrap())
        .collect::<Vec<_>>();
    println!("part1: {}", part1(&nums));
    println!("part2: {}", part2(&nums));
}
