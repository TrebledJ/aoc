use itertools::Itertools;
use std::fs;

fn main() {
    let filename = "../input/d06.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents);
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

fn parse(contents: String) -> String {
    contents
}

fn find_first_window(xs: &String, n: usize) -> Option<usize> {
    xs.as_bytes().windows(n).enumerate().filter(|(_, w)| w.iter().unique().count() == n).next().map(|x| x.0 + n)
}

fn part1(xs: &String) -> usize {
    find_first_window(xs, 4).unwrap()
}

fn part2(xs: &String) -> usize {
    find_first_window(xs, 14).unwrap()
}
