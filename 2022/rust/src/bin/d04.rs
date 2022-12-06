use std::fs;

fn main() {
    let filename = "../input/d04.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents);
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

fn parse(contents: String) -> Vec<((i32, i32), (i32, i32))> {
    contents
        .lines()
        .map(|s| {
            let mut spl = s.split(',').map(|x| {
                let (a, b) = x.split_once('-').unwrap();
                (a.parse().unwrap(), b.parse().unwrap())
            });
            (spl.next().unwrap(), spl.next().unwrap())
        })
        .collect::<Vec<_>>()
}

fn part1(xs: &Vec<((i32, i32), (i32, i32))>) -> usize {
    xs.iter().filter(|((a1, b1), (a2, b2))| {
        (a1 <= a2 && b2 <= b1) || (a2 <= a1 && b1 <= b2)
    }).count()
}

fn part2(xs: &Vec<((i32, i32), (i32, i32))>) -> usize {
    xs.iter().filter(|((a1, b1), (a2, b2))| {
        !(b1 < a2 || b2 < a1)
    }).count()
}
