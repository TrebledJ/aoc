use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;

type Point = (usize, usize);
type Grid = HashMap<Point, u32>;

fn main() {
    let filename = "../input/d12.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents).expect("could not parse contents");
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

fn parse(contents: String) -> Option<(Point, Point, usize, usize, Grid)> {
    fn to_level(c: char) -> u32 {
        match c {
            'S' => 0,
            'E' => 25,
            _ => (c as u32) - ('a' as u32),
        }
    }

    let h = contents.lines().count();
    let w = contents.lines().next().unwrap().len();
    let map = contents
        .lines()
        .enumerate()
        .map(|(y, xs)| xs.chars().enumerate().map(move |(x, c)| ((x, y), c)))
        .flatten()
        .collect::<HashMap<_, _>>();
    let start = map.iter().find(|(_, v)| **v == 'S').unwrap().0.clone();
    let end = map.iter().find(|(_, v)| **v == 'E').unwrap().0.clone();
    Some((
        start,
        end,
        w,
        h,
        map.iter().map(|(k, v)| (*k, to_level(*v))).collect(),
    ))
}

fn part1((start, end, w, h, grid): &(Point, Point, usize, usize, Grid)) -> u32 {
    let (srcmap, _) = bfs(start, w, h, grid, |p| p == *end, false);
    backtrack(&srcmap, end, start).len() as u32
}

fn part2((_, end, w, h, grid): &(Point, Point, usize, usize, Grid)) -> u32 {
    let (srcmap, last) = bfs(end, w, h, grid, |p| grid[&p] == 0, true);
    backtrack(&srcmap, &last, end).len() as u32
}

fn bfs(
    start: &Point,
    w: &usize,
    h: &usize,
    grid: &Grid,
    check: impl Fn(Point) -> bool,
    reversed: bool,
) -> (HashMap<Point, Point>, Point) {
    let mut queued = VecDeque::new();
    queued.push_back(*start);

    let mut visited = HashSet::new();
    let mut from = HashMap::<Point, Point>::new();

    fn can_move(from_level: u32, to_level: u32) -> bool {
        from_level + 1 >= to_level
    }

    let neighbours = |(x, y): Point| -> Vec<Point> {
        [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
            .into_iter()
            .filter(|(a, b)| {
                *a < *w
                    && *b < *h
                    && can_move(
                        if !reversed {
                            grid[&(x, y)]
                        } else {
                            grid[&(*a, *b)]
                        },
                        if !reversed {
                            grid[&(*a, *b)]
                        } else {
                            grid[&(x, y)]
                        },
                    )
            })
            .collect()
    };

    let mut curr = (0, 0);
    while !queued.is_empty() {
        curr = queued.pop_front().unwrap();

        if visited.contains(&curr) {
            continue;
        }

        if check(curr) {
            break;
        }

        visited.insert(curr);

        for n in neighbours(curr) {
            if !visited.contains(&n) {
                queued.push_back(n);
                from.insert(n, curr);
            }
        }
    }

    (from, curr)
}

fn backtrack(srcmap: &HashMap<Point, Point>, start: &Point, target: &Point) -> Vec<Point> {
    let mut vec = Vec::new();
    let mut curr = start.clone();
    while curr != *target {
        curr = *srcmap.get(&curr).unwrap();
        vec.push(curr);
    }
    vec
}
