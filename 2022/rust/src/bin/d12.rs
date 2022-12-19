use priority_queue::PriorityQueue;
use std::collections::{HashMap, HashSet, LinkedList};
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
    let mut queued = PriorityQueue::new();
    queued.push(*start, 10000);

    let mut from = HashMap::<Point, Point>::new();
    let mut gscore = HashMap::<Point, u32>::new();

    gscore.insert(*start, 10000);

    // The lower the score, the better.
    let score = |p: &Point| -> u32 {
        // (grid[end] - grid[p]) *
        dist(p, end) + 1
    };

    let can_move = |from_level: u32, to_level: u32| -> bool { from_level + 1 >= to_level };
    let neighbours = |(x, y): Point| -> Vec<Point> {
        [(x, y - 1), (x + 1, y), (x, y + 1), (x - 1, y)]
            .into_iter()
            .filter(|(a, b)| *a < *w && *b < *h && can_move(grid[&(x, y)], grid[&(*a, *b)]))
            .collect()
    };

    while !queued.is_empty() {
        let (curr, _) = queued.pop().unwrap();
        // println!("curr: {curr:?}");

        for n in neighbours(curr) {
            let s = score(&n);
            // println!("  n: {n:?} ({:?}) / score: {s:?}", grid[&n]);
            let new_gscore = gscore.get(&curr).unwrap() + s;
            if !gscore.contains_key(&n) || new_gscore < *gscore.get(&n).unwrap() {
                queued.push(n, 10000 - s);
                gscore.insert(n, new_gscore);
                from.insert(n, curr);
            }
        }
    }

    // print_dirmap(&from, *w, *h);

    // println!("\nbacktracking");
    let mut curr = end.clone();
    let mut count = 0;
    loop {
        // println!("{curr:?}");
        curr = *from.get(&curr).unwrap();
        count += 1;
        if curr == *start {
            break;
        }
    }
    count
}

fn print_dirmap(xs: &HashMap<Point, Point>, w: usize, h: usize) {
    let invmap = xs.iter().map(|(k, v)| (v, k)).collect::<HashMap<_, _>>();
    for y in 0..h {
        for x in 0..w {
            let &&(tox, toy) = invmap.get(&(x, y)).unwrap_or(&&(x, y));
            let dir = if tox == x + 1 {
                '>'
            } else if tox == x - 1 {
                '<'
            } else if toy == y + 1 {
                'v'
            } else if toy == y - 1 {
                '^'
            } else {
                '.'
            };
            print!("{}", dir);
        }
        println!("");
    }
}

fn part2((start, end, w, h, grid): &(Point, Point, usize, usize, Grid)) -> u32 {
    0
}

fn dist(&(x1, y1): &Point, &(x2, y2): &Point) -> u32 {
    (x1.abs_diff(x2) + y1.abs_diff(y2)) as u32
}
