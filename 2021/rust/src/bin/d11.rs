use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;

type KType = usize;
type VType = u8;
type Map = HashMap<(KType, KType), VType>;
type Set = HashSet<(KType, KType)>;

fn main() {
    let filename = "../input/d11.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let grid = parse(contents);

    println!("part1: {}", part1(&grid));
    println!("part2: {}", part2(&grid));
}

fn parse(contents: String) -> Map {
    contents
        .lines()
        .enumerate()
        .map(|(j, s)| {
            s.chars()
                .enumerate()
                .map(move |(i, c)| ((i, j), c.to_digit(10).unwrap() as VType))
        })
        .flatten()
        .into_iter()
        .collect()
}

fn part1(grid: &Map) -> u32 {
    let mut mgrid = grid.clone();
    let keys = grid.keys().copied().collect::<Vec<_>>();
    let steps = 100;
    let mut total_flashes = 0;
    for _ in 0..steps {
        total_flashes += step(&keys, &mut mgrid);
    }
    disp(&mgrid, false);
    total_flashes
}

fn part2(grid: &Map) -> u32 {
    let mut mgrid = grid.clone();
    let keys = grid.keys().copied().collect::<Vec<_>>();
    let grid_size = 100;
    let mut i = 0;
    loop {
        i += 1;
        if step(&keys, &mut mgrid) == grid_size {
            return i;
        }
    }
}

fn flash(mgrid: &mut Map, (x, y): (KType, KType), flashed: &mut Set) {
    if flashed.contains(&(x, y)) {
        return;
    }
    flashed.insert((x, y));
    let coors = [
        (0, -1),
        (1, -1),
        (1, 0),
        (1, 1),
        (0, 1),
        (-1, 1),
        (-1, 0),
        (-1, -1),
    ]
    .iter()
    .map(|(dx, dy)| (x as i32 + dx, y as i32 + dy))
    .filter(|(x2, y2)| 0 <= *x2 && *x2 < 10 && 0 <= *y2 && *y2 < 10)
    .map(|(x2, y2)| (x2 as usize, y2 as usize))
    .collect::<Vec<_>>();
    for coor in &coors {
        *mgrid.get_mut(coor).unwrap() += 1;
        if mgrid[coor] > 9 {
            flash(mgrid, *coor, flashed)
        }
    }
}

fn step(keys: &Vec<(KType, KType)>, mgrid: &mut Map) -> u32 {
    // Return number of new flashes.
    let mut flashed = Set::new();
    for coor in keys {
        *mgrid.get_mut(coor).unwrap() += 1;
    }
    for (coor, v) in mgrid.clone() {
        if v > 9 {
            flash(mgrid, coor, &mut flashed);
        }
    }
    for coor in keys {
        if mgrid[coor] > 9 {
            *mgrid.get_mut(coor).unwrap() = 0;
        }
    }
    flashed.len() as u32
}

fn disp(grid: &Map, spread: bool) {
    for j in 0..10 {
        for i in 0..10 {
            if spread {
                print!(" {:2}", grid[&(i as usize, j as usize)])
            } else {
                print!("{}", grid[&(i as usize, j as usize)])
            }
        }
        println!("");
    }
    println!("");
}
