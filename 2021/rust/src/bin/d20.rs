/**
 * Note that lookup[0] == '#' and lookup[511] = '.'. To properly account for the infinite field, this means
 * on the odd flash, the entire (infinite) surrounding area would be '#'. Thus, on the odd flash, the default
 * cell should be '#' instead of '.'.
 */
use std::collections::HashMap;
use std::fs;

type Map = HashMap<(i32, i32), bool>;

fn main() {
    let filename = "../input/d20.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let (lookup, grid, w, h) = parse(contents);

    println!("part1: {}", part1(&lookup, &grid, w, h));
    println!("part2: {}", part2(&lookup, &grid, w, h));
}

fn parse(contents: String) -> (Vec<bool>, Map, usize, usize) {
    let (s, input) = contents.split_once("\n\n").unwrap();
    let ls = input.lines();
    (
        s.chars()
            .filter(|&c| c == '#' || c == '.')
            .map(|c| c == '#')
            .collect(),
        ls.clone()
            .enumerate()
            .map(|(j, row)| {
                row.chars()
                    .enumerate()
                    .map(move |(i, c)| ((i as i32, j as i32), c == '#'))
            })
            .flatten()
            .into_iter()
            .collect(),
        ls.clone().collect::<Vec<_>>().len(),
        ls.take(1).collect::<Vec<_>>()[0].len(),
    )
}

fn part1(lookup: &Vec<bool>, grid: &Map, w: usize, h: usize) -> u32 {
    enhance(2, lookup, grid, w, h)
}

fn part2(lookup: &Vec<bool>, grid: &Map, w: usize, h: usize) -> u32 {
    enhance(50, lookup, grid, w, h)
}

fn enhance(n: u32, lookup: &Vec<bool>, grid: &Map, w: usize, h: usize) -> u32 {
    let mut oldg = grid.clone();
    let mut newg: Map = HashMap::new();

    // Helper to get a cell.
    let get = |i, j, k, oldg: &Map| {
        if match oldg.get(&(i, j)) {
            Some(&v) => v,
            None => k % 2 == 0, // Default to `true` on odd flashes and `false` on even flashes. f -> t -> f -> t -> ...
        } {
            '1'
        } else {
            '0'
        }
    };

    // Enhances character at (i, j).
    let unfilter = |i, j, k, oldg: &Map| {
        let s: String = [
            get(i - 1, j - 1, k, oldg),
            get(i, j - 1, k, oldg),
            get(i + 1, j - 1, k, oldg),
            get(i - 1, j, k, oldg),
            get(i, j, k, oldg),
            get(i + 1, j, k, oldg),
            get(i - 1, j + 1, k, oldg),
            get(i, j + 1, k, oldg),
            get(i + 1, j + 1, k, oldg),
        ]
        .iter()
        .collect();
        let ind = usize::from_str_radix(s.as_str(), 2).unwrap();
        lookup[ind]
    };

    for k in 1..=n as i32 {
        for j in -k..h as i32 + k {
            for i in -k..w as i32 + k {
                let val = unfilter(i, j, k, &oldg);
                match newg.get_mut(&(i, j)) {
                    Some(m) => {
                        *m = val;
                    }
                    None => {
                        newg.insert((i, j), val);
                    }
                }
            }
        }
        oldg = newg.clone();
        // disp(&newg, -k, h as i32 + k, -k, w as i32 + k);
    }
    newg.iter().filter(|(_, c)| **c).count() as u32
}

#[allow(dead_code)]
fn disp(g: &Map, minx: i32, maxx: i32, miny: i32, maxy: i32) {
    for j in minx..maxx {
        for i in miny..maxy {
            print!("{}", if g[&(i, j)] { '#' } else { '.' })
        }
        print!("\n")
    }
    print!("\n")
}
