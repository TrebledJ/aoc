use std::fs;

type Grid = Vec<u32>;

fn main() {
    let filename = "../input/d04.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let (nums, grids) = parse(contents);

    println!("part1: {}", part1(&nums, &grids));
    println!("part2: {}", part2(&nums, &grids));
}

fn parse(input: String) -> (Vec<u32>, Vec<Grid>) {
    let mut sections = input.split("\n\n");
    let nums: Vec<u32> = sections
        .next()
        .unwrap()
        .split(',')
        .map(|x| x.parse::<u32>().unwrap())
        .collect();
    let grids = sections
        .map(|g| {
            g.split(char::is_whitespace)
                .filter(|x| !x.is_empty())
                .map(|x| x.parse::<u32>().unwrap())
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();
    (nums, grids)
}

fn part1(nums: &Vec<u32>, grids: &Vec<Grid>) -> u32 {
    let ngrids = grids.len();
    let mut solved = vec![false; ngrids];
    let mut marks = vec![vec![false; 5 * 5]; ngrids];
    for x in nums {
        for (gi, g) in grids.iter().enumerate() {
            if !solved[gi] && g.contains(&x) {
                let gii = g.iter().position(|y| y == x).unwrap();
                marks[gi][gii] = true;
                if check_win(&marks[gi], gii) {
                    solved[gi] = true;
                    return grid_score(g, &marks[gi], x);
                }
            }
        }
    }
    unreachable!();
}

fn part2(nums: &Vec<u32>, grids: &Vec<Grid>) -> u32 {
    let ngrids = grids.len();
    let mut solved_grids = 0usize;
    let mut solved = vec![false; ngrids];
    let mut marks = vec![vec![false; 5 * 5]; ngrids];
    for x in nums {
        for (gi, g) in grids.iter().enumerate() {
            if !solved[gi] && g.contains(&x) {
                let gii = g.iter().position(|y| y == x).unwrap();
                marks[gi][gii] = true;
                if check_win(&marks[gi], gii) {
                    solved[gi] = true;
                    solved_grids += 1;
                    if solved_grids == ngrids {
                        return grid_score(g, &marks[gi], x);
                    }
                }
            }
        }
    }
    unreachable!();
}

fn at(i: usize, j: usize) -> usize {
    return j * 5 + i;
}
fn index2(i: usize) -> (usize, usize) {
    return (i % 5, i / 5);
}

fn check_win(mark: &Vec<bool>, last_update: usize) -> bool {
    let (x, y) = index2(last_update);

    return (0..=4).all(|i| mark[at(x, i)]) || (0..=4).all(|i| mark[at(i, y)]);
}

fn grid_score(grid: &Grid, mark: &Vec<bool>, last: &u32) -> u32 {
    return grid
        .iter()
        .zip(mark)
        .map(|(x, m)| x * (!m as u32))
        .sum::<u32>()
        * last;
}
