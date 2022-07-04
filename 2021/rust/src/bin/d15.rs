use priority_queue::PriorityQueue;
use std::cmp;
use std::fs;

type CellType = u32;
type Grid = Vec<Vec<CellType>>;

/**
 * DP Recursion.
 *
 * Assume we can only move right and down.                           | Bad assumption I guess.
 * Let G be the grid, so that G(x,y) looks up the value at (x,y).
 * Let P(x,y) be the minimum risk level from the top-left to (x,y).
 *  - base case
 *      P(0,0) = 0
 *  - recursive case
 *      P(x,0) = P(x-1,0) + G(x,0)
 *      P(0,y) = P(0,y-1) + G(0,y)
 *      P(x,y) = min(P(x-1,y), P(x,y-1)) + G(x,y)
 *
 * Tabulate a 2D array.
 */
#[allow(dead_code)]
fn dp(grid: &Grid) -> u32 {
    let h = grid.len();
    let w = grid[0].len();
    let mut p = vec![vec![0; w]; h];
    for j in 0..h {
        for i in 0..w {
            if i == 0 {
                if j == 0 {
                    p[0][0] = 0;
                } else {
                    p[j][i] = p[j - 1][i] + grid[j][i];
                }
            } else if j == 0 {
                p[j][i] = p[j][i - 1] + grid[j][i];
            } else {
                p[j][i] = cmp::min(p[j - 1][i], p[j][i - 1]) + grid[j][i];
            }
        }
    }
    p[h - 1][w - 1]
}

fn djikstra(grid: &Grid) -> u32 {
    let h = grid.len();
    let w = grid[0].len();
    let mut nodes = vec![vec![u32::MAX; w]; h];
    let mut queue: PriorityQueue<(u32, u32), u32> = PriorityQueue::new();
    fn prio(v: u32) -> u32 {
        // Invert priority since PQ works on max elements.
        u32::MAX - v
    }
    nodes[0][0] = 0;
    queue.push((0, 0), prio(0));
    while !queue.is_empty() {
        let ((x, y), weight0) = queue.pop().unwrap();
        let weight = prio(weight0);
        let (xi, yi) = (x as usize, y as usize);

        // Found target.
        if xi == (w - 1) && yi == (h - 1) {
            return weight;
        }

        // Update neighbours.
        let ns = [(0, -1), (1, 0), (0, 1), (-1, 0)]
            .iter()
            .map(|(i, j)| (x as i32 + i, y as i32 + j))
            .filter(|(x, y)| 0 <= *x && *x < w as i32 && 0 <= *y && *y < h as i32);
        for (nx, ny) in ns {
            let (nxi, nyi) = (nx as usize, ny as usize);
            let new_weight = nodes[yi][xi] + grid[nyi][nxi];
            if nodes[nyi][nxi] == u32::MAX {
                // Not visited before.
                nodes[nyi][nxi] = new_weight;
                queue.push((nx as u32, ny as u32), prio(new_weight));
            } else if new_weight < nodes[nyi][nxi] {
                // Found a shorter path. Update.
                nodes[nyi][nxi] = new_weight;
                queue.change_priority(&(nx as u32, ny as u32), prio(new_weight));
            }
        }
    }
    0
}

fn part1(grid: &Grid) -> u32 {
    djikstra(grid)
}

fn part2(grid: &Grid) -> u32 {
    let mut g = grid.clone();
    let h = grid.len();
    for y in 0..h {
        for i in 1..5 {
            // 9 should wrap to 1. Since valid values are 1..9, apply mod 9 with some extra arithmetic.
            g[y].extend(grid[y].iter().map(|x| (x + i - 1) % 9 + 1));
        }
    }
    let fat_grid = g.clone();
    for i in 1..5 {
        g.extend(
            fat_grid
                .iter()
                .map(|r| r.iter().map(|c| (c + i - 1) % 9 + 1).collect::<Vec<u32>>()),
        );
    }
    djikstra(&g)
}

/**
 * Helper function for printing out a grid.
 */
#[allow(dead_code)]
fn disp(grid: &Grid, spread: bool) {
    let h = grid.len();
    let w = grid[0].len();
    for j in 0..h {
        for i in 0..w {
            if spread {
                print!(" {:2}", grid[j][i])
            } else {
                print!("{}", grid[j][i])
            }
        }
        println!("");
    }
    println!("");
}

fn parse(contents: String) -> Grid {
    contents
        .lines()
        .map(|s| {
            s.chars()
                .map(|c| c.to_digit(10).unwrap() as CellType)
                .collect()
        })
        .collect()
}

fn main() {
    let filename = "../input/d15.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let grid = parse(contents);

    println!("part1: {}", part1(&grid));
    println!("part2: {}", part2(&grid));
}
