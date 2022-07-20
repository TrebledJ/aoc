use std::fs;


// The three states that a cell could have.
#[derive(Debug, Clone, PartialEq, Eq)]
enum Cell {
    Empty,
    Down,
    Right,
}

impl Cell {
    fn from_char(c: char) -> Cell {
        match c {
            '.' => Cell::Empty,
            'v' => Cell::Down,
            '>' => Cell::Right,
            _ => panic!("aaaaaaaaaah")
        }
    }
}

type Grid = Vec<Vec<Cell>>;

#[derive(Debug, Clone)]
struct Data {
    grid: Grid,
    width: usize,
    height: usize,
    downs: Vec<(usize, usize)>,
    rights: Vec<(usize, usize)>,
}


fn main() {
    let filename = "../input/d25.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let cmds = parse(contents);

    println!("part1: {}", part1(&cmds));
}

fn parse(contents: String) -> Data {
    let grid: Grid = contents.lines().map(|l| l.chars().map(Cell::from_char).collect()).collect();
    let width = grid[0].len();
    let height = grid.len();
    let with_coords: Vec<(char, usize, usize)> = contents.lines().enumerate().map(|(y, l)| l.chars().enumerate().map(|(x, c)| (c, x, y)).collect::<Vec<_>>()).flatten().collect();
    let downs = with_coords.iter().filter(|(c, _, _)| *c == 'v').map(|(_, x, y)| (*x, *y)).collect();
    let rights = with_coords.iter().filter(|(c, _, _)| *c == '>').map(|(_, x, y)| (*x, *y)).collect();
    Data{grid, width, height, downs, rights}
}


fn part1(g: &Data) -> u32 {
    let mut moved = true;
    let mut grid = g.grid.clone();
    let mut new_grid = grid.clone();
    let mut downs = g.downs.clone();
    let mut rights = g.rights.clone();

    let eastof = |(x, y): (usize, usize)| {
        if x + 1 == g.width { (0, y) } else { (x + 1, y) }
    };
    let southof = |(x, y): (usize, usize)| {
        if y + 1 == g.height { (x, 0) } else { (x, y + 1) }
    };

    fn isfree(grid: &Grid, (x, y): (usize, usize)) -> bool {
        grid[y][x] == Cell::Empty
    }

    let mut iter = 0;

    while moved {
        iter += 1;
        moved = false;

        // Step east-facing cucumbers.
        for i in 0..rights.len() {
            let (x, y) = rights[i];
            let (tx, ty) = eastof(rights[i]);
            if isfree(&grid, (tx, ty)) {
                new_grid[y][x] = Cell::Empty;
                new_grid[ty][tx] = Cell::Right;
                rights[i] = (tx, ty);
                moved = true;
            }
        }
        grid = new_grid.clone();

        // Step south-facing cucumbers.
        for i in 0..downs.len() {
            let (x, y) = downs[i];
            let (tx, ty) = southof(downs[i]);
            if isfree(&grid, (tx, ty)) {
                new_grid[y][x] = Cell::Empty;
                new_grid[ty][tx] = Cell::Down;
                downs[i] = (tx, ty);
                moved = true;
            }
        }
        grid = new_grid.clone();
    }
    iter
}

#[allow(dead_code)]
fn disp(grid: &Grid) {
    for j in 0..grid.len() {
        for i in 0..grid[j].len() {
            print!("{}", match grid[j][i] {
                Cell::Empty => ".",
                Cell::Down => "v",
                Cell::Right => ">",
            })
        }
        println!("")
    }
}