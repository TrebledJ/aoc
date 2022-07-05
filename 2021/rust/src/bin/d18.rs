use std::fs;

#[derive(Debug, Clone)]
struct Cell {
    value: u32,
    depth: u32,
}

fn main() {
    let filename = "../input/d18.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let stuff = parse(contents);

    println!("part1: {}", part1(&stuff));
    println!("part2: {}", part2(&stuff));
}

fn parse(contents: String) -> Vec<Vec<Cell>> {
    fn parse_line(line: &str) -> Vec<Cell> {
        let mut v = Vec::new();
        let mut depth = 0u32;
        for c in line.bytes() {
            if c == b'[' {
                depth += 1;
            } else if c == b']' {
                depth -= 1;
            } else if c.is_ascii_digit() {
                v.push(Cell {
                    value: (c - b'0') as u32,
                    depth,
                })
            }
        }
        v
    }
    contents.lines().map(parse_line).collect()
}

fn part1(input: &Vec<Vec<Cell>>) -> u32 {
    let sum = input
        .iter()
        .cloned()
        .reduce(|acc, x| add(&acc, &x))
        .unwrap();
    magnitude(&sum)
}

fn part2(input: &Vec<Vec<Cell>>) -> u32 {
    let mut magmax = 0;
    for i in 0..input.len() {
        for j in i + 1..input.len() {
            let res = add(&input[i], &input[j]);
            let mag = magnitude(&res);
            if mag > magmax {
                magmax = mag;
            }
        }
    }
    magmax
}

// Maybe using a linked list is better?
fn add(a: &Vec<Cell>, b: &Vec<Cell>) -> Vec<Cell> {
    let mut v = a
        .iter()
        .map(|x| Cell {
            value: x.value,
            depth: x.depth + 1,
        })
        .chain(b.iter().map(|x| Cell {
            value: x.value,
            depth: x.depth + 1,
        }))
        .collect::<Vec<_>>();

    fn action(v: &mut Vec<Cell>) -> bool {
        for (i, &Cell { value, depth }) in v.iter().enumerate() {
            if depth > 4 {
                // KABOOM!
                if i > 0 {
                    v[i - 1].value += value;
                }
                if i + 2 < v.len() {
                    v[i + 2].value += v[i + 1].value;
                }
                v[i].value = 0;
                v[i].depth -= 1;
                v.remove(i + 1);
                return true;
            }
        }
        for (i, &Cell { value, depth }) in v.iter().enumerate() {
            if value >= 10 {
                // SPLAT!
                v[i].value /= 2;
                v[i].depth += 1;
                v.insert(
                    i + 1,
                    Cell {
                        value: (value as f64 / 2.0).ceil() as u32,
                        depth: depth + 1,
                    },
                );
                return true;
            }
        }
        false
    }

    while action(&mut v) {}
    v
}

fn magnitude(v: &Vec<Cell>) -> u32 {
    let mut vstack: Vec<u32> = Vec::new();
    let mut dstack: Vec<u32> = Vec::new();
    for &Cell { value, depth } in v.iter() {
        let pdepth = if dstack.is_empty() {
            0
        } else {
            *dstack.last().unwrap()
        };
        if depth > pdepth {
            vstack.push(value);
            dstack.push(depth);
        } else if depth < pdepth {
            panic!("aaaaaaaaaaaaaaaaaaaaaaaaaaaaahhhhhhhhh!");
        } else {
            let mut d = depth;
            let mut val = value;
            while !dstack.is_empty() && d == *dstack.last().unwrap() {
                val = 3 * vstack.pop().unwrap() + 2 * val;
                d -= 1;
                dstack.pop();
            }
            vstack.push(val);
            dstack.push(d);
        }
    }
    vstack.pop().unwrap()
}

#[allow(dead_code)]
fn disp(v: &Vec<Cell>) {
    print!("[");
    for (i, &Cell { value, depth }) in v.iter().enumerate() {
        if i > 0 {
            print!(", ");
        }
        print!("{}:{}", value, depth);
    }
    print!("]\n");
}
