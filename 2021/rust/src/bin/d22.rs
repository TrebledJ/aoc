use regex::Regex;
use std::collections::{HashSet, LinkedList};
use std::fs;

type Set = HashSet<(cube_t, cube_t, cube_t)>;

#[allow(non_camel_case_types)]
type cube_t = i64;
type Cuboid = ((cube_t, cube_t), (cube_t, cube_t), (cube_t, cube_t));
struct Command(bool, Cuboid);

fn main() {
    let filename = "../input/d22.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let cmds = parse(contents);

    println!("part1: {}", part1(&cmds));
    use std::time::Instant;
    let now = Instant::now();
    println!("part2: {}", part2(&cmds));
    println!("Elapsed: {:.2?}", now.elapsed());
    let now = Instant::now();
    println!("part2 (opt): {}", part2_opt(&cmds));
    println!("Elapsed: {:.2?}", now.elapsed());
    let now = Instant::now();
    println!("part2 (opt2?): {}", part2_opt2(&cmds));
    println!("Elapsed: {:.2?}", now.elapsed());
}

fn parse(contents: String) -> Vec<Command> {
    let re =
        Regex::new(r"(on|off) x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)").unwrap();
    contents
        .lines()
        .map(|s| {
            for cap in re.captures_iter(s) {
                return Command(
                    cap[1].eq("on"),
                    (
                        (
                            cap[2].parse::<cube_t>().unwrap(),
                            cap[3].parse::<cube_t>().unwrap(),
                        ),
                        (
                            cap[4].parse::<cube_t>().unwrap(),
                            cap[5].parse::<cube_t>().unwrap(),
                        ),
                        (
                            cap[6].parse::<cube_t>().unwrap(),
                            cap[7].parse::<cube_t>().unwrap(),
                        ),
                    ),
                );
            }
            unreachable!("aaaaaah")
        })
        .collect()
}

fn part1(cmds: &Vec<Command>) -> u32 {
    let mut set: Set = HashSet::new();
    let r = 50;
    for &Command(onoff, ((x1_, x2_), (y1_, y2_), (z1_, z2_))) in cmds {
        let x1 = x1_.max(-r);
        let x2 = x2_.min(r);
        let y1 = y1_.max(-r);
        let y2 = y2_.min(r);
        let z1 = z1_.max(-r);
        let z2 = z2_.min(r);
        for i in x1..=x2 {
            for j in y1..=y2 {
                for k in z1..=z2 {
                    if onoff {
                        set.insert((i, j, k));
                    } else {
                        set.remove(&(i, j, k));
                    }
                }
            }
        }
    }

    set.len() as u32
}

fn part2(cmds: &Vec<Command>) -> u64 {
    let mut alternating_unions: LinkedList<Vec<usize>> = LinkedList::new();
    let lookup = cmds.iter().map(|&Command(_, c)| c).collect::<Vec<_>>();
    for (i, &Command(on, _)) in cmds.iter().enumerate() {
        for v in alternating_unions.iter_mut() {
            v.push(i);
        }
        if (alternating_unions.len() % 2 == 0) == on {
            // A new union term is added if "on" and even, or "off" and odd.
            let mut v = vec![i];
            v.reserve(cmds.len() - i);
            alternating_unions.push_back(v);
        }
    }

    fn eval_union(lookup: &Vec<Cuboid>, add: bool, int: Cuboid, u: &[usize]) -> i64 {
        u.iter()
            .enumerate()
            .map(|(i, &tag)| match intersect(&int, &lookup[tag]) {
                Some(next_int) => {
                    let v = eval_cuboid(&next_int) as i64;
                    let rest = eval_union(lookup, !add, next_int, &u[i + 1..]);
                    if add {
                        rest + v
                    } else {
                        rest - v
                    }
                }
                None => 0,
            })
            .sum::<i64>()
    }

    let scope = ((-200000, 200000), (-200000, 200000), (-200000, 200000));
    alternating_unions
        .iter()
        .enumerate()
        .map(|(i, u)| eval_union(&lookup, i % 2 == 0, scope, &u[..]))
        .sum::<i64>() as u64
}

fn part2_opt(cmds: &Vec<Command>) -> u64 {
    // Optimised space-complexity.
    let mut alternating_unions: LinkedList<usize> = LinkedList::new();
    let lookup = cmds.iter().map(|&Command(_, c)| c).collect::<Vec<_>>();
    for (i, &Command(on, _)) in cmds.iter().enumerate() {
        if (alternating_unions.len() % 2 == 0) == on {
            // A new union term is added if "on" and even, or "off" and odd.
            alternating_unions.push_back(i);
        }
    }

    fn eval_union(lookup: &Vec<Cuboid>, add: bool, int: Cuboid, from: usize) -> i64 {
        (from..lookup.len())
            .map(|tag| match intersect(&int, &lookup[tag]) {
                Some(next_int) => {
                    let v = eval_cuboid(&next_int) as i64;
                    let rest = eval_union(lookup, !add, next_int, tag + 1);
                    if add {
                        rest + v
                    } else {
                        rest - v
                    }
                }
                None => 0,
            })
            .sum::<i64>()
    }

    let scope = ((-200000, 200000), (-200000, 200000), (-200000, 200000));
    alternating_unions
        .iter()
        .enumerate()
        .map(|(i, u)| eval_union(&lookup, i % 2 == 0, scope, *u))
        .sum::<i64>() as u64
}

fn part2_opt2(cmds: &Vec<Command>) -> u64 {
    // Here we optimise for computations. Instead of repeating computations, we reuse previous computations.
    // n=4, [ 0, 2, 3 ] ==> |L[0] u L[1] u L[2] u L[3]| - |L[2] u L[3]| + |L[3]|
    // Instead of computing each term individually, we compute |L[3]| first, use the result in |L[2] u L[3]|,
    // and finally use _that_ result in |L[0] u L[1] u L[2] u L[3]|.
    let mut alternating_unions: LinkedList<usize> = LinkedList::new();
    let lookup = cmds.iter().map(|&Command(_, c)| c).collect::<Vec<_>>();
    for (i, &Command(on, _)) in cmds.iter().enumerate() {
        if (alternating_unions.len() % 2 == 0) == on {
            // A new union term is added if "on" and even, or "off" and odd.
            alternating_unions.push_back(i);
        }
    }

    /*
    f(A, B + C - (B n C), |B| + |C| - |B n C|):
        (
            B + C - (B n C) + A - A n B - A n C + A n B n C,
            |.| + |A| - |A n .|
        )
    */
    let n = lookup.len();
    let mut set: LinkedList<(bool, Cuboid)> = LinkedList::new();
    let mut sum = 0i64;
    let mut val = 0i64;
    let mut prev_u = n;
    for &u in alternating_unions.iter().rev() {
        for i in u..prev_u {
            let mut intersects = set
                .iter()
                .map(|(add, s)| (!add, intersect(s, &lookup[i])))
                .filter(|(_, x)| x.is_some())
                .map(|(b, x)| (b, x.unwrap()))
                .collect::<LinkedList<_>>();
            val += eval_cuboid(&lookup[i]) as i64;
            val += intersects
                .iter()
                .map(|(add, c)| (if *add { 1 } else { -1 }) * eval_cuboid(c) as i64)
                .sum::<i64>();
            set.push_back((true, lookup[i]));
            set.append(&mut intersects);
        }
        prev_u = u;
        sum = val - sum;
    }
    sum as u64
}

fn eval_cuboid(((x1, x2), (y1, y2), (z1, z2)): &Cuboid) -> u64 {
    ((x2 - x1 + 1) * (y2 - y1 + 1) * (z2 - z1 + 1)) as u64
}

fn intersect((x1, y1, z1): &Cuboid, (x2, y2, z2): &Cuboid) -> Option<Cuboid> {
    fn range_intersect(
        &(a1, a2): &(cube_t, cube_t),
        &(b1, b2): &(cube_t, cube_t),
    ) -> Option<(cube_t, cube_t)> {
        if a2 < b1 || b2 < a1 {
            None
        } else if a1 <= b1 && b2 <= a2 {
            Some((b1, b2))
        } else if b1 <= a1 && a2 <= b2 {
            Some((a1, a2))
        } else if a1 <= b1 && a2 <= b2 {
            Some((b1, a2))
        } else if b1 <= a1 && b2 <= a2 {
            Some((a1, b2))
        } else {
            None
        }
    }
    match (
        range_intersect(x1, x2),
        range_intersect(y1, y2),
        range_intersect(z1, z2),
    ) {
        (Some(x), Some(y), Some(z)) => Some((x, y, z)),
        _ => None,
    }
}
