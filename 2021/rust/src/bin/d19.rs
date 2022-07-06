use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::ops;
use std::ops::Sub;

#[derive(Debug, Clone)]
struct Rot(i32, i32, i32);

#[derive(Debug, Eq, PartialEq, Hash, Copy, Clone)]
struct Pos(i32, i32, i32);
impl Pos {
    fn rotate(&self, &Rot(rx, ry, rz): &Rot) -> Pos {
        let select = |i: i32| {
            (match i.abs() {
                1 => self.0,
                2 => self.1,
                3 => self.2,
                _ => panic!("aaah!"),
            }) * i.signum()
        };
        Pos(select(rx), select(ry), select(rz))
    }
}
impl ops::Add<Pos> for Pos {
    type Output = Pos;
    fn add(self, pos: Pos) -> Pos {
        Pos(self.0 + pos.0, self.1 + pos.1, self.2 + pos.2)
    }
}
impl ops::Sub<Pos> for Pos {
    type Output = Pos;
    fn sub(self, pos: Pos) -> Pos {
        Pos(self.0 - pos.0, self.1 - pos.1, self.2 - pos.2)
    }
}
impl ops::Neg for Pos {
    type Output = Pos;
    fn neg(self) -> Pos {
        Pos(-self.0, -self.1, -self.2)
    }
}

fn main() {
    let filename = "../input/d19.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let stuff = parse(contents);

    println!("part1: {}", part1(&stuff));
    println!("part2: {}", part2(&stuff));
}

fn parse(contents: String) -> Vec<Vec<Pos>> {
    contents
        .split("\n---")
        .map(|sc| {
            sc.lines()
                .skip(1)
                .filter(|s| !s.is_empty())
                .map(|l| {
                    l.split(",")
                        .map(|n| n.parse::<i32>().unwrap())
                        .collect::<Vec<_>>()
                })
                .map(|v| Pos(v[0], v[1], v[2]))
                .collect()
        })
        .collect()
}

/**
 * Approach.
 *
 * For scanner 0...
 *  - Find the 6 beacons farthest away from the current scanner in each direction (+-x, +-y, +-z).
 *      - Call these beacons `eb`, edge beacons.
 *  - For each of these 6 beacons, compute the offset between this beacon and the rest.
 *  - (opt.) Order offsets by magnitude to sort by distance.
 *  - (opt.) Take half of the points nearest to the edge beacon.
 *
 * Check if scanner N overlaps with scanner 0...
 *  - For each beacon `b` in scanner N...
 *      - Calculate offsets `boff` from `b` to the rest of scanner N beacons.
 *      - For each `si` in the 6 scanner-0 offset-sets...
 *          - For each of the 24 possible rotations `r`...
 *              - Apply rotation `r` on set `si` to get `ri`.
 *              - Intersect `ri` with `boff`.
 *              - If the number of elements in the intersection exceed 12, then merge scanner N to scanner 0.
 *              
 *              - Merge scanner-N to 0...
 *                  - Offset beacons in scanner N to scanner 0 coords by applying the inverse rotation `invr`.
 *                  - Apply the translation `eb` - `b` to scanner-N beacons.
 *                  - Add those beacons to scanner-0's set.
 *                  - Compute the edge beacons and associated offsets of scanner-N (just like we did for scanner 0) and add them to the total set of edge beacons and offsets.
 */
fn part1(scs: &Vec<Vec<Pos>>) -> u32 {
    fn try_intersect(sc: &Vec<Pos>, ebs: &HashMap<Pos, HashSet<Pos>>, rots: &Vec<Rot>) -> Option<HashSet<Pos>> {
        for b in sc {
            let boff = offsets(sc, *b);
            for (eb, ebset) in ebs.clone() {
                for r in rots {
                    let rboff = boff.iter().map(|b2| b2.rotate(r)).collect::<HashSet<_>>();
                    let inter = rboff.intersection(&ebset).collect::<HashSet<_>>();
                    if inter.len() >= 12 {
                        println!("found match with scanner offset {:?}", eb - b.rotate(r));
                        return Some(offsets(&Vec::from_iter(rboff.iter().cloned()), -eb));
                    }
                }
            }
        }
        None
    }

    let rots = rotations();
    let mut merged = HashSet::from([0usize]);
    let mut bacon: HashSet<Pos> = HashSet::from_iter(scs[0].iter().cloned());
    let mut ebs = edge_beacons(&scs[0]);
    while merged.len() != scs.len() {
        for (i, sc) in scs.iter().enumerate() {
            if merged.contains(&i) {
                continue;
            }

            println!("checking scanner {i}");
            match try_intersect(sc, &ebs, &rots) {
                Some(newbs) => {
                    bacon.extend(newbs.clone());
                    ebs.extend(edge_beacons(&Vec::from_iter(newbs.iter().cloned()))); // Add new edge beacons from current scanner.
                    merged.insert(i);
                },
                None => ()
            }
        }
    }

    bacon.len() as u32
}

fn test() {
    let p1 = Pos(-618, -824, -621);
    let p2 = Pos(-537, -823, -458);
    let p12 = Pos(686, 422, 578);
    let p22 = Pos(605, 423, 415);
    let p3 = p2 - p1;
    let p32 = p22 - p12;
    println!("{:?}\t{:?}", p3, p32);
    for r in rotations() {
        println!("{:?}", p32.rotate(&r));
    }
}

fn part2(input: &Vec<Vec<Pos>>) -> u32 {
    0
}

/**
 * Returns a map of (edge beacon -> offset set).
 */
fn edge_beacons(v: &Vec<Pos>) -> HashMap<Pos, HashSet<Pos>> {
    let edges = vec![
        *v.iter().min_by(|u, v| u.0.cmp(&v.0)).unwrap(),
        *v.iter().max_by(|u, v| u.0.cmp(&v.0)).unwrap(),
        *v.iter().min_by(|u, v| u.1.cmp(&v.1)).unwrap(),
        *v.iter().max_by(|u, v| u.1.cmp(&v.1)).unwrap(),
        *v.iter().min_by(|u, v| u.2.cmp(&v.2)).unwrap(),
        *v.iter().max_by(|u, v| u.2.cmp(&v.2)).unwrap(),
    ];
    edges.iter().map(|eb| (*eb, offsets(v, *eb))).collect()
}

fn offsets(v: &Vec<Pos>, p: Pos) -> HashSet<Pos> {
    v.iter().map(|q| *q - p).collect()
}

fn rotations() -> Vec<Rot> {
    let v = vec![Rot(1, 2, 3), Rot(1, -2, -3), Rot(1, 3, -2), Rot(1, -3, 2)];
    let mut w = v.clone();
    w.extend(v.iter().map(|Rot(a, b, c)| Rot(*b, *c, *a)));
    w.extend(v.iter().map(|Rot(a, b, c)| Rot(*c, *a, *b)));
    let mut y = w.clone();
    y.extend(w.iter().map(|Rot(a, b, c)| Rot(-a, -c, -b)));
    y
}
