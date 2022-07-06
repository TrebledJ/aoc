/**
 * Terminology.
 * 
 * scs: scanners
 * scoff: scanner offset
 * bacon: beacon
 * ebs: edge beacon (beacons supposedly on the fringes of explored territory)
 * newbs: this works on multiple levels. new bacons, new beacons, newbie beacons.
 */
use std::collections::HashMap;
use std::collections::HashSet;
use std::fs;
use std::ops;
use itertools::iproduct;


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
    fn manhattan(self, rhs: Pos) -> u32 {
        ((self.0 - rhs.0).abs() + (self.1 - rhs.1).abs() + (self.2 - rhs.2).abs()) as u32
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
    let scs = parse(contents);

    let (p1, scoffs) = part1(&scs);
    println!("part1: {}", p1);
    println!("part2: {}", part2(&scoffs));
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
 * 
 * Returns (part1 answer, scanner offsets in order).
 */
fn part1(scs: &Vec<Vec<Pos>>) -> (u32, Vec<Pos>) {
    fn try_intersect(sc: &Vec<Pos>, ebs: &HashMap<Pos, HashSet<Pos>>, rots: &Vec<Rot>) -> Option<(HashSet<Pos>, Pos, Pos)> {
        for b in sc {
            let boff = offsets(sc, *b);
            for (eb, ebset) in ebs.clone() {
                for r in rots {
                    let rboff = boff.iter().map(|b2| b2.rotate(r)).collect::<HashSet<_>>();
                    let inter = rboff.intersection(&ebset).collect::<HashSet<_>>();
                    if inter.len() >= 12 {
                        let scoff = eb - b.rotate(r);
                        return Some((offsets(rboff.iter(), -eb), scoff, eb));
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
    let mut scoffs = vec![Pos(0, 0, 0); scs.len()];
    while merged.len() != scs.len() {
        for (i, sc) in scs.iter().enumerate() {
            if merged.contains(&i) {
                continue;
            }

            match try_intersect(sc, &ebs, &rots) {
                Some((newbs, scoff, eb)) => {
                    scoffs[i] = scoff;
                    ebs.remove(&eb); // Remove edge bacon since, it likely won't be used again.
                    bacon.extend(newbs.clone());
                    ebs.extend(edge_beacons(newbs.iter())); // Add new edge beacons from current scanner.
                    merged.insert(i);
                    println!("({}/{}) merged scanner {i} at offset {:?}", merged.len(), scs.len(), scoff);
                },
                None => ()
            }
        }
    }

    (bacon.len() as u32, scoffs)
}

fn part2(scoffs: &Vec<Pos>) -> u32 {
    iproduct!(scoffs, scoffs.clone().iter().skip(1)).map(|(&a, &b)| a.manhattan(b)).max().unwrap()
}

/**
 * Returns a map of (edge beacon -> offset set).
 */
fn edge_beacons<'a, I>(v: I) -> HashMap<Pos, HashSet<Pos>> 
    where I: IntoIterator<Item=&'a Pos>, I: Clone
{
    let edges = vec![
        *v.clone().into_iter().min_by(|u, v| u.0.cmp(&v.0)).unwrap(),
        *v.clone().into_iter().max_by(|u, v| u.0.cmp(&v.0)).unwrap(),
        *v.clone().into_iter().min_by(|u, v| u.1.cmp(&v.1)).unwrap(),
        *v.clone().into_iter().max_by(|u, v| u.1.cmp(&v.1)).unwrap(),
        *v.clone().into_iter().min_by(|u, v| u.2.cmp(&v.2)).unwrap(),
        *v.clone().into_iter().max_by(|u, v| u.2.cmp(&v.2)).unwrap(),
    ];
    edges.iter().map(|eb| (*eb, offsets(v.clone(), *eb)))
        .collect()
}

fn offsets<'a>(v: impl IntoIterator<Item=&'a Pos>, p: Pos) -> HashSet<Pos> {
    v.into_iter().map(|q| *q - p).collect()
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
