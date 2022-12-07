use itertools::Itertools;
use regex::Regex;
use std::fs;

type State = Vec<Vec<char>>;

#[derive(Debug, Clone, Copy)]
struct Command {
    num: usize,
    from: usize,
    to: usize,
}

fn main() {
    let filename = "../input/d05.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents);
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

fn parse(contents: String) -> (State, Vec<Command>) {
    let (st_str, cmds_str) = contents.split_once("\n\n").unwrap();
    let mut ls = st_str.lines().rev();
    // ls.next();
    let labels_str = ls.next().unwrap();
    let labels: Vec<char> = labels_str.chars().skip(1).step_by(4).collect();

    let to_index = |label: char| labels.iter().position(|&l| l == label).unwrap();

    let mut st = State::new();
    st.resize(labels.len(), Vec::new());

    for line in ls {
        for (i, c) in line.chars().skip(1).step_by(4).enumerate() {
            if c != ' ' {
                st[i].push(c);
            }
        }
    }

    let re = Regex::new(r"move (\d+) from (\d+) to (\d+)").unwrap();
    let cmds = cmds_str
        .split("\n")
        .map(|s| {
            for cap in re.captures_iter(s) {
                return Command {
                    num: cap[1].parse().unwrap(),
                    from: to_index(cap[2].chars().nth(0).unwrap()),
                    to: to_index(cap[3].chars().nth(0).unwrap()),
                };
            }
            panic!("aaaah!")
        })
        .collect();

    (st, cmds)
}

fn part1((st, cmds): &(State, Vec<Command>)) -> String {
    let mut st1 = st.clone();
    cmds.iter().for_each(|Command { num, from, to }| {
        (0..*num).for_each(|_| {
            let x = st1[*from]
                .pop()
                .expect("expected char from stack, but got nothing");
            st1[*to].push(x);
        })
    });
    st1.iter().map(|stk| *stk.last().unwrap()).join("")
}

fn part2((st, cmds): &(State, Vec<Command>)) -> String {
    let mut st1 = st.clone();
    cmds.iter().for_each(|Command { num, from, to }| {
        let chop = st1[*from].len() - num;
        let copy = st1[*from].clone();
        st1[*to].extend(&copy[chop..]);
        st1[*from].truncate(chop);
    });
    st1.iter().map(|stk| *stk.last().unwrap()).join("")
}
