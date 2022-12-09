use std::fs;
use itertools::Itertools;

#[derive(PartialEq, Debug)]
struct Dir {
    name: String,
    contents: Vec<FS>,
}

#[derive(PartialEq, Debug)]
struct File {
    name: String,
    size: u32,
}

#[derive(PartialEq, Debug)]
enum FS {
    Dir(Dir),
    File(File),
}

impl FS {
    fn as_dir(&self) -> &Dir {
        match self {
            FS::Dir(x) => x,
            _ => panic!("not a dir")
        }
    }

    fn as_file(&self) -> &File {
        match self {
            FS::File(x) => x,
            _ => panic!("not a file")
        }
    }

    fn as_dir_mut(&mut self) -> &mut Dir {
        match self {
            FS::Dir(x) => x,
            _ => panic!("not a dir")
        }
    }

    fn as_file_mut(&mut self) -> &mut File {
        match self {
            FS::File(x) => x,
            _ => panic!("not a file")
        }
    }
}

fn main() {
    let filename = "../input/d07.txt";

    let contents = fs::read_to_string(filename).unwrap();
    let xs = parse(contents);
    println!("part1: {}", part1(&xs));
    println!("part2: {}", part2(&xs));
}

// Jank AST parsing code.
fn parse(contents: String) -> Dir {
    let mut root = Dir {
        name: "/".to_string(),
        contents: Vec::new(),
    };
    let mut cwds: Vec<&str> = Vec::new();

    fn find_dir(Dir { name: _, contents }: &mut Dir, dname: String) -> &mut Dir {
        contents
            .iter_mut()
            .find(|x| match x {
                FS::Dir(Dir { name, contents: _ }) => *name == dname,
                _ => false,
            })
            .unwrap_or_else(|| panic!("could not find dirname: {}", dname))
            .as_dir_mut()
    }

    for line in contents.lines() {
        if line.starts_with("$ ") {
            let cmd: Vec<&str> = line.strip_prefix("$ ").unwrap().split(" ").collect();
            match cmd[0] {
                "cd" => {
                    if cmd[1] == "/" {
                        // Ignore.
                    } else if cmd[1] == ".." {
                        cwds.pop();
                    } else {
                        cwds.push(cmd[1]);
                    }
                }
                "ls" => {} // Ignore.
                s => panic!("unknown command: {}", s),
            }
        } else {
            let fs = if line.starts_with("dir") {
                let dname = line.strip_prefix("dir ").unwrap();
                FS::Dir(Dir {
                    name: dname.to_string(),
                    contents: Vec::new(),
                })
            } else {
                let file: Vec<&str> = line.split(" ").collect();
                let (size, fname) = (file[0].parse::<u32>().unwrap(), file[1]);
                FS::File(File {
                    name: fname.to_string(),
                    size,
                })
            };

            let Dir { name: _, contents } = cwds
                .iter()
                .fold(&mut root, |acc, x| find_dir(acc, x.to_string()));
            contents.push(fs);
        }
    }
    root
}

fn get_directory_sizes(dir: &Dir) -> Vec<u32> {
    // Returns total size of current directory, and sizes of subdirectories.
    fn visit(Dir { name: _, contents }: &Dir) -> (u32, Vec<u32>) {
        let mut v = Vec::new();
        let mut folder_total = 0;
        for x in contents {
            match x {
                FS::File(File { name: _, size }) => {
                    folder_total += *size;
                }
                FS::Dir(dir) => {
                    let (total, mut subdirs) = visit(dir);
                    folder_total += total;
                    v.append(&mut subdirs);
                }
            }
        }
        v.push(folder_total);
        (folder_total, v)
    }
    visit(dir).1
}

fn part1(fs: &Dir) -> u32 {
    get_directory_sizes(fs).iter().filter(|&&x| x <= 100000).sum()
}

fn part2(fs: &Dir) -> u32 {
    let total_space = 70000000;
    let target_space = 30000000;
    let sizes = get_directory_sizes(fs);
    let curr_used = sizes.last().unwrap();
    *sizes.iter().sorted().filter(|&&x| total_space - curr_used + x >= target_space).next().unwrap()
}
