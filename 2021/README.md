## 2021 AOC Challenges

Attempted to solve through a combination of Haskell, Rust, and maybe more.

To run haskell programs:

```sh
cd haskell
stack run d01
```

To run rust programs:

```sh
cd rust
cargo run --release --bin d01
```

With haskell programs you can specify options:
```sh
stack run d01 -- -f ~/input.txt # Run with other input files.
stack run d01 -- p1 # Run only part 1.
```

By default, both parts 1 and 2 are run.

N.B. Haskell Stack will [compile all executables in one go](https://github.com/commercialhaskell/stack/issues/1406), so it may be a bit slow. To speed up compilation, comment the unneeded executables in package.yaml.