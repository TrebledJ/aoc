## 2021 AOC Challenges

Attempted to solve through a combination of Haskell, Rust, and maybe more.

### Haskell
To run haskell programs:
```sh
cd haskell
stack run -- -d 1
```

Run with a custom input file:
```sh
stack run -- -d 1 -f ~/input.txt # Run day 01 with other input files.
```

Run only part1:
```sh
stack run -- -d 1 p1
```

(By default, both parts 1 and 2 are run.)

Bench part1 and part2:
```sh
stack run -- -d 1 --bench
```

If no day (`-d`) is specified, the program will run the first day by default.

To run test suites:
```sh
stack test
stack test --test-arguments "-m D22"  # Individual test suite.
```

To run dedicated benchmarks:
```sh
stack bench
```

By default, both parts 1 and 2 are run.

<!-- N.B. Haskell Stack will [compile all executables in one go](https://github.com/commercialhaskell/stack/issues/1406), so it may be a bit slow. To speed up compilation, comment the unneeded executables in package.yaml. -->

### Rust
To run rust programs:

```sh
cd rust
cargo run --release --bin d01
```
