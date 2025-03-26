# Oxide9

<p align="center">
  <img src="https://github.com/mathmoi/oxide9/blob/master/assets/images/Oxide9-large.png?raw=true" alt="m8">
</p>

Oxide9 is a UCI-compatible chess engine written in Rust by [Mathieu Pagé](https://www.mathieupage.com). It can be used with most chess interfaces to analyze positions or play games.

## Installation
### Prerequisites
- [git](https://git-scm.com/)
- [Rust and Cargo (Rust's package manager)](https://www.rust-lang.org/tools/install)

### Building from Source
```bash
git clone https://github.com/mathmoi/oxide9.git
cd oxide9
cargo build --release
```
The executable will be located at `target/release/oxide9`.

## Usage
### Running with Cargo
You can run the engine directly with Cargo:
```bash
cargo run --release -- help
```

### Running the Binary
If you want to run the binary directly (for example, when connecting to a chess GUI):

1. Copy the binary from `target/release/oxide9` to your desired location
2. Make sure to also copy the [oxide9.toml](assets/config/oxide9.toml) configuration file to the same directory as the binary

## License

Oxide9 is licensed under a simple source available [license](LICENSE).