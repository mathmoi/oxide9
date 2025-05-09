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

### Running with Cargo
You can run the engine directly with Cargo:
```bash
cargo run --release -- help
```

### Running the Binary
If you want to run the binary directly (for example, when connecting to a chess GUI), simply copy the binary from `target/release/oxide9` to your desired location.


### Building and running the Docker image to play on Lichess
You first need to add your OAuth token in the [config.yml](lichess-bot/config.yml) file. Then, from the root of the repository, run the following commands:

```bash
docker build . -f lichess-bot/Dockerfile -t o9:latest
docker run -d --restart=always --name o9 o9
```

## Play online
If you want to play against Oxide9, but don't want to install it on your computer, you can usually find it on Lichess under the name [Oxide9](https://lichess.org/@/oxide9).

## Change log

The change log can be found in the [CHANGELOG.md](CHANGELOG.md) file.

### Versionning

Oxide9 version number use the following format: [MAJOR].[MINOR].[PATCH] (example: 1.2.3).

- MAJOR is incremented for each releases intended for end-users. For this reasons users will generally used version in the format vX.0 where 'X' is the Releases number.
- MINOR is incremented each time a new features is implemented, an existing features is improved or removed. Version where the MINOR value is not '0' are considered development version and should not be used by end-users.
- PATCH is optional. It is present and incremented when changes are made without significantly modifying the features of the engine.

## License

Oxide9 is licensed under a simple source available [license](LICENSE).