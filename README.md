# argi

Argument parsing for the future 🚀

## Features

- Macro-based approach, providing an intuitive way to layout a cli
- Rich auto-help generation, styling based off of python's [`click`](https://click.palletsprojects.com/en/8.0.x/) library
- Zero dependencies
- Speedy compile times
- Low binary bloat

This library is intended to be a replacement for both heavier [`structopt`](https://crates.io/crates/structopt)/[`clap`](https://crates.io/crates/clap)-based interfaces due to amount of features contained within, as well as slim ones based upon [`pico-args`](https://crates.io/crates/pico-args) due to the small workflow and compilation footprint. As this crate isn't fully battle-tested yet, smaller programs are a prime contender for usage with this library.

## Usage

Place the following into your `Cargo.toml` file:

```toml
[dependencies]
argi = "0.1.0-beta.5"
```

## Example

Complete demo ([help page](https://github.com/Owez/argi/blob/master/examples/irl_help.txt)):

```rust
use argi::{cli, data};

fn main() {
    cli!(
        help: "Imaginary webapp program, allowing simple tasks",
        launch: {
            help: "Launches instance to given address",
            run: (|ctx, _| todo!("Launch at port {:?}", data!(ctx => --port))),
            --db [url]: { help: "Database URL" },
            --bind [url]: { help: "Binding address" },
            --port [port]: { help: "Port to hook onto" },
        },
        delete [int?]: {
            help: "Deletes an optional id from the database",
            run: (|_, data| todo!("Delete {:?}", data))
        }
    )
    .launch();
}
```

You can find more of the examples shown below within the useful [`examples/`](https://github.com/Owez/argi/tree/master/examples) directory!

## Licensing

This project is dual-licensed under both MIT and Apache, so feel free to use either at your discretion. Links to the files are listed below:

- [MIT](LICENSE-MIT)
- [Apache](LICENSE-APACHE)
