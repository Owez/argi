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
argi = "0.1.0-beta.3"
```

## Examples 

This example simply prints the time-tested hello world message:

```rust
use argi::cli;

fn main() {
    cli! {
        help: "Demo command-line utility",
        run: (|_, _| println!("Hello, world!"))
    }
    .launch();
}
```

(Generating [this](https://github.com/Owez/argi/blob/master/examples/basic_help.txt) help page)

This example is a more complex command-line interface which pretends to launch a website via the use of arguments:

```rust
use argi::{cli, data};

fn main() {
    cli! {
        help: "Demo application which launches something",
        run: (|ctx, _| {
            let addr = data!(ctx => --address).unwrap();
            let port = data!(u16, ctx => --port).unwrap();
            println!("Address found: {}\nPort found: {}", addr, port);
        }),
        --address -a [text]: {
            help: "Address to bind to",
        },
        --port -p [port]: {
            help: "Port number from 0 to 65535",
        }
    }
    .launch();
}
```

(Generating [this](https://github.com/Owez/argi/blob/master/examples/pretend_website_help.txt) help page)

You can find more of the examples shown below within the useful [`examples/`](https://github.com/Owez/argi/tree/master/examples) directory.

## Licensing

This project is dual-licensed under both MIT and Apache, so feel free to use either at your discretion. Links to the files are listed below:

- [MIT](LICENSE-MIT)
- [Apache](LICENSE-APACHE)
