# argi

Argument parsing for the future 🚀

## Status

This package is now in beta (`0.1.0-beta.2`), allowing developers to get a preliminary look at the features this library has to offer. The main missing section of argi is documentation and minor polish, but no promises on api compatibility!

## Examples 

You can find more of the examples shown below within the useful [`examples`](examples/) directory.

### Basic

This example simply prints the time-tested hello world message:

```rust
use argi::cli;

fn main() {
    cli! {
        help: "Demo command-line utility",
        run: (|_, _| println!("Hello, world!"))
    }
    .launch()
}
```

The top-level help message for this example looks like:

```none
Usage: basic [OPTIONS]

  Demo command-line utility

Options:
  No commands or arguments found
```

### Pretend website

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

The top-level help message for this example looks like:

```none
Usage: pretend_website [OPTIONS]

  Demo application which launches something

Arguments:
  -a --address [text]    Address to bind to
  -p --port [port]       Port number from 0 to 65535
```

## Usage

Place the following into your `Cargo.toml` file:

```toml
[dependencies]
argi = "0.1.0-beta.1"
```

## Licensing

This project is dual-licensed under both MIT and Apache, so feel free to use either at your discretion. Links to the files are listed below:

- [MIT](LICENSE-MIT)
- [Apache](LICENSE-APACHE)
