//! ```none
//! Usage: basic [OPTIONS]
//!
//!   Demo command-line utility
//!
//! Options:
//!   No commands or arguments found
//! ```

use argi::cli;

fn main() {
    cli!(
        help: "Demo command-line utility",
        run: (|_, _| println!("Hello, world!"))
    )
    .launch();
}
