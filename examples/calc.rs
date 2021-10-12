//! Slightly larger demo, showing more advanced data fetching

use argi::{cli, data};

fn main() {
    cli! {
        help: "Calculates two values given an operation, adds by default",
        add: {
            help: "Adds two numbers together from left and right",
            run: (|ctx, _| println!("Add: {}", data!(i32, ctx => -l) + data!(i32, ctx => -r))),
            -l --left [int]: {},
            -r --right [int]: {}
        }
        div: {
            help: "Divides two numbers together from left and right",
            run: (|ctx, _| println!("Add: {}", data!(i32, ctx => -l) / data!(i32, ctx => -r))),
            -l --left [int]: {},
            -r --right [int]: {},
        },
    }
    .launch();
}
