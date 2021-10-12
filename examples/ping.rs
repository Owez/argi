//! Ping pong

use argi::cli;

fn main() {
    cli! {
        ping [text]: {
            help: "Returns the message you sent",
            run: (|_, ping| println!("{}", ping.unwrap()))
        }
    }
    .launch()
}
