//! ```none
//! Usage: irl [OPTIONS]
//!
//!   Imaginary webapp program, allowing simple tasks
//!
//! Commands:
//!   launch           Launches instance to given address
//!   delete [int?]    Deletes an optional id from the database
//! ```

use argi::{cli, data, Command};
use std::env;

/// Program-wide configuration values, taken from a mix of cli arguments and environment variables
pub struct Config {
    /// URL for an imaginary database
    pub db_url: String,
    /// Address to bind an instance to
    pub bind_addr: String,
    /// Port to bind an instance to
    pub port: u16,
}

impl Config {
    /// Creates a new configuration instance
    pub fn new(ctx: &Command) -> Result<Self, env::VarError> {
        let db_url = match data!(ctx => --db) {
            Some(val) => val,
            None => env::var("DB_URL")?,
        };
        let bind_addr = match data!(ctx => --bind) {
            Some(val) => val,
            None => env::var("BIND_ADDR")?,
        };
        let port = match data!(u16, ctx => --port) {
            Some(val) => val,
            None => env::var("BIND_PORT")?.parse().unwrap(),
        };

        Ok(Self {
            db_url,
            bind_addr,
            port,
        })
    }
}

/// Launches webapp when asked to by the command
fn launch(ctx: &Command, _: Option<String>) {
    let config = Config::new(ctx).expect("Couldn't create config");

    println!(
        "Launching instance here at http://{}/ address",
        config.bind_addr
    );
}

fn main() {
    cli!(
        help: "Imaginary webapp program, allowing simple tasks",
        launch: {
            help: "Launches instance to given address",
            run: (launch),
            --db [url]: { help: "Database URL" },
            --bind [url]: { help: "Binding address" },
            --port [port]: { help: "Port to hook onto" },
        },
        delete [text?]: {
            help: "Performs the delete operation upon data",
            run: (|_, data| println!("Deleting {:?} item", data))
            all: { help: "Deletes everything" }
        }
    )
    .launch();
}
