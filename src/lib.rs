//! Argument parsing for the future 🚀
//!
//! ## Features
//!
//! - Macro-based approach, providing an intuitive way to layout a cli
//! - Rich auto-help generation, styling based off of python's [`click`](https://click.palletsprojects.com/en/8.0.x/) library
//! - Zero dependencies
//! - Speedy compile times
//! - Low binary bloat
//!
//! This library is intended to be a replacement for both heavier [`structopt`](https://crates.io/crates/structopt)/[`clap`](https://crates.io/crates/clap)-based interfaces due to amount of features contained within, as well as slim ones based upon [`pico-args`](https://crates.io/crates/pico-args) due to the small workflow and compilation footprint. As this crate isn't fully battle-tested yet, smaller programs are a prime contender for usage with this library.
//!
//! ## Usage
//!
//! Place the following into your `Cargo.toml` file:
//!
//! ```toml
//! [dependencies]
//! argi = "0.1.0-beta.4"
//! ```
//!
//! ## Examples
//!
//! This example simply prints the time-tested hello world message:
//!
//! ```rust
//! use argi::cli;
//!
//! fn main() {
//!     cli!(
//!         help: "Demo command-line utility",
//!         run: (|_, _| println!("Hello, world!"))
//!     )
//!     .launch();
//! }
//! ```
//!
//! (Generating [this](https://github.com/Owez/argi/blob/master/examples/basic_help.txt) help page)
//!
//! This example is a more complex command-line interface which pretends to launch a website via the use of arguments:
//!
//! ```no_run
//! use argi::{cli, data};
//!
//! fn main() {
//!     cli!(
//!         help: "Demo application which launches something",
//!         run: (|ctx, _| {
//!             let addr = data!(ctx => --address).unwrap();
//!             let port = data!(u16, ctx => --port).unwrap();
//!             println!("Address found: {}\nPort found: {}", addr, port);
//!         }),
//!         --address -a [text]: {
//!             help: "Address to bind to",
//!         },
//!         --port -p [port]: {
//!             help: "Port number from 0 to 65535",
//!         }
//!     )
//!     .launch();
//! }
//! ```
//!
//! (Generating [this](https://github.com/Owez/argi/blob/master/examples/pretend_website_help.txt) help page)
//!
//! You can find more of the examples shown below within the useful [`examples`](examples/) directory.
//!
//! ## Licensing
//!
//! This project is dual-licensed under both MIT and Apache, so feel free to use either at your discretion. Links to the files are listed below:
//!
//! - [MIT](LICENSE-MIT)
//! - [Apache](LICENSE-APACHE)

#![deny(unsafe_code)]
#![warn(missing_docs)]
#![allow(clippy::needless_doctest_main)]

mod error;

pub use error::{Error, Result};

use std::io::{self, Write};
use std::{env, fmt, iter::Peekable, process};

/// Whitelisted overriding help targets
const HELP_POSSIBLES: &[&str] = &["--help", "-h", "help"];

/// Gets file name current exe as a string
pub(crate) fn get_cur_exe() -> Result<String> {
    // TODO: `./` if on unix-like?
    // TODO: redo this whole function
    Ok(env::current_exe()
        .map_err(|_| Error::InvalidCurExe)?
        .file_name()
        .ok_or(Error::InvalidCurExe)?
        .to_os_string()
        .into_string()
        .map_err(|_| Error::InvalidCurExe)?
        .trim()
        .to_string())
}

/// Message to display to a user to provide argument or command information
///
/// # Usage
///
/// As a developer, you don't really have to ever manually use this structure
/// as it's simply a public storage item for a help message entered at generation.
/// This can be used to make custom help message formats if wanted.
#[derive(Default)]
pub struct Help<'a>(Option<&'a str>);

impl<'a> From<Option<&'a str>> for Help<'a> {
    fn from(opt_string: Option<&'a str>) -> Self {
        Self(opt_string)
    }
}

impl<'a> From<&'a str> for Help<'a> {
    fn from(string: &'a str) -> Self {
        Self(Some(string))
    }
}

impl<'a> fmt::Display for Help<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Some(help) => write!(f, "{}", help),
            None => Ok(()), // no help provided
        }
    }
}

/// Common methods and data items to all call types
trait CommonInternal<'a> {
    /// Generates leftmost help message
    fn help_left(&self) -> String;
    /// Applies [AfterLaunch]-related tasks
    fn apply_afters(&mut self, data: Option<String>);
}

pub struct Command<'a> {
    pub name: &'a str,
    pub args: Vec<Argument<'a>>,
    pub subcmds: Vec<Command<'a>>,
    // the following values have been flattened into this and argument and function the same, this is for end user ease of use as the cost of us
    /// Indicates if a user has invoked this command at any point
    pub used: bool,
    /// Raw data found from parsing
    pub data: Option<String>,
    // TODO: merge all below with argument by way of a new [Shared] struct, see issue #11 <https://github.com/Owez/argi/issues/11>
    /// Help message, if added, for this argument
    pub help: Help<'a>,
    /// The type of data this argument parses, if any
    pub parses: Option<&'a str>,
    /// Indicates if parsing is an optional endeavour
    pub parses_opt: bool,
    /// User-implemented closure which is ran at parse-time, if added
    pub run: Option<fn(&'_ Self, Option<String>)>,
}

impl<'a> Command<'a> {
    pub fn launch(mut self) -> Self {
        let mut stream = env::args().peekable();
        stream.next();

        if !self.parses_opt && self.run.is_none() && stream.peek().is_none() {
            self.help_err(Error::NothingInputted);
            process::exit(1)
        }

        match self.parse(&mut stream, &mut vec![]) {
            Ok(()) => self,
            Err(err) => {
                self.help_err(err);
                process::exit(1)
            }
        }
    }

    /// Recurses from current command instance horizontally to fetch arguments and downwards to more subcommands
    fn parse(
        &mut self,
        stream: &mut Peekable<impl Iterator<Item = String>>,
        call: &mut Vec<String>,
    ) -> Result<()> {
        let left = match stream.next() {
            Some(val) if HELP_POSSIBLES.contains(&val.as_str()) => {
                self.help(&mut io::stdout())?;
                process::exit(0)
            } // in whitelist, return with help
            Some(val) => val, // good value
            None => {
                return if self.parses.is_some() && !self.parses_opt && self.data.is_none() {
                    Err(Error::DataRequired(call.clone()))
                } else {
                    if let Some(run) = self.run {
                        run(self, None)
                    }
                    Ok(())
                }
            } // end of stream, run and return
        };
        call.push(left.clone()); // add new left to call stream

        if left.starts_with('-') {
            // argument
            self.arg_flow(stream, call, left.clone())?
        } else if let Some(cmd) = self.search_subcmds_mut(&left) {
            // subcommand
            cmd.parse(stream, call)?
        } else if self.parses.is_none() {
            // unwanted data
            return Err(Error::CommandNotFound((left, call.clone())));
        }

        // data for self
        self.apply_afters(Some(left));
        Ok(())
    }

    /// Pathway for arguments which automatically adds data to arguments if found in current instance
    fn arg_flow(
        &mut self,
        stream: &mut Peekable<impl Iterator<Item = String>>,
        call: &mut Vec<String>,
        left: String,
    ) -> Result<()> {
        // get formatted instigator
        let instigator: &str = if let Some(instigator) = left.strip_prefix("--") {
            Ok(instigator)
        } else if let Some(instigator) = left.strip_prefix('-') {
            Ok(instigator)
        } else {
            // errors
            if call.len() >= 3 {
                // maybe data required for arg
                let lasts = &call.iter().rev().take(3).collect::<Vec<&String>>()[1..];
                if lasts[0].starts_with('-') && lasts[1].starts_with('-') {
                    return Err(Error::DataRequiredArg(lasts[1].clone()));
                }
            }

            // arg not found
            Err(Error::ArgumentNotFound((left, call.clone())))
        }?;

        // validation and help
        let mut found = false;
        let mut instant = None;
        for arg in self.args.iter() {
            if arg.instigators.contains(&instigator) {
                found = true;
                match stream.peek() {
                    Some(next) if HELP_POSSIBLES.contains(&next.as_str()) => {
                        stream.next();
                        self.help(&mut io::stdout())?;
                        process::exit(0)
                    } // argument was a help call
                    Some(next) if arg.parses_opt && self.arg_exists(next) => {
                        let next_owned = stream.next().unwrap();
                        call.push(next_owned.clone());
                        instant = Some(next_owned);
                    } // optional argument with data provided, but the data is another valid argument
                    _ => break, // found
                }
            }
        }
        if !found {
            // not found err
            return Err(Error::ArgumentNotFound((
                instigator.to_string(),
                call.clone(),
            )));
        } else if let Some(instant) = instant {
            // parse next argument if current is optional and next is present
            return self.arg_flow(stream, call, instant);
        }

        // mutable data application, due to rust
        for arg in self.args.iter_mut() {
            if arg.instigators.contains(&instigator) {
                if arg.parses.is_some() {
                    match stream.next() {
                        None if !arg.parses_opt => return Err(Error::DataRequired(call.clone())),
                        Some(data) => {
                            call.push(data.clone());
                            arg.apply_afters(Some(data))
                        }
                        got => arg.apply_afters(got),
                    }
                } else {
                    arg.apply_afters(None)
                }
                break;
            }
        }

        // finish up
        match stream.next() {
            Some(left) => {
                call.push(left.clone());
                self.arg_flow(stream, call, left)
            } // next arg
            None => Ok(()), // end
        }
    }

    /// Searches current instance's subcommands for given name
    fn search_subcmds_mut(&mut self, name: &str) -> Option<&mut Command<'a>> {
        for subcmd in self.subcmds.iter_mut() {
            if subcmd.name == name {
                return Some(subcmd);
            }
        }
        None
    }

    /// Checks if an argument exists by its raw instigator value (includes `-` or `--` section)
    fn arg_exists(&self, raw_instigator: &str) -> bool {
        let instigator = if let Some(v) = raw_instigator.strip_prefix("--") {
            v
        } else if let Some(v) = raw_instigator.strip_prefix('-') {
            v
        } else {
            raw_instigator
        };

        for arg in self.args.iter() {
            if arg.instigators.contains(&instigator) {
                return true;
            }
        }
        false
    }

    /// Writes full help message to buffer
    pub fn help(&self, buf: &mut impl Write) -> Result<()> {
        // TODO: multi-line arguments
        // TODO: truncate message if too long
        buf.write_fmt(format_args!(
            "Usage: {} {}{}",
            get_cur_exe()?,
            self.name,
            fmt_parses(&self.parses, self.parses_opt),
        ))?;

        if self.parses.is_some() || !self.name.is_empty() {
            buf.write_all(&[b' '])?;
        }

        buf.write_all(b"[OPTIONS]")?;

        if self.help.0.is_some() {
            buf.write_fmt(format_args!("\n\n  {}", self.help))?;
        }

        /// Automatically pads left and right hand side of help messages together
        fn tab_to<'a>(buf: &mut impl Write, lr: Vec<(String, &Help<'a>)>) -> Result<()> {
            const TAB_SPACE: &str = "    ";

            let mut max = 0;
            for (l, _) in lr.iter() {
                let l_len = l.len();
                if l_len > max {
                    max = l_len
                }
            }

            for (l, r) in lr {
                let padding = " ".repeat(max - l.len());
                buf.write_fmt(format_args!("  {}{}{}{}\n", l, padding, TAB_SPACE, r))?;
            }

            Ok(())
        }

        let mut any = false;

        if !self.subcmds.is_empty() {
            buf.write_all(b"\n\nCommands:\n")?;
            any = true;

            let lr = self
                .subcmds
                .iter()
                .map(|subcmd| (subcmd.help_left(), &subcmd.help))
                .collect();
            tab_to(buf, lr)?;
        }

        if !self.args.is_empty() {
            buf.write_all(if any {
                b"\nArguments:\n"
            } else {
                b"\n\nArguments:\n"
            })?;
            any = true;

            let lr = self
                .args
                .iter()
                .map(|arg| (arg.help_left(), &arg.help))
                .collect();
            tab_to(buf, lr)?;
        }

        if !any {
            buf.write_all(b"\n\nOptions:\n  No commands or arguments found\n")?;
        }

        Ok(())
    }

    /// Prints a provided error to the `stderr` buffer, used in macros so is public but hidden
    #[doc(hidden)]
    pub fn help_err(&self, err: Error) {
        const ERROR: &str = "\nError:\n  ";
        let stderr = io::stderr();
        let mut stderr_lock = stderr.lock();
        match self.help(&mut stderr_lock) {
            Ok(()) => (),
            Err(_) => eprintln!("{}Couldn't generate help for error below!", ERROR),
        }

        // print error
        eprintln!("{}{}!", ERROR, err);
    }
}

impl<'a> CommonInternal<'a> for Command<'a> {
    fn help_left(&self) -> String {
        let mut output = self.name.to_string();
        output.push(' ');
        output.push_str(&fmt_parses(&self.parses, self.parses_opt));
        output
    }

    fn apply_afters(&mut self, data: Option<String>) {
        self.used = true;
        self.data = data.clone(); // keep this above; this should appear pre-done to user
        if let Some(run) = self.run {
            run(self, data)
        }
    }
}

pub struct Argument<'a> {
    /// Calls which can be made which instigate this argument
    pub instigators: &'a [&'a str],
    // the following values have been flattened into this and argument and function the same, this is for end user ease of use as the cost of us
    /// Indicates if a user has invoked this argument at any point
    pub used: bool,
    /// Raw data found from parsing
    pub data: Option<String>,
    // TODO: merge all below with command by way of a new [Shared] struct, see issue #11 <https://github.com/Owez/argi/issues/11>
    /// Help message, if added, for this argument
    pub help: Help<'a>,
    /// The type of data this argument parses, if any
    pub parses: Option<&'a str>,
    /// Indicates if parsing is an optional endeavour
    pub parses_opt: bool,
    /// User-implemented closure which is ran at parse-time, if added
    pub run: Option<fn(&'_ Self, Option<String>)>,
}

impl<'a> CommonInternal<'a> for Argument<'a> {
    fn help_left(&self) -> String {
        let mut output = String::new();
        let mut short = vec![];
        let mut long = vec![];

        for instigator in self.instigators {
            if instigator.len() < 2 {
                short.push(instigator)
            } else {
                long.push(instigator)
            }
        }

        // FIXME: make less janky formats, use write! instead. see issue #3 <https://github.com/Owez/argi/issues/3>
        let mut fmtd: Vec<String> = short.iter().map(|s| format!("-{}", s)).collect();
        fmtd.extend(long.iter().map(|l| format!("--{}", l)));

        output.push_str(fmtd.join(" ").as_str());
        output.push(' ');
        output.push_str(&fmt_parses(&self.parses, self.parses_opt));
        output
    }

    fn apply_afters(&mut self, data: Option<String>) {
        self.used = true;
        self.data = data.clone(); // keep this above; this should appear pre-done to user
        if let Some(run) = self.run {
            run(self, data)
        }
    }
}

/// Used to query if a fuzzy macro section is an argument or not
#[doc(hidden)]
pub trait ArgiIsArg {
    fn is_arg(&self) -> bool;
}

impl ArgiIsArg for Argument<'_> {
    fn is_arg(&self) -> bool {
        true
    }
}

impl ArgiIsArg for Command<'_> {
    fn is_arg(&self) -> bool {
        false
    }
}

fn fmt_parses(parses: &Option<&str>, optional: bool) -> String {
    if let Some(parses) = parses {
        let opt = if optional { "?" } else { "" };
        format!("[{}{}]", parses, opt)
    } else {
        String::new()
    }
}

#[macro_export]
macro_rules! cli {
    // new plain cli
    () => { $crate::Command {
        name: "",
        help: $crate::Help::default(),
        args: vec![],
        subcmds: vec![],
        parses: None,
        parses_opt: false,
        used: false,
        run:None,
        data:None,
    } };
    // configurations for new cli
    ($($tail:tt)*) => {
        {
            let mut cli = $crate::cli!();
            $crate::cli_below!(cli; $($tail)*);
            cli
        }
    };
}

#[macro_export]
macro_rules! get {
    ($ctx:expr =>) => { $ctx };
    ($ctx:expr => --$query:ident $($tail:tt)*) => {
        $crate::get!($ctx.args.iter().find(|e| e.instigators.contains(&stringify!($query))).unwrap() => $($tail)*) // TODO: better error
    };
    ($ctx:expr => -$query:ident $($tail:tt)*) => {
        {
            let name = stringify!($query);
            if name.len() > 1 {
                panic!()
            }
            $crate::get!($ctx => --$query $($tail)*)
        }
    };
    ($ctx:expr => $query:ident $($tail:tt)*) => {
        $crate::get!($ctx.subcmds.iter().find(|e| e.name == stringify!($query)).unwrap() => $($tail)*) // TODO: better error
    }
}

// TODO: optional is gobbling others, need to maybe peek and check
// TODO: rewrite data

#[macro_export]
macro_rules! data {
    // plain (string) data fetch
    ($ctx:expr => $($tail:tt)*) => {
        {
            let got = $crate::get!($ctx => $($tail)*);
            match got.data.clone() {
                Some(data) => Some(data),
                None if got.parses_opt || got.parses.is_none() => None,
                _ => {
                    use $crate::ArgiIsArg;
                    let err = match got.is_arg() {
                        true => $crate::Error::DataRequiredArg(got.instigators[0].to_string()),
                        false => $crate::Error::DataRequiredCommand
                    };
                    $ctx.help_err(err);
                    std::process::exit(1)
                }
            }
        }
    };
    // parse to type
    ($to:ty, $ctx:expr => $($tail:tt)*) => {
        $crate::data!($ctx => $($tail)*).map(|data| {
            match data.parse::<$to>() {
                Ok(parsed) => parsed,
                Err(err) => {
                    $ctx.help_err($crate::Error::InvalidData(stringify!($to)));
                    std::process::exit(1)
                }
            }
        })
    };
}

#[doc(hidden)] // rust workaround, #61265 (see https://github.com/rust-lang/rust/issues/61265)
#[macro_export]
macro_rules! cli_below {
    // empty (end of parsing)
    ($($wu:expr;)? $(,)?) => {};
    // help
    ($wu:expr; $(,)? help: $help:literal $($tail:tt)* ) => { { $wu.help = $help.into(); $crate::cli_below!($wu; $($tail)*); } };
    // parses
    ($wu:expr; $(,)? parses: $parses:ident $($tail:tt)* ) => { $wu.parses = Some(stringify!($parses)); $crate::cli_below!($wu; $($tail)*); };
    // optional
    ($wu:expr; $(,)? optional: true $($tail:tt)*) => { $wu.parses_opt = true };
    ($wu:expr; $(,)? optional: false $($tail:tt)*) => { $wu.parses_opt = false };
    // parses/optional matcher
    ($wu:expr => parse; $parses:ident ?) => { $crate::cli_below!($wu; parses: $parses, optional: true) };
    ($wu:expr => parse; $parses:ident) => { $crate::cli_below!($wu; parses: $parses) };
    // help arg errors
    ($wu:expr; $(,)? -h $($tail:tt)*) => { std::compile_error!("Help commands (`help` and `-h` along with `--help`) are reserved") };
    ($wu:expr; $(,)? --help $($tail:tt)*) => { $crate::cli_below!($wu; -h) };
    ($wu:expr; $(,)? help: { $($inner:tt)* } $($tail:tt)*) => { $crate::cli_below!($wu; -h) };
    // run
    ($wu:expr; $(,)? run: ($c:expr) $($tail:tt)*) => {
        {
            $wu.run = Some($c);
            $crate::cli_below!($wu; $($tail)*);
        }
    };
    // args
    ($wu:expr; $(,)? $($(-)?- $left:ident)+ $([$($parses:tt)*])? : { $($inner:tt)* } $($tail:tt)* ) => {
        {
            let instigators = &[ $( stringify!($left) ),+ ];
            #[allow(unused_mut)]
            let mut arg = $crate::Argument {
                instigators,
                help: $crate::Help::default(),
                parses: None,
                parses_opt: false, // TOOD: use
                used: false,
                run: None,
                data: None
            };

            $($crate::arg_below!(arg => parse; $($parses)*);)?
            $crate::arg_below!(arg; $($inner)*);
            $wu.args.push(arg);
            $crate::cli_below!($wu; $($tail)*);
        }
    };
    // commands
    ($wu:expr; $(,)? $left:ident $([$($parses:tt)*])? : { $($inner:tt)* } $($tail:tt)* ) => {
        {
            let mut cmd = $crate::Command {
                name: stringify!($left),
                help: $crate::Help::default(),
                args: vec![],
                subcmds: vec![],
                parses: None,
                parses_opt: false, // TODO: use
                used: false,
                run: None,
                data: None
            };

            $($crate::arg_below!(cmd => parse; $($parses)*);)?
            $crate::cli_below!(cmd; $($inner)*);
            $wu.subcmds.push(cmd);
            $crate::cli_below!($wu; $($tail)*);
        }
    };
}

#[doc(hidden)] // rust workaround, #61265 (see https://github.com/rust-lang/rust/issues/61265)
#[macro_export]
macro_rules! arg_below {
    // end of parsing
    ($($arg:expr;)? $(,)?) => {};
    // help
    ($arg:expr; help: $help:literal $($tail:tt)*) => {
        $arg.help = $help.into();
        $crate::arg_below!($arg; $($tail)*);
    };
    // run
    ($arg:expr; $(,)? run: ($c:expr) $($tail:tt)*) => {
        {
            $arg.run = Some($c);
            $crate::arg_below!($arg; $($tail)*);
        }
    };
    // parses
    ($arg:expr; $(,)? parses: $parses:ident $($tail:tt)* ) => { $arg.parses = Some(stringify!($parses)); $crate::arg_below!($arg; $($tail)*); };
    // optional
    ($arg:expr; $(,)? optional: true $($tail:tt)*) => { $arg.parses_opt = true };
    ($arg:expr; $(,)? optional: false $($tail:tt)*) => { $arg.parses_opt = false };

    ($arg:expr => parse; $parses:ident ?) => { $crate::cli_below!($arg; parses: $parses, optional: true) };
    ($arg:expr => parse; $parses:ident) => { $crate::cli_below!($arg; parses: $parses) };
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::str;

    const ID_STRING: &str = "AAAAAA";

    fn example_cmd<'a>() -> Command<'a> {
        Command {
            name: "mine",
            help: "This is a simple command".into(),
            parses: Some("number"),
            parses_opt: false,
            args: vec![
                Argument {
                    instigators: &["a", "b", "append"],
                    help: None.into(),
                    parses: Some("path"),
                    parses_opt: false,
                    used: false,
                    run: None,
                    data: None, // TODO: after launch to ensure working with "args_basic" test
                },
                Argument {
                    instigators: &["z", "zeta"],
                    help: "Simple help".into(),
                    parses: Some("text"),
                    parses_opt: false,
                    used: false,
                    run: None,
                    data: None,
                },
            ],
            subcmds: vec![Command {
                name: "water",
                help: None.into(),
                parses: Some("path"),
                parses_opt: false,
                args: vec![],
                subcmds: vec![],
                used: false,
                data: None,
                run: Some(|_, _| println!("{}", ID_STRING)),
            }],
            used: false,
            run: None,
            data: None,
        }
    }

    #[test]
    fn cmd_help_left() {
        let cmd = Command {
            name: "mine",
            help: None.into(),
            parses: Some("number"),
            parses_opt: false,
            args: vec![],
            subcmds: vec![],
            used: false,
            run: None,
            data: None,
        };
        assert_eq!(cmd.help_left(), "mine [number]".to_string());
    }

    #[test]
    fn arg_help_left() {
        let arg = Argument {
            instigators: &["a", "b", "append"],
            help: None.into(),
            parses: Some("path"),
            parses_opt: false,
            used: false,
            run: None,
            data: None,
        };
        assert_eq!(arg.help_left(), "-a -b --append [path]".to_string())
    }

    #[test]
    fn cmd_help_full() {
        let mut buf = vec![];

        example_cmd().help(&mut buf).unwrap();

        let mut lines = str::from_utf8(buf.as_slice()).unwrap().lines();
        lines.next();
        let res = lines.collect::<Vec<&str>>().join("\n");

        assert_eq!(res, "\n  This is a simple command\n\nCommands:\n  water [path]    \n\nArguments:\n  -a -b --append [path]    \n  -z --zeta [text]         Simple help".to_string())
    }

    #[test]
    fn args_short_basic() {
        let data = "egdata".to_string();
        let mut cmd = example_cmd();
        let mut input_stream = vec![data.clone()].into_iter().peekable();

        cmd.arg_flow(&mut input_stream, &mut vec![], "-a".to_string())
            .unwrap();

        assert_eq!(cmd.args[0].data, Some(data));
    }

    // TODO: implement, see issue #1 (https://github.com/Owez/argi/issues/1)
    // #[test]
    // fn args_short_multi() {
    //     let data = "pathandtext".to_string();
    //     let mut cmd = example_cmd();
    //     let mut input_stream = vec![data.clone()].into_iter().peekable();

    //     cmd.arg_flow(&mut input_stream, &mut vec![], "-az".to_string())
    //         .unwrap();

    //     assert_eq!(cmd.args[0].data, Some(data.clone()));
    //     assert_eq!(cmd.args[1].data, Some(data.clone()));
    // }

    #[test]
    fn args_long() {
        let data = "egdata".to_string();
        let mut cmd = example_cmd();
        let mut input_stream = vec![data.clone()].into_iter().peekable();

        cmd.arg_flow(&mut input_stream, &mut vec![], "--append".to_string())
            .unwrap();

        assert_eq!(cmd.args[0].data, Some(data));
    }

    #[test]
    fn searching_subcmds() {
        let mut cmd = example_cmd();
        assert_eq!(
            cmd.search_subcmds_mut("water").unwrap().parses,
            Some("path")
        ); // subcommand doesn't use partialeq so check HelpType
    }

    #[test]
    fn cli_macro_syntax() {
        cli_below!(cli!(); help: "hi");
        cli!();
        cli!(help: "hi");
        cli!(parses: text);
        cli!(help: "hi", parses: text);
        cli!(parses: path, help: "hi");
        cli! {
            help: "hello",
            parses: text,
            -a -b: {},
            -a -b: {
                help: "hello this"
            },
            -a -b: {},
        };
        cli! {
            help: "My cool program",
            run: (|ctx, _| println!("{:?}", data!(ctx => -d))),
            --hello: {help: "hi"},
            create: { help: "Creates something", -i [text]: { help: "Id to add" } },
            delete: { help: "Deletes something", --name [text]: { help: "Name to delete" } },
            -d --debug: { help: "Debug mode" }
        };
    }

    #[test]
    fn fmt_parses_opt() {
        assert_eq!(format!("[hello]"), fmt_parses(&Some("hello"), false));
        assert_eq!(format!("[hello?]"), fmt_parses(&Some("hello"), true));
        assert_eq!(String::new(), fmt_parses(&None, true));
    }

    // TODO: test data
    // TODO: test get
    // TODO: test help with opt parses
}
