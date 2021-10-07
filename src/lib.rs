//! The simple cli library

#![deny(unsafe_code)]
#![warn(missing_docs)]

mod error;

pub use error::{Error, Result};

use std::io::{self, Write};
use std::iter::Peekable;
use std::{env, fmt, process};

/// Whitelisted overriding help targets
const HELP_POSSIBLES: &[&str] = &["--help", "-h", "help"];

/// Gets file name current exe as a string
pub(crate) fn get_cur_exe() -> Result<String> {
    env::current_exe()
        .map_err(|_| Error::InvalidCurExe)?
        .file_name()
        .ok_or(Error::InvalidCurExe)?
        .to_os_string()
        .into_string()
        .map_err(|_| Error::InvalidCurExe) // TODO: `./` if on unix-like?
}

#[derive(Debug, PartialEq, Eq)]
pub enum HelpType {
    None,
    Text,
    Number,
    Path,
    Custom(&'static str),
    // TODO: optional help
}

impl Default for HelpType {
    fn default() -> Self {
        Self::None
    }
}

impl fmt::Display for HelpType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            HelpType::None => Ok(()),
            HelpType::Text => write!(f, " [text]"),
            HelpType::Number => write!(f, " [number]"),
            HelpType::Path => write!(f, " [path]"),
            HelpType::Custom(val) => write!(f, "[{}] ", val.to_lowercase()),
        }
    }
}

/// Help message to display to the user
pub struct Help<'a>(Option<&'a str>);

impl<'a> Default for Help<'a> {
    fn default() -> Self {
        Self(None)
    }
}

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
            None => write!(f, "No help provided"),
        }
    }
}

/// Common methods and data items to all call types
trait CommonInternal<'a> {
    /// Generates leftmost help message
    fn help_left(&self) -> String;
    /// Checks the help type for the intended data
    fn no_data(&self) -> bool;
    /// Applies [AfterLaunch]-related tasks
    fn apply_afters(&mut self, data: String);
}

/// Contains common elements to both commands and arguments which can be used after launch to provide context
struct AfterLaunch {
    /// Raw data found from parsing, if found
    data: Option<String>,
    /// User-implemented closure which is ran at parse-time, if found
    run: Option<Box<dyn FnMut(Option<&str>)>>,
}

impl<'a> Default for AfterLaunch {
    fn default() -> Self {
        Self {
            data: None,
            run: None,
        }
    }
}

pub struct Command<'a> {
    pub name: &'a str,
    pub help: Help<'a>,
    pub help_type: HelpType,
    pub args: Vec<Argument<'a>>,
    pub subcmds: Vec<Command<'a>>,
    after_launch: AfterLaunch,
}

impl<'a> Command<'a> {
    /// Creates a new command for the "root", hidden from docs as this should be only accessed via macros
    #[doc(hidden)]
    pub fn new_root() -> Self {
        Self {
            name: "",
            help: Help::default(),
            help_type: HelpType::None,
            args: vec![],
            subcmds: vec![],
            after_launch: AfterLaunch::default(),
        }
    }

    pub fn launch(&mut self) {
        const ERROR: &str = "\nError:\n  ";
        let mut stream = env::args();
        stream.next();
        match self.launch_custom(&mut stream.peekable()) {
            Ok(()) => (),
            Err(err) => {
                // help
                let stderr = io::stderr();
                let mut stderr_lock = stderr.lock();
                match self.help(&mut stderr_lock) {
                    Ok(()) => (),
                    Err(_) => eprintln!("{}Couldn't generate help for error below!", ERROR),
                }

                // print error
                eprintln!("{}{}!", ERROR, err);
                process::exit(1)
            }
        }
    }

    pub fn launch_custom(
        &mut self,
        input_args: &mut Peekable<impl Iterator<Item = String>>,
    ) -> Result<()> {
        if self.no_data() && input_args.peek().is_none() {
            Err(Error::NoCommandsProvided)
        } else {
            self.parse_next(input_args, &mut vec![])
        }
    }

    /// Recurses from current command instance horizontally to fetch arguments and downwards to more subcommands
    fn parse_next(
        &mut self,
        stream: &mut Peekable<impl Iterator<Item = String>>,
        call: &mut Vec<String>,
    ) -> Result<()> {
        // TODO: recurse
        let left = match stream.next() {
            Some(val) if HELP_POSSIBLES.contains(&val.as_str()) => {
                self.help(&mut io::stdout())?;
                return Ok(());
            } // in whitelist, return with help
            Some(val) => val,      // good value
            None => return Ok(()), // end of stream
        };
        call.push(left.clone()); // add new left to call stream

        if left.starts_with('-') {
            // argument
            self.arg_flow(stream, call, left)?
        } else if let Some(cmd) = self.search_subcmds_mut(&left) {
            // subcommand
            cmd.parse_next(stream, call)?
        } else if self.help_type != HelpType::None {
            // data for self
            self.apply_afters(left)
        } else {
            // unwanted data
            return Err(Error::CommandNotFound((left, call.clone())));
        }

        // TODO: check logic of above vs below

        // recurse for arguments until stream end, all else is delt with via subcommand
        self.parse_next(stream, call)
    }

    /// Pathway for arguments which automatically adds data to arguments if found in current instance
    fn arg_flow(
        &mut self,
        stream: &mut Peekable<impl Iterator<Item = String>>,
        call: &mut Vec<String>,
        left: String,
    ) -> Result<()> {
        let instigator_fmt = if let Some(instigator) = left.strip_prefix("--") {
            vec![instigator.to_string()]
        } else {
            left[1..]
                .chars()
                .into_iter()
                .map(|c| c.to_string())
                .collect()
        };

        for instigator in instigator_fmt {
            let instigator_str = instigator.as_str();

            // validation and help
            let mut found = false;
            for arg in self.args.iter() {
                if arg.instigators.contains(&instigator_str) {
                    found = true;
                    match stream.peek() {
                        Some(next) if HELP_POSSIBLES.contains(&next.as_str()) => {
                            stream.next();
                            self.help(&mut io::stdout())?;
                            return Ok(());
                        }
                        _ => continue,
                    }
                }
            }
            if !found {
                return Err(Error::ArgumentNotFound((instigator, call.clone())));
            }

            // mutable data application, due to rust
            for arg in self.args.iter_mut() {
                if arg.instigators.contains(&instigator_str) {
                    arg.apply_afters(
                        stream
                            .next()
                            .ok_or_else(|| Error::DataRequired(call.clone()))?,
                    )
                }
            }
        }

        Ok(())
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

    /// Writes full help message to buffer
    fn help(&self, buf: &mut impl Write) -> Result<()> {
        // TODO: multi-line arguments
        // TODO: truncate message if too long
        buf.write_fmt(format_args!(
            "Usage: {} {}{}[OPTIONS]\n\n  {}",
            get_cur_exe()?,
            self.name,
            self.help_type,
            self.help
        ))?;

        /// Automatically pads left and right hand side of help messages together
        fn tab_to<'a>(buf: &mut impl Write, lr: Vec<(String, &Help<'a>)>) -> Result<()> {
            let mut max = 0;
            for (l, _) in lr.iter() {
                let l_len = l.len();
                if l_len > max {
                    max = l_len
                }
            }

            for (l, r) in lr {
                let padding = " ".repeat(max - l.len());
                buf.write_fmt(format_args!("  {}{}   {}\n", l, padding, r))?;
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
}

impl<'a> CommonInternal<'a> for Command<'a> {
    fn help_left(&self) -> String {
        let mut output = self.name.to_string();
        output.push_str(&self.help_type.to_string());
        output
    }

    // TODO: merge below two
    fn no_data(&self) -> bool {
        self.help_type == HelpType::None
    }

    fn apply_afters(&mut self, data: String) {
        if let Some(run) = &mut self.after_launch.run {
            run(Some(&data))
        }
        self.after_launch.data = Some(data);
    }
}

pub struct Argument<'a> {
    pub instigators: &'a [&'a str],
    pub help: Help<'a>,
    pub help_type: HelpType,
    after_launch: AfterLaunch,
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

        // FIXME: make less janky formats, use write! instead
        let mut fmtd: Vec<String> = short.iter().map(|s| format!("-{}", s)).collect();
        fmtd.extend(long.iter().map(|l| format!("--{}", l)));

        output.push_str(fmtd.join(" ").as_str());
        output.push_str(&self.help_type.to_string());
        output
    }

    fn no_data(&self) -> bool {
        self.help_type == HelpType::None
    }

    fn apply_afters(&mut self, data: String) {
        if let Some(run) = &mut self.after_launch.run {
            run(Some(&data))
        }
        self.after_launch.data = Some(data);
    }
}

#[macro_export]
macro_rules! cli {
    () => { $crate::Command::new_root() };
    ($($tail:tt)*) => {
        {
            let mut cli = $crate::cli!();
            $crate::cli_below!(cli; $($tail)*);
            cli
        }
     };
}

#[doc(hidden)] // rust workaround, #61265 (see https://github.com/rust-lang/rust/issues/61265)
#[macro_export]
macro_rules! cli_below {
    // recursion-only
    ($cmd:expr; $($($left:literal),* $(,)? $(:)? { $($tail:tt)* }),* $(,)?) => { todo!("recursive commands/args") };
    // meta-only help
    ($cmd:expr; help: $help:literal) => { $cmd.help = $help.into() };
    // meta-only parses
    ($cmd:expr; parses: none) => { $cmd.help_type = $crate::HelpType::None };
    ($cmd:expr; parses: text) => { $cmd.help_type = $crate::HelpType::Text };
    ($cmd:expr; parses: path) => { $cmd.help_type = $crate::HelpType::Path };
    ($cmd:expr; parses: $parses:literal) => {
        {
            // NOTE: you can't test errors without using the compiletest_rs crate
            match $parses  {
                "none" => std::compile_error!("Use `parses: none` or leave it out entirely instead of the the stringified `parses: \"none\"` version"),
                "text" => std::compile_error!("Use `parses: text` instead of the the stringified `parses: \"text\"` version"),
                "path" => std::compile_error!("Use `parses: path` instead of the the stringified `parses: \"path\"` version"),
                _ => $cmd.help_type = $crate::HelpType::Custom($parses)
            }
        }
    };
    ($cmd:expr; parses: None) => { $cmd.help_type = $crate::HelpType::None };
    ($cmd:expr; parses: $(&)?str) => { $cmd.help_type = $crate::HelpType::Text };
    ($cmd:expr; parses: String) => { $cmd.help_type = $crate::HelpType::Text };
    ($cmd:expr; parses: std::path::Path) => { $cmd.help_type = $crate::HelpType::Path };
    ($cmd:expr; parses: PathBuf) => { $cmd.help_type = $crate::HelpType::Path };
    // TODO: more
    // TODO: figure out run going into parses
    // meta-only mixed
    ($cmd:expr; help: $help:literal, parses: $parses:tt) => {
        $crate::cli_below!($cmd; help: $help);
        $crate::cli_below!($cmd; parses: $parses);
    };
    ($cmd:expr; parses: $parses:tt, help: $help:literal) => { $crate::cli_below!($cmd; help: $help, parses: $parses) };
    // mixed
    ($cmd:expr; help: $help:literal, $($further:tt)*) => {
        $crate::cli_below!($cmd; help: $help);
        $crate::cli_below!($cmd; $($further)*);
    };
    ($cmd:expr; parses: $parses:tt, $($further:tt)*) => {
        $crate::cli_below!($cmd; parses: $parses);
        $crate::cli_below!($cmd; $($further)*);
    };
    ($cmd:expr; help: $help:literal, parses: $parses:tt, $($further:tt)*) => {
        $crate::cli_below!($cmd; help: $help, parses: $parses);
        $crate::cli_below!($cmd; $($further)*);
    };
    ($cmd:expr; parses: $parses:tt, help: $help:literal, $($further:tt)*) => { $crate::cli_below!($cmd; help: $help, parses: $parses, $($further)*) };
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
            help_type: HelpType::Number,
            args: vec![
                Argument {
                    instigators: &["a", "b", "append"],
                    help: None.into(),
                    help_type: HelpType::Path,
                    after_launch: AfterLaunch::default(), // TODO: after launch to ensure working with "args_basic" test
                },
                Argument {
                    instigators: &["z", "zeta"],
                    help: "Simple help".into(),
                    help_type: HelpType::Text,
                    after_launch: AfterLaunch::default(),
                },
            ],
            subcmds: vec![Command {
                name: "water",
                help: None.into(),
                help_type: HelpType::Path,
                args: vec![],
                subcmds: vec![],
                after_launch: AfterLaunch {
                    data: None,
                    run: Some(Box::new(|_| println!("{}", ID_STRING))),
                },
            }],
            after_launch: AfterLaunch::default(),
        }
    }

    #[test]
    fn cmd_help_left() {
        let cmd = Command {
            name: "mine",
            help: None.into(),
            help_type: HelpType::Number,
            args: vec![],
            subcmds: vec![],
            after_launch: AfterLaunch::default(),
        };
        assert_eq!(cmd.help_left(), "mine [number]".to_string());
    }

    #[test]
    fn arg_help_left() {
        let arg = Argument {
            instigators: &["a", "b", "append"],
            help: None.into(),
            help_type: HelpType::Path,
            after_launch: AfterLaunch::default(),
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

        assert_eq!(res, "\n  This is a simple command\n\nCommands:\n  water [path]   No help provided\n\nArguments:\n  -a -b --append [path]   No help provided\n  -z --zeta [text]        Simple help".to_string())
    }

    #[test]
    fn args_short_basic() {
        let data = "egdata".to_string();
        let mut cmd = example_cmd();
        let mut input_stream = vec![data.clone()].into_iter().peekable();

        cmd.arg_flow(&mut input_stream, &mut vec![], "-a".to_string())
            .unwrap();

        assert_eq!(cmd.args[0].after_launch.data, Some(data));
    }

    // TODO: implement, see issue #1 (https://github.com/Owez/argi/issues/1)
    // #[test]
    // fn args_short_multi() {
    //     let data = "pathandtext".to_string();
    //     let mut cmd = example_cmd();
    //     let mut input_stream = vec![data.clone()].into_iter().peekable();

    //     cmd.arg_flow(&mut input_stream, &mut vec![], "-az".to_string())
    //         .unwrap();

    //     assert_eq!(cmd.args[0].after_launch.data, Some(data.clone()));
    //     assert_eq!(cmd.args[1].after_launch.data, Some(data.clone()));
    // }

    #[test]
    fn args_long() {
        let data = "egdata".to_string();
        let mut cmd = example_cmd();
        let mut input_stream = vec![data.clone()].into_iter().peekable();

        cmd.arg_flow(&mut input_stream, &mut vec![], "--append".to_string())
            .unwrap();

        assert_eq!(cmd.args[0].after_launch.data, Some(data));
    }

    #[test]
    fn searching_subcmds() {
        let mut cmd = example_cmd();
        assert_eq!(
            cmd.search_subcmds_mut("water").unwrap().help_type,
            HelpType::Path
        ); // subcommand doesn't use partialeq so check HelpType
    }

    #[test]
    fn raw_parsing() {
        // equals `./program mine 21 -a "./path" water`
        let stream = vec![
            "mine".to_string(),
            "21".to_string(),
            "-a".to_string(),
            "./path".to_string(),
            "water".to_string(),
        ];

        // generate
        let mut cmd = example_cmd();

        // parse_next version
        cmd.parse_next(&mut stream.clone().into_iter().peekable(), &mut vec![])
            .unwrap();
        assert_eq!(cmd.after_launch.data, Some("21".to_string())); // mine
        assert_eq!(cmd.args[0].after_launch.data, Some("./path".to_string())); // -a

        // reset
        cmd = example_cmd();

        // launch_custom version
        cmd.launch_custom(&mut stream.clone().into_iter().peekable())
            .unwrap();
        assert_eq!(cmd.after_launch.data, Some("21".to_string())); // mine
        assert_eq!(cmd.args[0].after_launch.data, Some("./path".to_string())); // -a
    }

    #[test]
    fn cli_macro_syntax() {
        cli_below!(cli!(); help: "hi");
        cli!();
        cli!(help: "hi");
        cli!(parses: PathBuf);
        cli!(help: "hi", parses: PathBuf);
        cli!(parses: PathBuf, help: "hi");
        cli! {
            help: "hello",
            parses: PathBuf,
            "-a", "-b" {},
            "-a", "-b" {
                help: "hello this"
            },
            "-a", "-b" {},
        }
    }

    // TODO: more raw parsing tests
}
