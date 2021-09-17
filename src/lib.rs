//! The simple cli library

#![deny(unsafe_code)]
#![warn(missing_docs)]

mod error;

pub use error::{Error, Result};

use std::io::{self, Write};
use std::{env, fmt, process, str::FromStr};

pub enum HelpType {
    None,
    Text,
    Number,
    Path,
    Custom(&'static str),
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

trait CommonInternal<'a> {
    fn help_left(&self) -> String;
}

/// Contains common elements to both commands and arguments which can be used after launch to provide context
struct AfterLaunch {
    /// Raw data found from parsing, if found
    data: Option<String>,
    /// User-implemented closure which is ran at parse-time, if found
    run: Option<Box<dyn FnMut(&str)>>,
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
    pub fn parse<T: FromStr>(&self) -> std::result::Result<T, <T as FromStr>::Err> {
        T::from_str(self.after_launch.data.clone().unwrap().as_str())
    }

    pub fn launch(&mut self) -> Result<()> {
        let mut buf = io::stdout();
        let mut args = env::args();
        args.next();
        self.launch_custom(&mut buf, args)
    }

    /// Launches parsing with custom io buffer and argument source
    pub fn launch_custom(
        &mut self,
        buf: &mut impl Write,
        mut args: impl Iterator<Item = String>,
    ) -> Result<()> {
        const HELP_POSSIBLES: &[&str] = &["--help", "-h", "help"];
        let stack = vec![]; // TODO: use and make mut for subcommand
        let arg = match args.next() {
            Some(string) => string,
            None => {
                self.help(buf, stack)?;
                process::exit(1)
            }
        };
        let arg_str = arg.as_str();

        if HELP_POSSIBLES.contains(&arg_str) {
            self.help(buf, stack)?;
            process::exit(0)
        } else if arg.starts_with("-") {
            self.arg_flow(buf, &mut args, stack, arg)
        } else {
            todo!("subcommand")
        }
    }

    /// Continuing flow for an argument once `-` token is detected
    fn arg_flow(
        &mut self,
        buf: &mut impl Write,
        args: &mut impl Iterator<Item = String>,
        stack: Vec<&str>,
        arg: String,
    ) -> Result<()> {
        let cut_len = if arg.starts_with("--") { 2 } else { 1 }; // TODO: implement multiple on `-` to mean *and*
        let opt_arg = self.search_args_mut(&arg[cut_len..]);

        match opt_arg {
            Some(arg) => {
                let data = args.next().ok_or(Error::NoDataToParse)?;

                match &mut arg.after_launch.run {
                    Some(to_run) => to_run(&data),
                    None => (),
                };
                arg.after_launch.data = Some(data);
            }
            None => {
                self.help(buf, vec![])?;
                let for_stack = match stack.len() {
                    0 => String::new(),
                    _ => format!(" for '{}' command", stack.join(" ")),
                };
                eprintln!("Argument '{}' not found{}", arg, for_stack);
                process::exit(1)
            }
        }

        Ok(())
    }

    /// Searches argument for mutable argument
    fn search_args_mut(&mut self, instigator: &str) -> Option<&mut Argument<'a>> {
        for arg in self.args.iter_mut() {
            if arg.instigators.contains(&instigator) {
                return Some(arg);
            }
        }
        None
    }

    /// Writes full help message to buffer
    fn help(&self, buf: &mut impl Write, stack: Vec<&str>) -> Result<()> {
        // TODO: multi-line arguments
        // TODO: truncate message if too long
        let exe_path = env::current_exe().map_err(|_| Error::InvalidCurExe)?;
        let exe = exe_path
            .file_name()
            .ok_or(Error::InvalidCurExe)?
            .to_str()
            .ok_or(Error::InvalidCurExe)?;
        buf.write_fmt(format_args!("Usage: {}", exe))?;

        if stack.len() != 0 {
            buf.write_fmt(format_args!("{} ", stack.join(" ")))?;
        }
        buf.write_fmt(format_args!(
            "{}{} [options]\n\n  {}",
            self.name, self.help_type, self.help
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

        if self.subcmds.len() != 0 {
            buf.write(b"\n\nCommands:\n")?;
            any = true;

            let lr = self
                .subcmds
                .iter()
                .map(|subcmd| (subcmd.help_left(), &subcmd.help))
                .collect();
            tab_to(buf, lr)?;
        }

        if self.args.len() != 0 {
            buf.write(if any {
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
            buf.write(b"\n\nOptions:\n  No commands or arguments found!")?;
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
}

pub struct Argument<'a> {
    pub instigators: &'a [&'a str],
    pub help: Help<'a>,
    pub help_type: HelpType,
    after_launch: AfterLaunch,
}

impl<'a> Argument<'a> {
    pub fn parse<T: FromStr>(&self) -> std::result::Result<T, <T as FromStr>::Err> {
        T::from_str(self.after_launch.data.clone().unwrap().as_str())
    }
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
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{path::PathBuf, str};

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
                    after_launch: AfterLaunch::default(),
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
                help_type: HelpType::None,
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

        example_cmd().help(&mut buf, vec!["monster"]).unwrap();

        let mut lines = str::from_utf8(buf.as_slice()).unwrap().lines();
        lines.next();
        let res = lines.collect::<Vec<&str>>().join("\n");

        assert_eq!(res, "\n  This is a simple command\n\nCommands:\n  water   No help provided\n\nArguments:\n  -a -b --append [path]   No help provided\n  -z --zeta [text]        Simple help".to_string())
    }

    #[test]
    fn launch_cmd_run() {
        let mut cmd = example_cmd();
        let mut print_buf: Vec<u8> = vec![];
        let mut args = vec!["mine".to_string()];

        // mine only, shouldn't run water
        cmd.launch_custom(&mut print_buf, args.clone().into_iter())
            .unwrap();
        assert_ne!(
            &print_buf[print_buf.len() - ID_STRING.len()..],
            ID_STRING.as_bytes()
        );

        // water, should run water but not mine
        args.push("water".to_string());
        print_buf = vec![];
        cmd.launch_custom(&mut print_buf, args.into_iter()).unwrap();
        assert_eq!(
            &print_buf[print_buf.len() - ID_STRING.len()..],
            ID_STRING.as_bytes()
        );
    }

    #[test]
    fn parse_cmd() {
        const PATH: &str = "./src/lib.rs";
        let cmd = Command {
            name: "example",
            help: None.into(),
            help_type: HelpType::Text,
            args: vec![],
            subcmds: vec![],
            after_launch: AfterLaunch {
                data: Some(PATH.to_string()),
                run: None,
            },
        };
        assert_eq!(cmd.parse(), Ok(PathBuf::from(PATH)));
    }

    #[test]
    fn parse_arg() {
        const PATH: &str = "./src/lib.rs";
        let arg = Argument {
            instigators: &["a", "after"],
            help: None.into(),
            help_type: HelpType::Path,
            after_launch: AfterLaunch {
                data: Some(PATH.into()),
                run: None,
            },
        };
        assert_eq!(arg.parse(), Ok(PathBuf::from(PATH)))
    }

    // TODO: arg_flow in Command
}

// /// High-level builder for a new command-line-interface
// #[macro_export]
// macro_rules! cli {

// }
// macro_rules! cli_inner {
//     ( $name:tt ( => help: $help:tt, parses: $parses:ident, run: $run:expr, below: [ (cli_inner)* ] )? ) => {
//         todo!()
//     };
// }

// cli!(help: "general cli help", "inner_stuff" => {help: "the inner stuff appears here"}, "--arg" => {run: |data| data.split(" ")})
// cli_inner!(
//     "command" => {
//         help: "given help here",
//         run: |data| data.destroy(),
//         below: [
//             "other" => {help: "mini help", run: |data| data},
//             "-f --final" => {help: "another mini help"},
//             "basic"
//         ]
//     }
// )
