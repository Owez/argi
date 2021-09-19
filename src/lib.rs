//! The simple cli library

#![deny(unsafe_code)]
#![warn(missing_docs)]

mod error;

pub use error::{Error, Result};

use std::io::{self, Write};
use std::process;
use std::{env, fmt, str::FromStr};

/// Whitelisted overriding help targets
const HELP_POSSIBLES: &[&str] = &["--help", "-h", "help"];

#[derive(PartialEq, Eq)]
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
    pub fn parse_into<T: FromStr>(&self) -> std::result::Result<T, <T as FromStr>::Err> {
        T::from_str(self.after_launch.data.clone().unwrap().as_str())
    }

    pub fn launch(&mut self) {
        const ERROR: &str = "Error: ";
        loop {
            match self.parse_next(&mut env::args(), &mut vec![]) {
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
                    eprintln!("{}{}", ERROR, err);
                    process::exit(1)
                }
            }
        }
    }

    /// Parses entirety of next item and processes [AfterLaunch] tasks
    fn parse_next(
        &mut self,
        stream: &mut impl Iterator<Item = String>,
        call: &mut Vec<String>,
    ) -> Result<()> {
        let left = match stream.next() {
            Some(val) if HELP_POSSIBLES.contains(&val.as_str()) => {
                self.help(&mut io::stdout())?;
                return Ok(());
            } // in whitelist, return with help
            Some(val) => val,      // good value
            None => return Ok(()), // end of stream
        };
        call.push(left.clone()); // add new left to call stream

        if left.starts_with("-") {
            // argument
            // TODO: move to another method for testing
            if left.starts_with("--") {
                // gen for long arg
                push_data(stream, call, self.search_args_mut(&call, &left[2..])?)?
            } else {
                // gen for short arg(s)
                for c in left[1..].chars() {
                    push_data(stream, call, self.search_args_mut(&call, &c.to_string())?)?
                }
            }
        } else {
            // subcommand
            todo!("subcommand")
        }

        Ok(())
    }

    /// Searches current instance for given argument by it's instigator
    fn search_args_mut(
        &mut self,
        call: &Vec<String>,
        instigator: &str,
    ) -> Result<&mut Argument<'a>> {
        for arg in self.args.iter_mut() {
            if arg.instigators.contains(&instigator) {
                return Ok(arg);
            }
        }
        Err(Error::ArgumentNotFound(call.clone()))
    }

    /// Writes full help message to buffer
    fn help(&self, buf: &mut impl Write) -> Result<()> {
        // TODO: multi-line arguments
        // TODO: truncate message if too long
        let exe_path = env::current_exe().map_err(|_| Error::InvalidCurExe)?;
        let exe = exe_path
            .file_name()
            .ok_or(Error::InvalidCurExe)?
            .to_str()
            .ok_or(Error::InvalidCurExe)?;
        buf.write_fmt(format_args!("Usage: {}", exe))?;
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

impl<'a> Argument<'a> {
    pub fn parse_into<T: FromStr>(&self) -> std::result::Result<T, <T as FromStr>::Err> {
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

/// If there is no help data intended, ensure there is data and then apply it
/// via the [CommonInternal::apply_afters] implementation
fn push_data<'a>(
    stream: &mut impl Iterator<Item = String>,
    call: &Vec<String>,
    item: &mut impl CommonInternal<'a>,
) -> Result<()> {
    if !item.no_data() {
        item.apply_afters(stream.next().ok_or(Error::DataRequired(call.clone()))?)
    }
    Ok(())
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
                    after_launch: AfterLaunch::default(), // TODO: after launch to ensure working with "arguments" test
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

        example_cmd().help(&mut buf).unwrap();

        let mut lines = str::from_utf8(buf.as_slice()).unwrap().lines();
        lines.next();
        let res = lines.collect::<Vec<&str>>().join("\n");

        assert_eq!(res, "\n  This is a simple command\n\nCommands:\n  water   No help provided\n\nArguments:\n  -a -b --append [path]   No help provided\n  -z --zeta [text]        Simple help".to_string())
    }

    // TODO: remaster
    // #[test]
    // fn launch_cmd_run() {
    //     // TODO: redo with stdout/stderr as the library now properly errors
    //     let mut cmd = example_cmd();
    //     let mut print_buf: Vec<u8> = vec![];
    //     let mut args = vec!["mine".to_string()];

    //     // mine only, shouldn't run water
    //     cmd.launch_custom(args.clone().into_iter()).unwrap();
    //     assert_ne!(
    //         &print_buf[print_buf.len() - ID_STRING.len()..],
    //         ID_STRING.as_bytes()
    //     );

    //     // water, should run water but not mine
    //     args.push("water".to_string());
    //     print_buf = vec![];
    //     cmd.launch_custom(args.into_iter()).unwrap();
    //     assert_eq!(
    //         &print_buf[print_buf.len() - ID_STRING.len()..],
    //         ID_STRING.as_bytes()
    //     );
    // }

    #[test]
    fn cmd_parse_into() {
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
        assert_eq!(cmd.parse_into(), Ok(PathBuf::from(PATH)));
    }

    #[test]
    fn arg_parse_into() {
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
        assert_eq!(arg.parse_into(), Ok(PathBuf::from(PATH)))
    }

    // TODO: remaster
    // #[test]
    // fn arguments() {
    //     // TODO: check argument run stdout
    //     let mut cmd = example_cmd();
    //     let mut input_stream = vec!["mine".to_string()].into_iter();
    //     cmd.arg_flow(&mut input_stream, "-a".to_string()).unwrap()
    // }
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
