//! The simple cli library

#![deny(unsafe_code)]
#![warn(missing_docs)]

mod error;

pub use error::{Error, Result};

use std::{env, fmt, io::Write, str::FromStr};

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

pub struct Command<'a> {
    pub name: &'a str,
    pub help: Help<'a>,
    pub help_type: HelpType,
    pub args: Vec<Argument<'a>>,
    pub subcmds: Vec<Command<'a>>,
    data: Option<String>,
}

impl<'a> Command<'a> {
    pub fn parse<T: FromStr>(&self) -> std::result::Result<T, <T as FromStr>::Err> {
        T::from_str(self.data.clone().unwrap().as_str())
    }

    /// Recursively searches for given name and adds all tried into vector for future "did you mean .." results
    fn search_subcmd<'b>(&mut self, name: &'b str, so_far: &mut Vec<&'b str>) -> Result<&mut Self> {
        if self.name == name {
            Ok(self)
        } else {
            so_far.push(name);
            for subcmd in self.subcmds.iter_mut() {
                match subcmd.search_subcmd(name, so_far) {
                    Ok(found) => return Ok(found),
                    Err(_) => (),
                }
            }
            Err(Error::CommandNotFound(name.to_string()))
        }
    }

    /// Writes full help message to buffer
    fn help(&self, buf: &mut impl Write, level: Vec<&'a str>) -> Result<()> {
        // TODO: multi-line arguments
        // TODO: truncate message if too long
        let exe_path = env::current_exe().map_err(|_| Error::InvalidCurExe)?;
        let exe = exe_path
            .file_name()
            .ok_or(Error::InvalidCurExe)?
            .to_str()
            .ok_or(Error::InvalidCurExe)?;
        buf.write_fmt(format_args!("Usage: {}", exe))?;

        if level.len() != 0 {
            buf.write_fmt(format_args!("{} ", level.join(" ")))?;
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
            any = true;
            buf.write(b"\n\nCommands:\n")?;
            let lr = self
                .subcmds
                .iter()
                .map(|subcmd| (subcmd.help_left(), &subcmd.help))
                .collect();
            tab_to(buf, lr)?;
        }

        if self.args.len() != 0 {
            any = true;
            buf.write(b"\n\nArguments:\n")?;
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
    data: Option<String>,
}

impl<'a> Argument<'a> {
    pub fn parse<T: FromStr>(&self) -> std::result::Result<T, <T as FromStr>::Err> {
        T::from_str(self.data.clone().unwrap().as_str())
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
    use std::str;

    #[test]
    fn cmd_help_left() {
        let cmd = Command {
            name: "mine",
            help: None.into(),
            help_type: HelpType::Number,
            args: vec![],
            subcmds: vec![],
            data: None,
        };
        assert_eq!(cmd.help_left(), "mine [number]".to_string());
    }

    #[test]
    fn arg_help_left() {
        let arg = Argument {
            instigators: &["a", "b", "append"],
            help: None.into(),
            help_type: HelpType::Path,
            data: None,
        };
        assert_eq!(arg.help_left(), "-a -b --append [path]".to_string())
    }

    #[test]
    fn cmd_help_full() {
        let cmd = Command {
            name: "mine",
            help: "This is a simple command".into(),
            help_type: HelpType::Number,
            args: vec![
                Argument {
                    instigators: &["a", "b", "append"],
                    help: None.into(),
                    help_type: HelpType::Path,
                    data: None,
                },
                Argument {
                    instigators: &["z", "zeta"],
                    help: "Simple help".into(),
                    help_type: HelpType::Text,
                    data: None,
                },
            ],
            subcmds: vec![],
            data: None,
        };
        let mut buf = vec![];

        cmd.help(&mut buf, vec!["monster"]).unwrap();

        let mut lines = str::from_utf8(buf.as_slice()).unwrap().lines();
        lines.next();
        let res = lines.collect::<Vec<&str>>().join("\n");

        assert_eq!(res, "\n  This is a simple command\n\nArguments:\n  -a -b --append [path]   No help provided\n  -z --zeta [text]        Simple help".to_string())
    }
}

// cli!("hello" => {help: "woah", parses: String, below: ["-a|--append": {parses: PathBuf}, "-b|-c": {}]}, "-zeta": {help: "just an awesome argument"})
