//! Constructs for reporting errors within this library

use crate::get_cur_exe;
use std::{fmt, io};

/// Crate-specific result type for ease-of-use
pub type Result<T> = std::result::Result<T, Error>;

/// Represents potential parsing errors which may occur
#[derive(Debug)]
pub enum Error {
    /// Data was required after known command/argument in stack but was not found
    DataRequired(Vec<String>),
    /// Data was required for argument but was not found, used in macro magic
    DataRequiredArg(String), // FIXME: could add call here too
    /// Data was required for command but was not found, used in macro magic
    DataRequiredCommand,
    /// Input/output error
    Io(io::Error),
    /// Current executable name is invalid
    InvalidCurExe,
    /// Command name provided could not be found
    CommandNotFound((String, Vec<String>)),
    /// Argument name provided could not be found
    ArgumentNotFound((String, Vec<String>)),
    /// Nothing was inputted
    NothingInputted,
    /// Invalid data was provided
    InvalidData(&'static str),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::DataRequired(call) => {
                write!(
                    f,
                    "Data was required after \".. {}\" but was not found",
                    fmt_call(call)
                )
            }
            Error::DataRequiredArg(arg) => {
                let fmt_arg = if arg.starts_with('-') {
                    arg.clone()
                } else if arg.len() == 1 {
                    format!("-{}", arg)
                } else {
                    format!("--{}", arg)
                };
                write!(f, "Data was required for {} but was not found", fmt_arg)
            }
            Error::DataRequiredCommand => {
                write!(f, "Data was required for command but was not found")
            }
            Error::Io(err) => write!(f, "Input/output error, {}", err),
            Error::InvalidCurExe => write!(f, "Current executable name is invalid"),
            Error::CommandNotFound((cmd, call)) => {
                write!(
                    f,
                    "Command `{}` not recognized, found inside of `{}` program call",
                    cmd,
                    fmt_call(call)
                )
            }
            Error::ArgumentNotFound((arg, call)) => {
                write!(
                    f,
                    "Argument {}{} not recognized, found inside of `{}` program call",
                    arg_dashes(arg.len()),
                    arg,
                    fmt_call(call)
                )
            }
            Error::NothingInputted => write!(f, "Nothing was inputted"),
            Error::InvalidData(v) => write!(f, "Data provided could not be parsed to {}", v),
        }
    }
}

/// Formats dashes onto arguments, used instead of directly supplying original
/// so `-abc` can be formatted down into `-b` potentially
fn arg_dashes(arg_len: usize) -> &'static str {
    if arg_len < 2 {
        "-"
    } else {
        "--"
    }
}

fn fmt_call(call: &[String]) -> String {
    let left = match get_cur_exe() {
        Ok(cur_exe) => cur_exe,
        Err(_) => String::new(),
    };
    format!("{} {}", left, call.join(" "))
}
