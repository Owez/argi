//! Constructs for reporting errors within this library

use crate::fmt_call;
use std::{fmt, io};

/// Crate-specific result type for ease-of-use
pub type Result<T> = std::result::Result<T, Error>;

/// Represents potential parsing errors which may occur
#[derive(Debug)]
pub enum Error {
    /// Data was required after known command/argument in stack but was not found
    DataRequired(Vec<String>),
    /// Data was required for argument but was not found
    DataRequiredArg(String), // FIXME: could add call here too
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
    /// Other argument in multiple short args required data but argument doesn't parse anything
    OtherArgNeedsData((String, Vec<String>)),
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
            Error::OtherArgNeedsData((left,call)) =>write!(f,"An argument inside of {} required data but another doesn't; these should be consistent, found inside of `{}` program call", left,fmt_call(call))
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
