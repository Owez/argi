//! Constructs for reporting errors within this library

use crate::get_cur_exe;
use std::{fmt, io};

/// Crate-specific result type for ease-of-use
pub type Result<T> = std::result::Result<T, Error>;

/// Represents potential parsing errors which may occur
#[derive(Debug)]
pub enum Error {
    /// Data was required after command/argument but was not found
    DataRequired(Vec<String>),
    /// Command or argument shouldn't have data attached but it does
    UnknownData,
    /// Parsing method was called but the cli has not yet been launched
    NoDataToParse,
    /// Input/output error
    Io(io::Error),
    /// Current executable name is invalid
    InvalidCurExe,
    /// No commands where provided at all
    NoCommandsProvided,
    /// Command name provided could not be found
    CommandNotFound(Vec<String>),
    /// Argument name provided could not be found
    ArgumentNotFound(Vec<String>),
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
            Error::UnknownData => write!(
                f,
                "Command or argument shouldn't have data attached but it does"
            ),
            Error::NoDataToParse => write!(
                f,
                "Parsing method was called but the cli has not yet been launched"
            ),
            Error::Io(err) => write!(f, "Input/output error, {}", err),
            Error::InvalidCurExe => write!(f, "Current executable name is invalid"),
            Error::NoCommandsProvided => write!(f, "No commands where provided"),
            Error::CommandNotFound(call) => {
                write!(
                    f,
                    "Command '{}' provided could not be found",
                    fmt_call(call)
                )
            }
            Error::ArgumentNotFound(call) => {
                write!(
                    f,
                    "Command '{}' provided could not be found",
                    fmt_call(call)
                )
            }
        }
    }
}

fn fmt_call(call: &[String]) -> String {
    let left = match get_cur_exe() {
        Ok(cur_exe) => cur_exe,
        Err(_) => String::new(),
    };
    format!("{}{}", left, call.join(" "))
}
