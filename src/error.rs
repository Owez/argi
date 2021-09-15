//! Constructs for reporting errors within this library

use std::{fmt, io};

/// Crate-specific result type for ease-of-use
pub type Result<T> = std::result::Result<T, Error>;

/// Represents potential parsing errors which may occur
#[derive(Debug)]
pub enum Error {
    /// Data was required after command/argument but was not found
    DataRequired(String),
    /// Command or argument shouldn't have data attached but it does
    UnknownData,
    /// Parsing method was called but the cli has not yet been launched
    NoDataToParse,
    /// Input/output error
    Io(io::Error),
    /// Current executable name is invalid
    InvalidCurExe,
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Self {
        Self::Io(err)
    }
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Error::DataRequired(name) => {
                write!(
                    f,
                    "Data was required after \".. {}\" but was not found",
                    name
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
        }
    }
}
