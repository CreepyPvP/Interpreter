use std::io;

#[derive(Debug)]
pub enum AppError {
    IoError(io::Error),
    ParserError(String),
}

impl std::error::Error for AppError {}

impl std::fmt::Display for AppError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "[Error]: {}",
            match self {
                Self::IoError(err) => format!("IoError: {}", err),
                Self::ParserError(err) => format!("ParserError: {}", err),
            }
        )
    }
}

impl From<std::io::Error> for AppError {
    fn from(value: std::io::Error) -> Self {
        Self::IoError(value)
    }
}
