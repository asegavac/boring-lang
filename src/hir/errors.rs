use std::error::Error;
use std::fmt;


#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ErrorLabel {
    Primary,
    Secondary,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct ErrorPart {
    pub message: String,
    pub label_type: ErrorLabel,
    pub start: i64
    pub end: i64
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct CompilerError {
    pub message: String,
    pub parts: Vec<ErrorPart>,
}



impl fmt::Display for CompilerError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, self.message)
    }
}
