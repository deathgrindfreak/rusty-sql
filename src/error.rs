use std::fmt;
use std::error::Error;

use crate::parser::ParseError;

#[derive(Debug)]
pub enum BackendError {
    ErrTableDoesNotExist,
    ErrColumnDoesNotExist,
    ErrInvalidSelectItem,
    ErrInvalidDatatype,
    ErrMissingValues,
    ErrInvalidOperands,
    ErrParsingError(ParseError),
}

impl fmt::Display for BackendError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        let err_msg = match self {
            BackendError::ErrTableDoesNotExist  => "Table does not exist".to_string(),
            BackendError::ErrColumnDoesNotExist => "Column does not exist".to_string(),
            BackendError::ErrInvalidSelectItem => "Select item is not valid".to_string(),
            BackendError::ErrInvalidDatatype => "Invalid datatype".to_string(),
            BackendError::ErrMissingValues => "Missing values".to_string(),
            BackendError::ErrInvalidOperands => "Invalid operands".to_string(),
            BackendError::ErrParsingError(e) => format!("Parse Error: {}", e),
        };
        write!(f, "{}", err_msg)
    }
}

impl Error for BackendError {}

impl Into<BackendError> for ParseError {
    fn into(self) -> BackendError {
        BackendError::ErrParsingError(self)
    }
}
