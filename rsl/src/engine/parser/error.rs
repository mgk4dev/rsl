use std::fmt::{Display, Formatter};

#[derive(Debug)]
pub enum ParsingError {
    BooleanParsingError,
    RelationalOperatorParsingError,
    EqualityOperatorParsingError,
    LogicalOperatorParsingError,
    AdditiveOperatorParsingError,
    MultiplicativeOperatorParsingError,
    UnaryOperatorParsingError,
}

impl Display for ParsingError {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            ParsingError::BooleanParsingError => {
                write!(f, "BooleanParsingError")?;
            }
            ParsingError::RelationalOperatorParsingError => {
                write!(f, "RelationalOperatorParsingError")?;
            }
            ParsingError::EqualityOperatorParsingError => {
                write!(f, "EqualityOperatorParsingError")?;
            }
            ParsingError::LogicalOperatorParsingError => {
                write!(f, "LogicalOperatorParsingError")?;
            }
            ParsingError::AdditiveOperatorParsingError => {
                write!(f, "AdditiveOperatorParsingError")?;
            }
            ParsingError::MultiplicativeOperatorParsingError => {
                write!(f, "MultiplicativeOperatorParsingError")?;
            }
            ParsingError::UnaryOperatorParsingError => {
                write!(f, "UnaryOperatorParsingError")?;
            }
        }
        Ok(())
    }
}

impl std::error::Error for ParsingError {}
