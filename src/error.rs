//! (De)serialization gone wrong

use std::{
	fmt::{self, Debug, Display, Formatter},
	str::Utf8Error,
};

use num_enum::TryFromPrimitiveError;
use winnow::{
	error::{FromExternalError, ParserError},
	stream::Stream,
};

use crate::de::DataType;

/// Possible errors that can occur when (de)serializing Binary VDF data
#[derive(Debug, Clone)]
#[non_exhaustive]
pub enum Error<E = winnow::error::ContextError> {
	/// Generic error message
	Message(String),
	/// Error from the parser
	ParserError(E),
	/// A data type has not been parsed
	MissingDataType,
	/// This data type is not supported
	UnsupportedDataType(DataType),
	/// This data type is not known
	UnknownDataType(TryFromPrimitiveError<DataType>),
	/// The expected and parsed data types are different
	InvalidDataType {
		/// Expected data type
		expected: DataType,
		/// Parsed data type
		parsed: DataType,
	},
	/// A string is not UTF-8
	Utf8Error(Utf8Error),
}
impl<E> From<TryFromPrimitiveError<DataType>> for Error<E> {
	fn from(err: TryFromPrimitiveError<DataType>) -> Self {
		Self::UnknownDataType(err)
	}
}
impl<E> From<Utf8Error> for Error<E> {
	fn from(err: Utf8Error) -> Self {
		Self::Utf8Error(err)
	}
}
impl<E: Display> Display for Error<E> {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match *self {
			Self::Message(ref msg) => f.write_str(msg),
			Self::ParserError(ref err) => Display::fmt(err, f),
			Self::MissingDataType => write!(f, "data type has not been parsed"),
			Self::UnsupportedDataType(data_type) => {
				write!(f, "{data_type:?} data type is not supported")
			}
			Self::UnknownDataType(ref err) => Display::fmt(err, f),
			Self::InvalidDataType { expected, parsed } => {
				write!(f, "expected {expected:?} data type, parsed {parsed:?}")
			}
			Self::Utf8Error(ref err) => Display::fmt(err, f),
		}
	}
}
impl<E> std::error::Error for Error<E>
where
	E: Debug + Display,
{
	fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
		match *self {
			Self::Message(_)
			| Self::ParserError(_)
			| Self::MissingDataType
			| Self::UnsupportedDataType(_)
			| Self::InvalidDataType { .. } => None,
			Self::UnknownDataType(ref err) => Some(err),
			Self::Utf8Error(ref err) => Some(err),
		}
	}
}
impl<I, E: ParserError<I>> ParserError<I> for Error<E>
where
	I: Stream,
{
	type Inner = Self;

	fn from_input(input: &I) -> Self {
		Self::ParserError(E::from_input(input))
	}

	fn into_inner(self) -> winnow::Result<Self::Inner, Self> {
		Ok(self)
	}
}
impl<I, EE: Into<Self>, E> FromExternalError<I, EE> for Error<E> {
	fn from_external_error(_input: &I, err: EE) -> Self {
		err.into()
	}
}
impl<E> serde::de::Error for Error<E>
where
	E: Debug + Display,
{
	fn custom<T: Display>(msg: T) -> Self {
		Self::Message(msg.to_string())
	}
}

/// Specialized `Result` for [`Error`]
pub type Result<T, E = winnow::error::ContextError> = std::result::Result<T, Error<E>>;
