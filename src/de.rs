//! Deserialize Binary VDF to a Rust data structure

use std::{
	fmt::{Debug, Display},
	marker::PhantomData,
	str::Utf8Error,
};

use num_enum::{TryFromPrimitive, TryFromPrimitiveError};
use serde::{
	Deserialize,
	de::{DeserializeSeed, IntoDeserializer as _, MapAccess, Visitor},
};
use winnow::{
	BStr,
	error::{FromExternalError, ParserError},
	prelude::*,
	stream::{Compare, FindSlice, Range, Stream, StreamIsPartial},
};

use crate::DataType;

/// Deserializes an instance of type `T` from bytes of Binary VDF
///
/// See [`from_bytes_with_error`] to customize the type of [`Error::ParserError`](crate::Error::ParserError).
///
/// # Errors
/// This conversion can fail if the structure of the input does not match the structure expected by `T`,
/// or if the input is malformed.
pub fn from_bytes<'de, I, T: Deserialize<'de>>(input: &'de I) -> crate::Result<T>
where
	I: AsRef<[u8]> + ?Sized,
{
	from_bytes_with_error(input)
}

/// Deserializes an instance of type `T` from bytes of Binary VDF
///
/// This function allows the caller to customize the type of [`Error::ParserError`](crate::Error::ParserError).
/// See [`from_bytes`] to use the default type.
///
/// # Errors
/// This conversion can fail if the structure of the input does not match the structure expected by `T`,
/// or if the input is malformed.
pub fn from_bytes_with_error<'de, I, E, T: Deserialize<'de>>(input: &'de I) -> crate::Result<T, E>
where
	I: AsRef<[u8]> + ?Sized,
	E: Debug
		+ Display
		+ ParserError<&'de BStr>
		+ FromExternalError<&'de BStr, TryFromPrimitiveError<DataType>>
		+ FromExternalError<&'de BStr, Utf8Error>,
{
	T::deserialize(&mut Deserializer::new(input))
}

/// Deserializes Binary VDF into Rust values
#[derive(Debug)]
pub struct Deserializer<'de, E> {
	/// Input buffer
	input: &'de BStr,
	/// Parser error type
	_parser_error: PhantomData<E>,

	/// Data type of the value that is currently parsed
	current_data_type: Option<DataType>,
	/// The parser is currently parsing a key in a Binary VDF document
	parsing_key: bool,
}
impl<'de, E> Deserializer<'de, E> {
	/// Constructs a new [`Deserializer`] from an input buffer
	pub fn new<I>(input: &'de I) -> Self
	where
		I: AsRef<[u8]> + ?Sized,
	{
		Self {
			input: BStr::new(input),
			_parser_error: PhantomData,
			current_data_type: None,
			parsing_key: false,
		}
	}

	/// Verifies if the [current data type](Self#structfield.current_data_type) is the expected one
	///
	/// # Errors
	/// - [`InvalidDataType`](crate::Error::InvalidDataType) if a data type has been parsed but is not the expected one
	/// - [`MissingDataType`](crate::Error::MissingDataType) if a data type has not been parsed
	fn verify_data_type(&mut self, expected: DataType) -> crate::Result<(), E> {
		match self.current_data_type.take() {
			Some(parsed) if parsed == expected => Ok(()),
			Some(parsed) => Err(crate::Error::InvalidDataType { expected, parsed }),
			None => Err(crate::Error::MissingDataType),
		}
	}
}
impl<'de, E> serde::de::Deserializer<'de> for &mut Deserializer<'de, E>
where
	E: Debug
		+ Display
		+ ParserError<&'de BStr>
		+ FromExternalError<&'de BStr, TryFromPrimitiveError<DataType>>
		+ FromExternalError<&'de BStr, Utf8Error>,
{
	type Error = crate::Error<E>;

	fn deserialize_any<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		if self.parsing_key {
			self.deserialize_identifier(visitor)
		} else {
			match self.current_data_type.map_or_else(
				|| {
					winnow::combinator::trace("deserialize_any", DataType::parse)
						.parse_next(&mut self.input)
				},
				Ok,
			) {
				Ok(DataType::NestedDocument) => self.deserialize_map(visitor),
				Ok(DataType::String) => self.deserialize_str(visitor),
				Ok(DataType::Int) => self.deserialize_i32(visitor),
				Ok(DataType::Float) => self.deserialize_f32(visitor),
				Ok(DataType::Uint64) => self.deserialize_u64(visitor),
				Ok(data_type) => Err(crate::Error::UnsupportedDataType(data_type)),
				Err(err) => Err(err),
			}
		}
	}

	fn deserialize_map<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		if self.current_data_type.is_some() {
			self.verify_data_type(DataType::NestedDocument)?;
		} else {
			winnow::combinator::trace::<_, _, Self::Error>(
				"deserialize_map",
				winnow::combinator::peek(DataType::NestedDocument),
			)
			.parse_next(&mut self.input)?;
		}
		visitor.visit_map(Document::new(self))
	}

	fn deserialize_struct<V: Visitor<'de>>(
		self,
		_name: &'static str,
		_fields: &'static [&'static str],
		visitor: V,
	) -> Result<V::Value, Self::Error> {
		self.deserialize_map(visitor)
	}

	fn deserialize_str<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		if !self.parsing_key {
			self.verify_data_type(DataType::String)?;
		}
		winnow::combinator::trace("deserialize_str", parse_string(0..))
			.parse_next(&mut self.input)
			.and_then(|v| visitor.visit_borrowed_str(v))
	}

	fn deserialize_string<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.deserialize_str(visitor)
	}

	fn deserialize_char<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.verify_data_type(DataType::String)?;
		winnow::combinator::trace(
			"deserialize_char",
			parse_string(1..=size_of::<char>())
				.map(|s| s.chars().next().unwrap_or_else(|| unreachable!())),
		)
		.parse_next(&mut self.input)
		.and_then(|v| visitor.visit_char(v))
	}

	fn deserialize_identifier<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.deserialize_str(visitor)
	}

	fn deserialize_i32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.verify_data_type(DataType::Int)?;
		winnow::combinator::trace("deserialize_i32", winnow::binary::le_i32)
			.parse_next(&mut self.input)
			.and_then(|v| visitor.visit_i32(v))
	}

	fn deserialize_f32<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.verify_data_type(DataType::Float)?;
		winnow::combinator::trace("deserialize_f32", winnow::binary::le_f32)
			.parse_next(&mut self.input)
			.and_then(|v| visitor.visit_f32(v))
	}

	fn deserialize_u64<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.verify_data_type(DataType::Uint64)?;
		winnow::combinator::trace("deserialize_u64", winnow::binary::le_u64)
			.parse_next(&mut self.input)
			.and_then(|v| visitor.visit_u64(v))
	}

	fn deserialize_bool<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		self.verify_data_type(DataType::Int)?;
		winnow::combinator::trace("deserialize_bool", winnow::binary::le_i32.map(|v| v != 0))
			.parse_next(&mut self.input)
			.and_then(|v| visitor.visit_bool(v))
	}

	fn deserialize_option<V: Visitor<'de>>(self, visitor: V) -> Result<V::Value, Self::Error> {
		visitor.visit_some(self)
	}

	fn deserialize_enum<V: Visitor<'de>>(
		self,
		_name: &'static str,
		_variants: &'static [&'static str],
		visitor: V,
	) -> Result<V::Value, Self::Error> {
		winnow::combinator::trace("deserialize_enum", parse_string(0..))
			.parse_next(&mut self.input)
			.and_then(|v| visitor.visit_enum(v.into_deserializer()))
	}

	serde::forward_to_deserialize_any! {
		u8 u16 u32 u128
		i8 i16 i64 i128
		f64
		newtype_struct tuple tuple_struct
		unit unit_struct
		seq byte_buf bytes
		ignored_any
	}

	#[inline]
	fn is_human_readable(&self) -> bool {
		false
	}
}

/// Parses the Binary VDF input into a string slice
///
/// See [`winnow::token::take_until`] for more details.
fn parse_string<'de, I, E>(len: impl Into<Range>) -> impl Parser<I, &'de str, E>
where
	I: Stream<Slice = &'de [u8]> + StreamIsPartial + FindSlice<u8> + Compare<u8>,
	E: ParserError<I> + FromExternalError<I, Utf8Error>,
{
	/// Terminator byte of strings in Binary VDF
	const NUL_TERMINATOR: u8 = 0;

	winnow::combinator::trace(
		"parse_string",
		winnow::combinator::terminated(
			winnow::token::take_until(len, NUL_TERMINATOR),
			NUL_TERMINATOR,
		)
		.try_map(std::str::from_utf8),
	)
}

/// Accesses maps of Binary VDF
#[derive(Debug)]
struct Document<'d, 'de, E> {
	/// Deserializer
	de: &'d mut Deserializer<'de, E>,
}
impl<'d, 'de, E> Document<'d, 'de, E> {
	/// Constructs a new [`Document`] from a [`Deserializer`]
	const fn new(de: &'d mut Deserializer<'de, E>) -> Self {
		Self { de }
	}
}
impl<'de, E> MapAccess<'de> for Document<'_, 'de, E>
where
	E: Debug
		+ Display
		+ ParserError<&'de BStr>
		+ FromExternalError<&'de BStr, TryFromPrimitiveError<DataType>>
		+ FromExternalError<&'de BStr, Utf8Error>,
{
	type Error = crate::Error<E>;

	fn next_key_seed<K: DeserializeSeed<'de>>(
		&mut self,
		seed: K,
	) -> Result<Option<K::Value>, Self::Error> {
		match winnow::combinator::trace("next_key_seed", DataType::parse)
			.parse_next(&mut self.de.input)
		{
			Ok(DataType::EndDocument) => Ok(None),
			Ok(data_type) => {
				self.de.current_data_type = Some(data_type);
				self.de.parsing_key = true;
				let ret = seed.deserialize(&mut *self.de).map(Some);
				self.de.parsing_key = false;
				ret
			}
			Err(err) => Err(err),
		}
	}

	fn next_value_seed<V: DeserializeSeed<'de>>(
		&mut self,
		seed: V,
	) -> Result<V::Value, Self::Error> {
		seed.deserialize(&mut *self.de)
	}
}

impl DataType {
	/// Parses the Binary VDF input into a [`DataType`] value
	///
	/// # Errors
	/// This parsing can fail if the input is empty,
	/// or if the parsed value does not map to a valid [`DataType`] value.
	fn parse<I, E>(input: &mut I) -> winnow::Result<Self, E>
	where
		I: Stream<Token = u8> + StreamIsPartial,
		E: ParserError<I> + FromExternalError<I, TryFromPrimitiveError<Self>>,
	{
		winnow::combinator::trace(
			"DataType::parse",
			winnow::binary::le_u8.try_map(TryFromPrimitive::try_from_primitive),
		)
		.parse_next(input)
	}
}
impl<I, E> Parser<I, Self, E> for DataType
where
	I: Stream + StreamIsPartial + Compare<u8>,
	E: ParserError<I>,
{
	fn parse_next(&mut self, input: &mut I) -> winnow::Result<Self, E> {
		winnow::combinator::trace(format!("DataType::{self:?}"), (*self as u8).value(*self))
			.parse_next(input)
	}
}

#[cfg(test)]
#[expect(clippy::unwrap_used, clippy::missing_panics_doc, reason = "tests")]
mod tests {
	use winnow::error::TreeError;

	use crate::tests::Shortcuts;

	use super::*;

	#[test]
	fn strings() {
		parse_string::<_, TreeError<_>>(0..)
			.parse_next(&mut b"".as_slice())
			.unwrap_err();
		assert_eq!(
			parse_string::<_, TreeError<_>>(0..)
				.parse_next(&mut b"\0".as_slice())
				.unwrap(),
			""
		);

		parse_string::<_, TreeError<_>>(3)
			.parse_next(&mut b"abc".as_slice())
			.unwrap_err();
		assert_eq!(
			parse_string::<_, TreeError<_>>(3)
				.parse_next(&mut b"abc\0".as_slice())
				.unwrap(),
			"abc"
		);
	}

	#[test]
	fn shortcuts() {
		let input = crate::tests::read_shortcuts_file().unwrap();

		assert_eq!(
			from_bytes_with_error::<_, TreeError<_>, Shortcuts>(&input).unwrap(),
			crate::tests::shortcuts()
		);
	}
}
