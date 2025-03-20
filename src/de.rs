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

/// Deserializes an instanece of type `T` from bytes of Binary VDF
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

/// Deserializes an instanece of type `T` from bytes of Binary VDF
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

/// Tag of Binary VDF data types
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, TryFromPrimitive)]
#[repr(u8)]
#[non_exhaustive]
pub enum DataType {
	/// Start of a Binary VDF document
	NestedDocument = 0,
	/// Null-terminated byte string
	String = 1,
	/// 32-bit little-endian signed integer
	Int = 2,
	/// 32-bit little-endian [IEEE-754](https://en.wikipedia.org/wiki/IEEE_754) single-precision floating point number
	Float = 3,
	/// 32-bit pointer
	Pointer = 4,
	/// Little-endian [UCS-2](https://en.wikipedia.org/wiki/Universal_Coded_Character_Set) codepoint string prefixed by a 16-bit little-endian signed integer representing its length
	WideString = 5,
	/// RGBA8888 color
	Color = 6,
	/// 64-bit little-endian unsigned integer
	Uint64 = 7,
	/// End of a Binary VDF document
	EndDocument = 8,
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
mod tests {
	use std::{collections::HashMap, io::Read as _, path::Path};

	use winnow::error::TreeError;

	use super::*;

	const ROOT_DIR: &str = env!("CARGO_MANIFEST_DIR");

	#[test]
	fn strings() {
		assert!(
			parse_string::<_, TreeError<_>>(0..)
				.parse_next(&mut b"".as_slice())
				.is_err()
		);
		assert_eq!(
			parse_string::<_, TreeError<_>>(0..)
				.parse_next(&mut b"\0".as_slice())
				.unwrap(),
			""
		);

		assert!(
			parse_string::<_, TreeError<_>>(3)
				.parse_next(&mut b"abc".as_slice())
				.is_err()
		);
		assert_eq!(
			parse_string::<_, TreeError<_>>(3)
				.parse_next(&mut b"abc\0".as_slice())
				.unwrap(),
			"abc"
		);
	}

	#[test]
	fn shortcuts() {
		#[derive(Debug, Deserialize, PartialEq, Eq)]
		struct Shortcuts<'input> {
			#[serde(borrow)]
			shortcuts: HashMap<&'input str, Shortcut<'input>>,
		}
		#[derive(Debug, Deserialize, PartialEq, Eq)]
		struct Shortcut<'input> {
			#[serde(rename = "appid")]
			app_id: i32,
			#[serde(rename = "AppName")]
			app_name: &'input str,
			#[serde(rename = "Exe")]
			exe: &'input str,
			#[serde(rename = "StartDir")]
			start_dir: &'input str,
			icon: &'input str,
			#[serde(rename = "ShortcutPath")]
			shortcut_path: &'input str,
			#[serde(rename = "LaunchOptions")]
			launch_options: &'input str,
			#[serde(rename = "IsHidden")]
			is_hidden: bool,
			#[serde(rename = "AllowDesktopConfig")]
			allow_desktop_config: bool,
			#[serde(rename = "AllowOverlay")]
			allow_overlay: bool,
			#[serde(rename = "OpenVR")]
			open_vr: bool,
			#[serde(rename = "Devkit")]
			devkit: bool,
			#[serde(rename = "DevkitGameID")]
			devkit_game_id: &'input str,
			#[serde(rename = "DevkitOverrideAppID")]
			devkit_override_app_id: bool,
			#[serde(rename = "LastPlayTime")]
			last_play_time: i32,
			#[serde(rename = "FlatpakAppID")]
			flatpak_app_id: &'input str,
			#[serde(borrow)]
			tags: HashMap<&'input str, &'input str>,
		}

		let mut input = Vec::new();
		std::fs::File::open(Path::new(ROOT_DIR).join("tests").join("shortcuts.vdf"))
			.unwrap()
			.read_to_end(&mut input)
			.unwrap();

		assert_eq!(
			from_bytes_with_error::<_, TreeError<_>, Shortcuts>(&input).unwrap(),
			Shortcuts {
				shortcuts: HashMap::from([(
					"0",
					Shortcut {
						app_id: -1731828815,
						app_name: "Minecraft (Latest release)",
						exe: "prismlauncher",
						start_dir: "/home/dragonink/Games/Minecraft",
						icon: "/home/dragonink/Games/Minecraft/instances/Latest release/icon.png",
						shortcut_path: "/usr/share/applications/org.prismlauncher.PrismLauncher.desktop",
						launch_options: "--launch 'Latest release'",
						is_hidden: false,
						allow_desktop_config: true,
						allow_overlay: true,
						open_vr: false,
						devkit: false,
						devkit_game_id: "",
						devkit_override_app_id: false,
						last_play_time: 1737202183,
						flatpak_app_id: "",
						tags: HashMap::new(),
					}
				)])
			}
		);
	}
}
