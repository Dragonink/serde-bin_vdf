//! Serialize a Rust data structure to Binary VDF

use std::{convert::Infallible, ffi::CString, io::Write};

use serde::{
	Serialize,
	ser::{Impossible, SerializeMap, SerializeStruct},
};

use crate::DataType;

/// Serializes an instance of type `T` to bytes of Binary VDF
///
/// See [`to_writer`] to output the Binary VDF into a generic [writer](Write).
///
/// # Errors
/// This conversion can fail if the given value cannot be serialized.
pub fn to_bytes<T: Serialize + ?Sized>(value: &T) -> crate::Result<Vec<u8>, Infallible> {
	let mut bytes = Vec::new();
	to_writer(&mut bytes, value)?;
	Ok(bytes)
}

/// Serializes an instance of type `T` to a writer of Binary VDF
///
/// See [`to_bytes`] to get a [`Vec<u8>`].
///
/// # Errors
/// This conversion can fail if the given value cannot be serialized.
pub fn to_writer<W: Write, T: Serialize + ?Sized>(
	writer: W,
	value: &T,
) -> crate::Result<(), Infallible> {
	value.serialize(&mut Serializer::new(writer))
}

/// Serializes Rust values into Binary VDF
#[derive(Debug)]
pub struct Serializer<W> {
	/// Output writer
	output: W,

	/// Serialized key of the pair currently being serialized
	current_key: Option<Vec<u8>>,
}
impl<W> Serializer<W> {
	/// Constructs a new [`Serializer`] from an output writer
	pub const fn new(output: W) -> Self {
		Self {
			output,
			current_key: None,
		}
	}
}
impl<W> Serializer<W>
where
	W: Write,
{
	/// Writes the [current key](Self#structfield.current_key)
	///
	/// # Errors
	/// - [`MissingKey`](crate::Error::MissingKey) if a key does not exist
	/// - [`IoError`](crate::Error::IoError) if the writing fails
	fn write_current_key<E>(&mut self) -> crate::Result<(), E> {
		match self.current_key.take() {
			Some(buf) => {
				self.output.write_all(&buf)?;

				Ok(())
			}
			None => Err(crate::Error::MissingKey),
		}
	}

	/// Writes the current key-value pair
	///
	/// # Errors
	/// - [`MissingKey`](crate::Error::MissingKey) if a key does not exist
	/// - [`IoError`](crate::Error::IoError) if the writing fails
	fn write_key_value<E>(&mut self, data_type: DataType, buf: &[u8]) -> crate::Result<(), E> {
		self.output.write_all(&[data_type as u8])?;
		self.write_current_key()?;
		self.output.write_all(buf)?;

		Ok(())
	}
}
impl<W> serde::ser::Serializer for &mut Serializer<W>
where
	W: Write,
{
	type Ok = ();
	type Error = crate::Error<Infallible>;

	type SerializeMap = Self;
	type SerializeStruct = Self;
	type SerializeSeq = Impossible<Self::Ok, Self::Error>;
	type SerializeTuple = Self::SerializeSeq;
	type SerializeTupleStruct = Self::SerializeTuple;
	type SerializeStructVariant = Impossible<Self::Ok, Self::Error>;
	type SerializeTupleVariant = Self::SerializeTupleStruct;

	fn serialize_map(self, _len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
		if let Some(key) = self.current_key.take() {
			self.output.write_all(&[DataType::NestedDocument as u8])?;
			self.output.write_all(&key)?;
		}

		Ok(self)
	}

	fn serialize_struct(
		self,
		_name: &'static str,
		len: usize,
	) -> Result<Self::SerializeStruct, Self::Error> {
		self.serialize_map(Some(len))
	}

	fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
		let s = CString::new(v)?;
		let s = s.as_bytes_with_nul();

		if self.current_key.is_some() {
			self.write_key_value(DataType::String, s)
		} else {
			self.output.write_all(s)?;

			Ok(())
		}
	}

	fn serialize_char(self, v: char) -> Result<Self::Ok, Self::Error> {
		self.serialize_str(&v.to_string())
	}

	fn serialize_i8(self, _v: i8) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("i8"))
	}

	fn serialize_i16(self, _v: i16) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("i16"))
	}

	fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> {
		self.write_key_value(DataType::Int, &v.to_le_bytes())
	}

	fn serialize_i64(self, _v: i64) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("i64"))
	}

	fn serialize_i128(self, _v: i128) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("i128"))
	}

	fn serialize_f32(self, v: f32) -> Result<Self::Ok, Self::Error> {
		self.write_key_value(DataType::Float, &v.to_le_bytes())
	}

	fn serialize_f64(self, _v: f64) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("f64"))
	}

	fn serialize_u8(self, _v: u8) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("u8"))
	}

	fn serialize_u16(self, _v: u16) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("u16"))
	}

	fn serialize_u32(self, _v: u32) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("u32"))
	}

	fn serialize_u64(self, v: u64) -> Result<Self::Ok, Self::Error> {
		self.write_key_value(DataType::Uint64, &v.to_le_bytes())
	}

	fn serialize_u128(self, _v: u128) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("u128"))
	}

	fn serialize_bool(self, v: bool) -> Result<Self::Ok, Self::Error> {
		self.serialize_i32(v.into())
	}

	fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("None"))
	}

	fn serialize_some<T: Serialize + ?Sized>(self, value: &T) -> Result<Self::Ok, Self::Error> {
		value.serialize(self)
	}

	fn serialize_unit_variant(
		self,
		_name: &'static str,
		_variant_index: u32,
		variant: &'static str,
	) -> Result<Self::Ok, Self::Error> {
		self.serialize_str(variant)
	}

	fn serialize_struct_variant(
		self,
		_name: &'static str,
		_variant_index: u32,
		_variant: &'static str,
		_len: usize,
	) -> Result<Self::SerializeStructVariant, Self::Error> {
		Err(crate::Error::CannotSerialize("struct_variant"))
	}

	fn serialize_newtype_variant<T>(
		self,
		_name: &'static str,
		_variant_index: u32,
		_variant: &'static str,
		_value: &T,
	) -> Result<Self::Ok, Self::Error>
	where
		T: ?Sized + Serialize,
	{
		Err(crate::Error::CannotSerialize("newtype_variant"))
	}

	fn serialize_tuple_variant(
		self,
		_name: &'static str,
		_variant_index: u32,
		_variant: &'static str,
		_len: usize,
	) -> Result<Self::SerializeTupleVariant, Self::Error> {
		Err(crate::Error::CannotSerialize("tuple_variant"))
	}

	fn serialize_newtype_struct<T: Serialize + ?Sized>(
		self,
		_name: &'static str,
		_value: &T,
	) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("newtype_struct"))
	}

	fn serialize_tuple(self, _len: usize) -> Result<Self::SerializeTuple, Self::Error> {
		Err(crate::Error::CannotSerialize("tuple"))
	}

	fn serialize_tuple_struct(
		self,
		_name: &'static str,
		_len: usize,
	) -> Result<Self::SerializeTupleStruct, Self::Error> {
		Err(crate::Error::CannotSerialize("tuple_struct"))
	}

	fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("unit"))
	}

	fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("unit_struct"))
	}

	fn serialize_seq(self, _len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
		Err(crate::Error::CannotSerialize("seq"))
	}

	fn serialize_bytes(self, _v: &[u8]) -> Result<Self::Ok, Self::Error> {
		Err(crate::Error::CannotSerialize("bytes"))
	}

	fn is_human_readable(&self) -> bool {
		false
	}
}
impl<W> SerializeMap for &mut Serializer<W>
where
	W: Write,
{
	type Ok = ();
	type Error = crate::Error<Infallible>;

	fn serialize_key<T: Serialize + ?Sized>(&mut self, key: &T) -> Result<(), Self::Error> {
		let mut ser = Serializer::new(Vec::new());
		key.serialize(&mut ser)?;
		self.current_key = Some(ser.output);

		Ok(())
	}

	fn serialize_value<T: Serialize + ?Sized>(&mut self, value: &T) -> Result<(), Self::Error> {
		value.serialize(&mut **self)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		self.output.write_all(&[DataType::EndDocument as u8])?;

		Ok(())
	}
}
impl<W> SerializeStruct for &mut Serializer<W>
where
	W: Write,
{
	type Ok = ();
	type Error = crate::Error<Infallible>;

	fn serialize_field<T: Serialize + ?Sized>(
		&mut self,
		key: &'static str,
		value: &T,
	) -> Result<(), Self::Error> {
		self.serialize_entry(key, value)
	}

	fn end(self) -> Result<Self::Ok, Self::Error> {
		SerializeMap::end(self)
	}
}

#[cfg(test)]
#[expect(clippy::unwrap_used, clippy::missing_panics_doc, reason = "tests")]
mod tests {
	use winnow::error::TreeError;

	use crate::tests::Shortcuts;

	use super::*;

	#[test]
	fn shortcuts() {
		let input = to_bytes(&crate::tests::shortcuts()).unwrap();

		assert_eq!(
			crate::from_bytes_with_error::<_, TreeError<_>, Shortcuts>(&input).unwrap(),
			crate::tests::shortcuts()
		);
	}
}
