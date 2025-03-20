//! [`serde`] format library for the [Steam Binary KeyValues](https://developer.valvesoftware.com/wiki/Binary_VDF) format

pub mod de;
pub mod error;

pub use de::{Deserializer, from_bytes, from_bytes_with_error};
pub use error::{Error, Result};
