//! [`serde`] format library for the [Steam Binary KeyValues](https://developer.valvesoftware.com/wiki/Binary_VDF) format

use num_enum::TryFromPrimitive;

pub mod de;
pub mod error;
pub mod ser;

pub use de::{Deserializer, from_bytes, from_bytes_with_error};
pub use error::{Error, Result};
pub use ser::{Serializer, to_bytes, to_writer};

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

#[cfg(test)]
mod tests {
	use std::{
		collections::HashMap,
		io::{self, Read as _},
		path::Path,
	};

	use serde::{Deserialize, Serialize};

	#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
	pub(crate) struct Shortcuts<'input> {
		#[serde(borrow)]
		pub(crate) shortcuts: HashMap<&'input str, Shortcut<'input>>,
	}

	#[expect(
		clippy::struct_excessive_bools,
		clippy::struct_field_names,
		reason = "struct of the shortcuts.vdf of Steam"
	)]
	#[derive(Debug, Deserialize, Serialize, PartialEq, Eq)]
	pub(crate) struct Shortcut<'input> {
		#[serde(rename = "appid")]
		pub(crate) app_id: i32,
		#[serde(rename = "AppName")]
		pub(crate) app_name: &'input str,
		#[serde(rename = "Exe")]
		pub(crate) exe: &'input str,
		#[serde(rename = "StartDir")]
		pub(crate) start_dir: &'input str,
		pub(crate) icon: &'input str,
		#[serde(rename = "ShortcutPath")]
		pub(crate) shortcut_path: &'input str,
		#[serde(rename = "LaunchOptions")]
		pub(crate) launch_options: &'input str,
		#[serde(rename = "IsHidden")]
		pub(crate) is_hidden: bool,
		#[serde(rename = "AllowDesktopConfig")]
		pub(crate) allow_desktop_config: bool,
		#[serde(rename = "AllowOverlay")]
		pub(crate) allow_overlay: bool,
		#[serde(rename = "OpenVR")]
		pub(crate) open_vr: bool,
		#[serde(rename = "Devkit")]
		pub(crate) devkit: bool,
		#[serde(rename = "DevkitGameID")]
		pub(crate) devkit_game_id: &'input str,
		#[serde(rename = "DevkitOverrideAppID")]
		pub(crate) devkit_override_app_id: bool,
		#[serde(rename = "LastPlayTime")]
		pub(crate) last_play_time: i32,
		#[serde(rename = "FlatpakAppID")]
		pub(crate) flatpak_app_id: &'input str,
		#[serde(borrow)]
		pub(crate) tags: HashMap<&'input str, &'input str>,
	}

	#[expect(clippy::missing_errors_doc, reason = "utility for tests")]
	pub(crate) fn read_shortcuts_file() -> io::Result<Vec<u8>> {
		const ROOT_DIR: &str = env!("CARGO_MANIFEST_DIR");

		let mut buf = Vec::new();
		std::fs::File::open(Path::new(ROOT_DIR).join("tests").join("shortcuts.vdf"))?
			.read_to_end(&mut buf)?;

		Ok(buf)
	}

	#[expect(
		clippy::unreadable_literal,
		reason = "literals taken from shortcuts.vdf"
	)]
	pub(crate) fn shortcuts() -> Shortcuts<'static> {
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
				},
			)]),
		}
	}
}
