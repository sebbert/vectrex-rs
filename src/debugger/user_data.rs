use debugger::label_registry::*;
use std::collections::HashSet;
use std::path::*;
use std::env::home_dir;
use serde_json;
use std::fs::File;
use std::io::prelude::*;
use std::io;

use debugger::Debugger;

#[derive(Default,Serialize,Deserialize)]
pub struct UserData {
	pub breakpoints: HashSet<u16>,
	pub labels: LabelRegistry
}

impl UserData {
	pub fn path() -> PathBuf {
		let mut dir = home_dir().unwrap();
		dir.push(".vxrs-debugger.json");
		dir
	}

	pub fn save(&self) -> io::Result<()> {
		let json = serde_json::to_string(self).unwrap();
		File::create(Self::path())?.write_all(json.as_ref())?;
		Ok(())
	}

	pub fn load() -> Option<UserData> {
		File::open(Self::path()).ok()
			.and_then(move |mut file| {
				let mut contents = String::new();
				file.read_to_string(&mut contents).unwrap_or_default();
				serde_json::from_str(&contents).ok()
			})
	}
}

impl Debugger {
	pub fn user_data(&self) -> UserData {
		UserData {
			breakpoints: self.breakpoints.clone(),
			labels: self.labels.clone()
		}
	}
}