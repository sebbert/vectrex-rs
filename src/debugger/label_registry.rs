use std::collections::HashMap;

pub struct Label {
	label: String,
	address: u16
}

#[derive(Default,Clone,Serialize,Deserialize)]
pub struct LabelRegistry {
	pub map: HashMap<String, u16>
}

impl LabelRegistry {
	pub fn address_for_label(&self, label: &str) -> Option<u16> {
		self.map.get(label).map(|a| *a)
	}

	pub fn label_for_address(&self, address: u16) -> Option<String> {
		self.map
			.iter()
			.find(|p| *p.1 == address)
			.map(|p| p.0.to_string())
	}

	pub fn set_label(&mut self, label: String, address: u16) -> Option<u16> {
		self.map.insert(label, address)
	}

	pub fn remove_label(&mut self, label: &str) -> Option<u16> {
		self.map.remove(label)
	}
}