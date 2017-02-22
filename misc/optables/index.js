"use strict";

// I'm so sorry you have to see this mess

const log = console.log.bind(console);
var fs = require("fs");
var path = require("path");
var table = fs.readFileSync("table.txt", { encoding: "utf8" });
var lines = table.split("\r\n");

var instructions = [];

const pad = (n, c, s) => {
	if(n <= s.length) return s;
	for(var i = 0; i < (n - s.length); ++i) {
		s = c + s;
	}
	return s;
}

const contains = (haystack, needle) => haystack.indexOf(needle) != -1;

class Reader {
	constructor(str) {
		this.str = str;
		this.index = 0;
	}

	take(n) {
		n = (typeof n === "undefined") ? 1 : n;
		let str = this.str.substr(this.index, n);
		this.index += n;
		return str;
	}

	takeUntil(seq) {
		let current = "";
		while(this.str.substr(this.index, seq.length) != seq && this.index < this.str.length) {
			current += this.str[this.index];
			this.index++;
		}
		this.index += seq.length;
		return current;
	}

	takeRest() {
		let lastIndex = this.str.length - 1;
		let str = this.str.substring(this.index, lastIndex);
		this.index = lastIndex;
	}
}

function parseVariant(lineReader) {
	let variantStr = lineReader.takeUntil("|");
	if(variantStr.trim().length == 0) return undefined;
	let reader = new Reader(variantStr);

	let pagePrefix = reader.take();
	let page = 1;
	if(pagePrefix === "!") page = 2;
	if(pagePrefix === "@") page = 3;

	let opcodeStr = reader.takeUntil(" ").trim();
	let cyclesStr = reader.takeUntil(" ").trim();
	let bytesStr = reader.takeUntil("|").trim();

	let opcode = parseInt(opcodeStr, 16);

	let cycles6809 = cyclesStr.split("/")[0].replace("+","");
	let cycles = parseInt(cycles6809, 10);

	let bytes = parseInt(bytesStr, 10);

	return {
		page,
		opcode,
		cycles,
		bytes
	};
}

lines.forEach(line => {
	let reader = new Reader(line);

	if(reader.take() !== "|") return;

	let second = reader.take();
	if(second == "*" || second == "-") return;
	
	let mnemonic = reader.takeUntil("|").trim().replace(/\s/g, "");
	if(mnemonic.length == 0 || mnemonic === "Mnem") return;
	
	let immediate = parseVariant(reader);
	let direct = parseVariant(reader);
	let indexed = parseVariant(reader);
	let extended = parseVariant(reader);
	let inherent = parseVariant(reader);

	instructions.push({
		mnemonic,
		opcodes: { immediate, direct, indexed, extended, inherent }
	});
});

let page2 = [];
let page3 = [];

function makeBranch(instr, variant) {
	let op = instr.opcodes[variant];

/*	let line = "OP_" + instr.mnemonic;

	if(variant != "inherent") {
		line += "_" + variant.toUpperCase();
	}
*/
	let line = "0x" + pad(2, "0", op.opcode.toString(16));

	let variantMacro = variant; 
	if(variant == "immediate") {
		let skipBytes = 1 + (contains([2,3], op.page) ? 1 : 0);
		variantMacro += (op.bytes - skipBytes) * 8;
	}

	line += ` => ${variantMacro}!(Self::instr_${instr.mnemonic.toLowerCase()}, ${op.cycles})`;
	return line;
}

function pageMatch(pageNumber, page) {
	let preamble =
`OP_PAGE_${pageNumber} => {
	let op = motherboard.read_u8(next_pc);
	next_pc = next_pc.wrapping_add(1);

	match op {
`.replace("\n", "\r\n");

	let postamble =`,
		_ => invalid_opcode!(op)
	}
}`;

	return preamble
		+ page.map(pd => {
			let instr = pd.instr;
			let variant = pd.variant;
			let op = instr[variant];

			return "\t\t" + makeBranch(instr, variant);
		}).join(",\r\n")
		+ postamble;
}


let outStr = instructions.filter(x => x).map(instr => {
	let lines = [];

	for(let variant in instr.opcodes) {
		let op = instr.opcodes[variant];
		if(op === undefined) continue;
		if(op.page == 2) {
			page2.push({instr, variant});
			continue;
		}
		if(op.page == 3) {
			page3.push({instr, variant});
			continue;
		}

		let branch = makeBranch(instr, variant);
		if(branch) lines.push(branch);
	}

	return lines.join(",\r\n");
}).filter(x => x).join(",\r\n");

outStr = [
	pageMatch(2, page2),
	pageMatch(3, page3),
	outStr
].join(",\r\n");

outStr = "match op {\r\n"
	+ outStr
		.split(/\r\n|\n/)
		.map(x => "\t" + x)
		.join("\r\n")
	+ ",\r\n\t_ => invalid_opcode!(op)\r\n}"
	+ "\r\n\r\n";

outStr += instructions.map(instr => `
fn instr_${instr.mnemonic.toLowerCase()}(&mut self, mobo: &Motherboard${instr.opcodes.inherent !== undefined ? '' : ", addr: u16"}) {
	panic!("Unimplemented instruction ${instr.mnemonic}");
}
`).join("");

fs.writeFileSync("match.rs", outStr);

let instructionsFile = "#[derive(Debug)]\r\npub enum Opcode {\r\n";

for(let i = 0; i < instructions.length; ++i) {
	let instr = instructions[i];
	let camelCaseMnemonic = instr.mnemonic[0].toUpperCase() + instr.mnemonic.toLowerCase().slice(1);
	instructionsFile += `\t${camelCaseMnemonic},\r\n`;
}

instructionsFile += `}

enum AddressingMode {
	Inherent,
	Immediate,
	Direct,
	Extended,
	Indexed
}

impl Opcode {
	pub fn from_u16(op_16: u16) -> Opcode {
		let hi_op = (op_16 >> 8) as u8;
		match hi_op {
			0x10 => {
				let op = op_16 as u8;
				match op {
${buildCases(2, 5)}
					
					_ => panic!("Unknown opcode 0x{:02x}", op)
				}
			},
			0x11 => {
				let op = op_16 as u8;

				match op {
${buildCases(3, 5)}
					_ => panic!("Unknown opcode 0x{:02x}", op)
				}
			}
			_ => Self::from_u8(hi_op)
		}
	}

	pub fn from_u8(op: u8) -> Opcode {
		match op {
`;

function buildCases(page, indentLevel) {
	var output = "";
	for(let i = 0; i < instructions.length; ++i) {
		let instr = instructions[i];
		let variants = Object.keys(instr.opcodes).filter(x => {
			let op = instr.opcodes[x];
			return op != undefined && op.page == page;
		});
		if(variants.length == 0) continue;
		let camelCaseMnemonic = instr.mnemonic[0].toUpperCase() + instr.mnemonic.toLowerCase().slice(1);
		let indent = "";
		for(var ind = 0; ind < indentLevel; ++ind) {
			indent += "\t";
		}

		let cases = variants.map((v, i) => {
			//let variantSuffix = (v == "inherent") ? "" : `_${v.toUpperCase()}`;
			let hexop = "0x" + pad(2, "0", instr.opcodes[v].opcode.toString(16));
			let insertNewline = (i+1)%2 == 0;

			return hexop
			     + ((i == variants.length-1) ? "" : "|")
			     //+ (insertNewline ? `\r\n${indent}` : "");
		}).join("");

		let singleVariant = variants.length == 1;

		output += `${indent}${singleVariant?"":""}${cases} => Opcode::${camelCaseMnemonic},\r\n`;

		let nextIsSingleVariant = true;
		if(i < instructions.length - 1) {
			let nextInstr = instructions[i+1];
			let nextVariants = Object.keys(nextInstr.opcodes).filter(x => nextInstr.opcodes[x]);
			nextIsSingleVariant = nextVariants.length == 1;
		}

		
	}

	return output;
}

instructionsFile += buildCases(1, 3)

instructionsFile += `
			_ => panic!("Unknown opcode 0x{:02x}", op)
		}
	}
}
`;


instructionsFile = instructionsFile.replace(/\r\n|\n/g, "\r\n");

fs.writeFileSync(path.join(__dirname, "../../src/debugger.rs"), instructionsFile);

fs.writeFileSync(path.join(__dirname, "../instructions.json"), JSON.stringify(instructions, undefined, "  "))