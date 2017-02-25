"use strict";

// I'm so sorry you have to see this mess

const log = console.log.bind(console);
var fs = require("fs");
var path = require("path");
var table = fs.readFileSync("table.txt", { encoding: "utf8" });
var lines = table.split("\n");

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

function parseBranchInstr(reader) {
	let mnemonic = reader.takeUntil("|").trim();
	let shortOpcode = reader.takeUntil("|").trim();
	shortOpcode = parseInt(shortOpcode, 16);

	let longMnemonic = reader.takeUntil("|").trim();
	let longPage = reader.take() == "!" ? 2 : 1;
	let longOpcode = reader.takeUntil("|").trim();
	longOpcode = parseInt(longOpcode, 16);
	let longBytes = (longPage == 2) ? 4 : 3;
	let longCycles = 5;
	if(mnemonic == "BSR") longCycles = 9;

	return {
		mnemonic: mnemonic,
		opcodes: {
			relative8: {
				opcode: shortOpcode,
				page: 1,
				cycles: 3,
				bytes: 2
			},
			relative16: {
				opcode: longOpcode,
				page: longPage,
				cycles: longCycles,
				bytes: longBytes
			}
		}
	}
}

lines.forEach(line => {
	let reader = new Reader(line);

	let first = reader.take();
	if(first === "b") {
		// Branch instr
		let second = reader.take();
		if(second !== "|") return;

		instructions.push(parseBranchInstr(reader));
		return;
	}
	else if(first !== "|") {
		return;
	}

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
`.replace("\n", "\n");

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
		}).join(",\n")
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

	return lines.join(",\n");
}).filter(x => x).join(",\n");

outStr = [
	pageMatch(2, page2),
	pageMatch(3, page3),
	outStr
].join(",\n");

outStr = "match op {\n"
	+ outStr
		.split(/\n\n/)
		.map(x => "\t" + x)
		.join("\n")
	+ ",\n\t_ => invalid_opcode!(op)\n}"
	+ "\n\n";

outStr += instructions.map(instr => `
fn instr_${instr.mnemonic.toLowerCase()}(&mut self, mobo: &Motherboard${instr.opcodes.inherent !== undefined ? '' : ", addr: u16"}) {
	panic!("Unimplemented instruction ${instr.mnemonic}");
}
`).join("");

fs.writeFileSync("match.rs", outStr);

let instructionsFile = "#[derive(Debug)]\npub enum Opcode {\n";

for(let i = 0; i < instructions.length; ++i) {
	let instr = instructions[i];
	let camelCaseMnemonic = instr.mnemonic[0].toUpperCase() + instr.mnemonic.toLowerCase().slice(1);
	instructionsFile += `\t${camelCaseMnemonic},\n`;
}

instructionsFile += `

match op {
	PAGE_2 => {
		pc += 1;
		let op = mem.read_u8(pc);
		match op {
${buildCases(2, 3)}

			_ => None
		}
	},
	PAGE_3 => {
		pc += 1;
		let op = mem.read_u8(pc);
		match op {
${buildCases(3, 3)}

			_ => None
		}
	},
${buildCases(1, 1)}
	_ => None
}`;

function buildCases(page, indentLevel) {
	var output = "";
	for(let i = 0; i < instructions.length; ++i) {
		let instr = instructions[i];

		Object.keys(instr.opcodes).forEach(variant => {
			let op = instr.opcodes[variant];
			if(op === undefined || op.page != page) return;

			let camelCaseMnemonic = instr.mnemonic[0].toUpperCase() + instr.mnemonic.toLowerCase().slice(1);
			let addressing = variant;

			if(variant == "immediate") {
				let skipBytes = 1 + (contains([2,3], op.page) ? 1 : 0);
				addressing += (op.bytes - skipBytes) * 8;
			}

			if(addressing == "inherent") {
				addressing += "()";
			} else {
				addressing += "(mem, pc)"
			}

			let indent = "";
			for(var ind = 0; ind < indentLevel; ++ind) {
				indent += "\t";
			}
			
			let hexop = "0x" + pad(2, "0", op.opcode.toString(16));

			output += `${indent}${hexop} => instr(Mnemonic::${camelCaseMnemonic}, ${addressing}),\n`;
		});

	}

	return output;
}

fs.writeFileSync(path.join(__dirname, "debugger.rs"), instructionsFile);

fs.writeFileSync(path.join(__dirname, "../instructions.json"), JSON.stringify(instructions, undefined, "  "))