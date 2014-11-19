;(function e(t,n,r){function s(o,u){if(!n[o]){if(!t[o]){var a=typeof require=="function"&&require;if(!u&&a)return a(o,!0);if(i)return i(o,!0);throw new Error("Cannot find module '"+o+"'")}var f=n[o]={exports:{}};t[o][0].call(f.exports,function(e){var n=t[o][1][e];return s(n?n:e)},f,f.exports,e,t,n,r)}return n[o].exports}var i=typeof require=="function"&&require;for(var o=0;o<r.length;o++)s(r[o]);return s})({1:[function(require,module,exports){
var tamago = require("./tamago/main.js");

var lastTime = 0;
var vendors = ['ms', 'moz', 'webkit', 'o'];

for(var x = 0; x < vendors.length && !window.requestAnimationFrame; ++x) {
    window.requestAnimationFrame = window[vendors[x]+'RequestAnimationFrame'];
    window.cancelAnimationFrame = window[vendors[x]+'CancelAnimationFrame'] 
                               || window[vendors[x]+'CancelRequestAnimationFrame'];
}

if (!window.requestAnimationFrame)
    window.requestAnimationFrame = function(callback, element) {
        var currTime = new Date().getTime();
        var timeToCall = Math.max(0, 16 - (currTime - lastTime));
        var id = window.setTimeout(function() { callback(currTime + timeToCall); }, 
          timeToCall);
        lastTime = currTime + timeToCall;
        return id;
    };

if (!window.cancelAnimationFrame) {
    window.cancelAnimationFrame = function(id) {
        clearTimeout(id);
    };
}

tamago.start();

},{"./tamago/main.js":12}],2:[function(require,module,exports){
module.exports = {
	memoryBytesPerLine: 16,
	registerBytesPerLine: 8,
	instructionCount: 55
};

},{}],3:[function(require,module,exports){
var addressing = require("./address.js"),
		operations = require("./operations.js"),
		instructions = require("../data/instructions.js"),
		object = require("../../util/object.js");

var r6502 = {},
		ops = {};

r6502.init = function () {
	this.a = 0;
	this.x = 0;
	this.y = 0;
	this.s = 0;
	this.p = 0;
	this.cycles = 0;
	this.pc = 0;

	object.each(instructions, function(op, code) {
		ops[code] = {
			operation: operations[op.instruction],
			address: addressing[op.addressing],
			cycles: op.cycles
		};
	});
}

r6502.reset = function () {
	this.pc = this.read_16(0xFFFC);
}

r6502.nmi = function () {
	this.push(this.pc >> 8);
	this.push(this.pc & 0xFF);
	this.push(this.p);

	this.pc = this.read_16(0xFFFA);
};

r6502.irq = function (brk) {
	this.push(this.pc >> 8);
	this.push(this.pc & 0xFF);
	this.push(this.p | (brk ? 0x10 : 0));

	this.pc = this.read_16(0xFFFE);

	this.i = 1;
};

r6502.step = function () {
	// Fire pending IRQs
	if (!this.i && this.pending_irq()) { this.irq(); }

	var next = ops[this.next()];
	if (next === undefined) { throw new Error("System has crashed (invalid operation)"); }
	next.operation(this, next.address(this));
	this.cycles -= next.cycles;
};

r6502.next = function () {
	var d = this.read(this.pc++);
	this.pc &= 0xFFFF;
	return d;
};

r6502.next_16 = function () {
	var l = this.next(),
		h = this.next();

	return l | (h << 8);
};

r6502.read_16 = function (addr) {
	var l = this.read(addr),
		h = this.read((addr+1) & 0xFFFF);

	return l | (h << 8);
};

r6502.pull = function () {
	this.s = (this.s + 1) & 0xFF;
	return this.read(this.s | 0x100);
};

r6502.push = function (data) {
	this.write(this.s | 0x100, data);
	this.s = (this.s - 1) & 0xFF;
};

Object.defineProperty(r6502, "p", {
	get: function () {
		return ((this.c) ? 0x01: 0) |
			((this.z) ? 0x02: 0) |
			((this.i) ? 0x04: 0) |
			((this.d) ? 0x08: 0) |
			0x20 | // Always set
			((this.v) ? 0x40: 0) |
			((this.n) ? 0x80: 0);
	},
	set: function (v) {
		this.c = v & 0x01;
		this.z = v & 0x02;
		this.i = v & 0x04;
		this.d = v & 0x08;
		this.v = v & 0x40;
		this.n = v & 0x80;
	}
});

module.exports = { r6502: r6502 };

},{"../../util/object.js":15,"../data/instructions.js":10,"./address.js":4,"./operations.js":7}],4:[function(require,module,exports){
/**
 ** This calculates effective address for various modes
 ** NOTE: I always return addr for future auto-inlining
 **/

module.exports = {
	implied: function (cpu) {
		var addr = null;
		return addr;
	}, 

	accumulator: function (cpu) {
		var addr = null;
		return addr;
	}, 

	immediate: function (cpu) {
		var addr = cpu.pc++;
		cpu.pc &= 0xFFFF;
		return addr;
	}, 

	relative: function (cpu) {
		// Sign extend 8-bit value, and add it to the now incremented PC.
		var addr = ((cpu.next() << 24 >> 24) + cpu.pc) & 0xFFFF;

		return addr;
	}, 

	zeropage: function (cpu) {
		var addr = cpu.next();
		return addr;
	},

	zeropageX: function (cpu) {
		var addr = (cpu.next() + cpu.x) & 0xFF;
		return addr;
	}, 

	zeropageY: function (cpu) {
		var addr = (cpu.next() + cpu.y) & 0xFF;
		return addr;
	},

	absolute: function (cpu) {
		var addr = cpu.next_16();
		return addr;
	},

	absoluteX: function (cpu) {
		var addr = (cpu.next_16() + cpu.x) & 0xFFFF;
		return addr;
	}, 

	absoluteY: function (cpu) {
		var addr = (cpu.next_16() + cpu.y) & 0xFFFF;
		return addr;
	},

	indirect: function (cpu) {
		var addr = cpu.next_16(),
			addr_l = cpu.read(addr),
			addr_h = cpu.read(((addr + 1) & 0x00FF) | (addr & 0xFF00));

		addr = addr_l | (addr_h << 8);

		return addr;
	}, 

	indirectX: function (cpu) {
		var addr = (cpu.next() + cpu.x) & 0xFF,
			addr_l = cpu.read(addr),
			addr_h = cpu.read((addr+1) & 0xFF);
		
		addr = addr_l | (addr_h << 8);
		return addr;
	},

	indirectY: function (cpu) {
		var addr = cpu.next(),
			addr_l = cpu.read(addr),
			addr_h = cpu.read((addr+1) & 0xFF);

		addr = ((addr_l | (addr_h << 8)) + cpu.y) & 0xFFFF;
		return addr;
	}
};

},{}],5:[function(require,module,exports){
var instructions = require("../data/instructions.js"),
		ports = require("../data/ports.js"),
		config = require("../config.js");

function disassemble(count, address, cpu) {
	var i = [];

	function z16(addr) {
		var l = cpu.read(addr & 0xFF),
			h = cpu.read((addr+1) & 0xFF);

		return l | (h << 8);
	}

	function g16(addr) {
		var l = cpu.read(addr & 0xFFFF),
			h = cpu.read((addr+1) & 0xFFFF);

		return l | (h << 8);
	}

	function r8() {
		return cpu.read(address++);
	}

	function r16() {
		var l = r8(),
			h = r8();
		
		return l | (h << 8);
	}

	while (count-- > 0) {
		var pos = address,
			op = instructions[r8()],
			output;
		
		// Undefined operation
		if (!op) { break ; }

		switch (op.addressing) {
		case "implied":
			output = { 
				instruction: op.instruction,
				data: null,
				address: null
			};
			break ;
		case "accumulator":
			output = {
				instruction: op.instruction,
				address: null,
				data: "A"
			};
			break ;
		case "immediate":
			output = {
				instruction: op.instruction,
				address: address,
				data: r8()
			};
			break ;
		case "indirect":
			d = r16();
			output = {
				instruction: op.instruction,
				address: g16(d),
				data: d
			};
			break ;
		case "indirectX":
			d = r8();
			output = {
				instruction: op.instruction,
				address: z16(d + cpu.x),
				data: d
			};
			break ;
		case "indirectY":
			d = r8();
			output = {
				instruction: op.instruction,
				address: (z16(d) + cpu.y) & 0xFFFF,
				data: d
			};
			break ;
		case "zeropage":
			d = r8();
			output = {
				instruction: op.instruction,
				address: d,
				data: d
			};
			break ;
		case "zeropageX":
			d = r8();
			output = {
				instruction: op.instruction,
				address: (d + cpu.x) & 0xFF,
				data: d
			};
			break ;
		case "zeropageY":
			d = r8();
			output = {
				instruction: op.instruction,
				address: (d + cpu.y) & 0xFF,
				data: d
			};
			break ;
		case "absolute":
			d = r16();
			output = {
				instruction: op.instruction,
				address: d,
				data: d
			};
			break ;
		case "absoluteX":
			d = r16();
			output = {
				instruction: op.instruction,
				address: (d + cpu.x) & 0xFFFF,
				data: d
			};
			break ;
		case "absoluteY":
			d = r16();
			output = {
				instruction: op.instruction,
				address: (d + cpu.y) & 0xFFFF,
				data: d
			};
			break ;
		case "relative":
			d = r8();
			output = {
				instruction: op.instruction,
				address: (d + address) & 0xFFFF,
				data: (d & 0x80) ? (d - 0x100) : d
			};
			break ;
		default:
			throw new Error("Unhandled addressing mode: " + op.addressing);
		}

		if (pos === cpu.pc) { output.active = true ; }
		output.port = (ports[output.address] || {}).name;
		output.mode = op.addressing;
		output.location = pos;

		output.bytes = []
		while (pos < address) {
			var int = cpu.read(pos++).toString(16).toUpperCase();

			while (int.length < 2) { int = "0" + int; }

			output.bytes.push(int);
		}
		output.bytes = output.bytes.join(" ");

		i.push(output);
	}

	return i;
}

module.exports =  {
	disassemble: disassemble
};

},{"../config.js":2,"../data/instructions.js":10,"../data/ports.js":11}],6:[function(require,module,exports){
var object = require("../../util/object.js");

var DISABLED = 0,
	COMMAND = 1,
	ADDRESS = 2,
	READ = 3,
	WRITE = 4;

function decode(data) {
	return data.match(/../g).map(function(v){
		return parseInt(v,16);
	});
}

function encode(data) {
	return data.map(function (v) {
		return (0x100 | v).toString(16).substr(1);
	}).join("");
}

function eeprom(bit_width) {
	bit_width || (bit_width = 12);
	var byte_size = 1 << bit_width;

	// Initalize eeprom data (4kB by default)
	try {
		this.data = decode(window.localStorage.eeprom_data);
	} catch(e) {
		this.data = object.fill(byte_size, 0);
	}

	this.address_width = Math.ceil(bit_width / 8);
	this.mask = (1 << bit_width) - 1;

	this.update(false);
}

eeprom.prototype.update = function(power, clk, data) {
	// Coerse clk / data lines into integer booleans
	clk = clk ? 1 : 0;
	data = data ? 1 : 0;

	var clk_d = clk - this.last_clk,
		data_d = data - this.last_data;

	this.last_pow = power;
	this.last_clk = clk;
	this.last_data = data;

	// This chip is not receiving power, so it is idle.
	if (!power) {
		this.state = DISABLED;
		this.output = 1; // NACK
		return ;
	}

	// There has been no bus change (idle)
	if (!clk_d && !data_d) { return ; }

	// Give friendly warning about the host behaving poorly
	if (clk_d && data_d) {
		console.error("WARNING: Data and clock lines are transitioning at the same time");
	}

	// Data transition while CLK is high
	if (clk && data_d) {
		if (data_d > 0) { 
			if (this.state === WRITE && window.localStorage) {
				window.localStorage.eeprom_data = encode(this.data);
			}

			// Stop
			this.state = DISABLED;
			this.output = 0;
		} else {
			// Start
			this.state = COMMAND;
			this.output = 0;

			this.bits_tx = 0;
			this.read = 0;
		}
	}

	// We are not processing any data right now
	if (this.state === DISABLED) { return ; }

	if (clk_d > 0) {
		// Rising edge clock (input)
		this.read = ((this.read << 1) & 0xFF) | (data ? 1 : 0);
	} else if (clk_d < 0) {
		// Falling edge (delivery)
		if (this.bits_tx < 8) {
			// Simply update output buffer
			if (this.state === READ) {
				this.output = ((this.data[this.address] << this.bits_tx) & 0x80) ? 1 : 0;
			} else {
				this.output = 1;
			}
		} else if (this.bits_tx === 8) {
			this.output = 0; // ACK

			// We have received a full command / output a value
			switch (this.state) {
			case COMMAND:
				switch(this.read & 0xF1) {
				case 0xA0: // Write
					this.state = ADDRESS;
					this.addressbyte = 0;
					this.address = 0;
					break ;
				case 0xA1: // Read
					this.state = READ;
					break ;
				default:
					this.output = 1; // NACK
					break ;
				}
				break ;
			case ADDRESS:
				// Update address
				this.address = (this.address << 8) | this.read;
				if (++this.addressbyte >= this.address_width) {
					this.state = WRITE;
				}
				break ;
			case WRITE:
				this.data[this.address] = this.read & 0xFF;
				this.address = (this.address + 1) & this.mask;
				break ;
			case READ:
				this.address = (this.address + 1) & this.mask;
				break ;
			}
		}

		// Increment bit clock
		this.bits_tx = (this.bits_tx + 1) % 9;
	}
}

module.exports = {
	eeprom: eeprom
};

},{"../../util/object.js":15}],7:[function(require,module,exports){
/**
 ** These operate on the effective address
 **/
function set_nz(cpu, d) {
    cpu.n = d & 0x80;
    cpu.z = !d;
}

module.exports =  {
    // Other(verified)
    NOP: function (cpu, addr) {},

    // Flags(verified)
    CLC: function (cpu, addr) {
        cpu.c = 0;
    },
    CLI: function (cpu, addr) {
        cpu.i = 0;
    },
    CLV: function (cpu, addr) {
        cpu.v = 0;
    },
    CLD: function (cpu, addr) {
        cpu.d = 0;
    },
    SEC: function (cpu, addr) {
        cpu.c = 1;
    },
    SED: function (cpu, addr) {
        cpu.d = 1;
    },
    SEI: function (cpu, addr) {
        cpu.i = 1;
    },

    // Transfer (verified)
    TXA: function (cpu, addr) {
        set_nz(cpu, cpu.a = cpu.x);
    },
    TYA: function (cpu, addr) {
        set_nz(cpu, cpu.a = cpu.y);
    },
    TXS: function (cpu, addr) {
        cpu.s = cpu.x;
    },
    TAX: function (cpu, addr) {
        set_nz(cpu, cpu.x = cpu.a);
    },
    TAY: function (cpu, addr) {
        set_nz(cpu, cpu.y = cpu.a);
    },
    TSX: function (cpu, addr) {
        set_nz(cpu, cpu.x = cpu.s);
    },
    LDA: function (cpu, addr) {
        set_nz(cpu, cpu.a = cpu.read(addr));
    },
    LDX: function (cpu, addr) {
        set_nz(cpu, cpu.x = cpu.read(addr));
    },
    LDY: function (cpu, addr) {
        set_nz(cpu, cpu.y = cpu.read(addr));
    },
    STA: function (cpu, addr) {
        cpu.write(addr, cpu.a);
    },
    STX: function (cpu, addr) {
        cpu.write(addr, cpu.x);
    },
    STY: function (cpu, addr) {
        cpu.write(addr, cpu.y);
    },

    // Bit Operations (verified)
    BIT: function (cpu, addr) {
        var data = cpu.read(addr);
        cpu.n = data & 0x80;
        cpu.v = data & 0x40;
        cpu.z = !(cpu.a & data);
    },
    ORA: function (cpu, addr) {
        set_nz(cpu, cpu.a |= cpu.read(addr));
    },
    EOR: function (cpu, addr) {
        set_nz(cpu, cpu.a ^= cpu.read(addr));
    },
    AND: function (cpu, addr) {
        set_nz(cpu, cpu.a &= cpu.read(addr));
    },

    // Shifter
    ASL: function (cpu, addr) {
        var data = cpu.read(addr),
            out = (data << 1) & 0xFF;

        cpu.c = data & 0x80;

        set_nz(cpu, out);
        cpu.write(addr, out);
    },
    LSR: function (cpu, addr) {
        var data = cpu.read(addr),
            out = data >> 1;

        cpu.c = data & 0x01;

        set_nz(cpu, out);
        cpu.write(addr, out);
    },

    ROL: function (cpu, addr) {
        var data = cpu.read(addr),
            out = ((data << 1) & 0xFF) | (cpu.c ? 1 : 0);

        cpu.c = data & 0x80;

        set_nz(cpu, out);
        cpu.write(addr, out);
    },

    ROR: function (cpu, addr) {
        var data = cpu.read(addr),
            out = (data >> 1) | (cpu.c ? 0x80 : 0);

        cpu.c = data & 0x01;

        set_nz(cpu, out);
        cpu.write(addr, out);
    },

    // Arithmatic
    DEC: function (cpu, addr) {
        var data = (cpu.read(addr) - 1) & 0xFF;
        set_nz(cpu, data);
        cpu.write(addr, data)
    },
    DEX: function (cpu, addr) {
        set_nz(cpu, cpu.x = (cpu.x - 1) & 0xFF);
    },
    DEY: function (cpu, addr) {
        set_nz(cpu, cpu.y = (cpu.y - 1) & 0xFF);
    },
    INC: function (cpu, addr) {
        var data = (cpu.read(addr) + 1) & 0xFF;
        set_nz(cpu, data);
        cpu.write(addr, data)
    },
    INX: function (cpu, addr) {
        set_nz(cpu, cpu.x = (cpu.x + 1) & 0xFF);
    },
    INY: function (cpu, addr) {
        set_nz(cpu, cpu.y = (cpu.y + 1) & 0xFF);
    },
    ADC: function (cpu, addr) {
        var data = cpu.read(addr),
            o = cpu.a + data + (cpu.c ? 1 : 0);

        cpu.v = ~(cpu.a ^ data) & (cpu.a ^ o) & 0x80;
        set_nz(cpu, o & 0xFF);

        if (cpu.d) {
            var al = (cpu.a & 0x0F) + (data & 0x0F) + (cpu.c ? 1 : 0),
                ah = (cpu.a & 0xF0) + (data & 0xF0) + ((al >= 0x10) ? 0x10 : 0);
            
            // Decimal mode fixup
            if (al > 0x09) { al += 0x06; }
            if (ah > 0x90) { ah += 0x60; }

            // We fixed up the decimal, recombine
            o = (al & 0x0F) + ah; 
        }

        cpu.c = o & ~0xFF;

        cpu.a = o & 0xFF;
    },
    SBC: function (cpu, addr) {
        var data = cpu.read(addr),
            o = cpu.a - data - (cpu.c ? 0 : 1);

        // All flags are like binary mode
        cpu.v = (cpu.a ^ data) & (cpu.a ^ o) & 0x80;
        set_nz(cpu, o & 0xFF);
        cpu.c = !(o & ~0xFF);

        if (cpu.d) {
            var al = (cpu.a & 0x0F) - (data & 0x0F) - (cpu.c ? 0 : 1),
                ah = (cpu.a & 0xF0) - (data & 0xF0) - ((al < 0) ? 0x10 : 0);

            // Calculate fix up decimal mode
            if (al < 0x00) { al -= 0x06; }
            if (ah < 0x00) { ah -= 0x60; }

            o = (al & 0x0F) + ah;
        }

        cpu.a = o & 0xFF;
    },
    CMP: function (cpu, addr) {
        var data = cpu.read(addr);

        cpu.c = cpu.a >= data;
        set_nz(cpu, (cpu.a - data) & 0xFF);
    },
    CPX: function (cpu, addr) {
        var data = cpu.read(addr);

        cpu.c = cpu.x >= data;
        set_nz(cpu, (cpu.x - data) & 0xFF);
    },
    CPY: function (cpu, addr) {
        var data = cpu.read(addr);

        cpu.c = cpu.y >= data;
        set_nz(cpu, (cpu.y - data) & 0xFF);
    },

    // Stack Operations
    PHP: function (cpu, addr) {
        cpu.push(cpu.p | 0x10);
    },
    PHA: function (cpu, addr) {
        cpu.push(cpu.a);
    },
    PLA: function (cpu, addr) {
        set_nz(cpu, cpu.a = cpu.pull());
    },
    PLP: function (cpu, addr) {
        cpu.p = cpu.pull();
    },

    // Interrupts / Branch (verified)
    JMP: function (cpu, addr) {
        cpu.pc = addr;
    },
    JSR: function (cpu, addr) {
        cpu.pc = (cpu.pc - 1) & 0xFFFF;
        cpu.push(cpu.pc >> 8);
        cpu.push(cpu.pc & 0xFF);
        cpu.pc = addr;
    },
    RTI: function (cpu, addr) {
        cpu.p = cpu.pull();
        cpu.pc = cpu.pull();
        cpu.pc |= cpu.pull() << 8;
    },
    RTS: function (cpu, addr) {
        cpu.pc = cpu.pull();
        cpu.pc |= cpu.pull() << 8;
        cpu.pc = (cpu.pc + 1) & 0xFFFF;
    },
    BRK: function (cpu, addr) {
        // This should probably actually find out which IRQ to service
        cpu.pc = (cpu.pc + 1) & 0xFFFF;
        cpu.irq(true);
    },

    BNE: function (cpu, addr) {
        if (!cpu.z) cpu.pc = addr;
    },
    BEQ: function (cpu, addr) {
        if (cpu.z) cpu.pc = addr;
    },
    BPL: function (cpu, addr) {
        if (!cpu.n) cpu.pc = addr;
    },
    BMI: function (cpu, addr) {
        if (cpu.n) cpu.pc = addr;
    },
    BCC: function (cpu, addr) {
        if (!cpu.c) cpu.pc = addr;
    },
    BCS: function (cpu, addr) {
        if (cpu.c) cpu.pc = addr;
    },
    BVC: function (cpu, addr) {
        if (!cpu.v) cpu.pc = addr;
    },
    BVS: function (cpu, addr) {
        if (cpu.v) cpu.pc = addr;
    }
};

},{}],8:[function(require,module,exports){
var ports = require("../data/ports.js"),
		object = require("../../util/object.js");

// ==== Bank Switch ====
function write_bank(reg, value) {
	this._cpureg[reg] = value;
	this.set_rom_page(value);
}

// ==== IRQ Logic ===
function write_int_flag(reg, value) {
	this._cpureg[reg] &= ~value;
}

// ==== PortA ====
function write_porta_dir_data(reg, value) {
	this._cpureg[reg] = value;
	// no writes yet.
}

function read_porta_data(reg, value) {
	var mask = this._cpureg[0x11],
		value = this._cpureg[0x12],
		spi_power = mask & value & 0x10,
		input = this.keys | 
				((spi_power ? 0 : this.inserted_figure) << 5);

	return (mask & value) | (~mask & input);
}

// ==== PortB ====
function write_portb_dir_data(reg, value) {
	this._cpureg[reg] = value;

	var mask = this._cpureg[0x15],
		d = ~mask | this._cpureg[0x16];	// Values are pulled up

	this._eeprom.update(d&4, d&2, d&1);
}

function read_portb_data(reg, value) {
	var mask = this._cpureg[0x15],
		input = (this._eeprom.output ? 1 : 0);
	
	return (mask & this._cpureg[0x16]) | (~mask & input);
}

// --- REGISTER LAYOUT ---
function pad(s, l) {
	return "00000000".substr(0, l).substr(s.length) + s;
}

// Default register actions
function undef_read(reg) {
	console.log(
		pad(this._cpureg[0].toString(16), 2),
		this.pc.toString(16),
		"Unhandled register read  (" + (0x3000+reg).toString(16) + ")", 
		"             ", 
		(ports[reg|0x3000] || {}).name || "---");

	if (reg == 0xB7) return 0xFF;

	return this._cpureg[reg];
}

function undef_write(reg, data) {
	console.log(
		pad(this._cpureg[0].toString(16), 2),					
		this.pc.toString(16),
		"Unhandled register write (" + (0x3000+reg).toString(16) + ")", 
		pad(data.toString(16),2), 
		"-", 
		pad(data.toString(2), 8), 
		(ports[reg|0x3000] || {}).name || "---");
	this._cpureg[reg] = data;
}

var register_layout = {
	0x00: { write: write_bank },
	0x01: {}, // SILENCE
	0x04: {}, // SILENCE
	0x31: {}, // SILENCE

	// --- DATA Ports
	0x10: {}, // SILENCE CONFIG
	0x11: { write: write_porta_dir_data },
	0x12: { write: write_porta_dir_data, read: read_porta_data },
	0x14: {}, // SILENCE CONFIG
	0x15: { write: write_portb_dir_data },
	0x16: { write: write_portb_dir_data, read: read_portb_data },

	// --- IRQ Block
	0x70: {}, // IRQ Enables are normal 
	0x71: {}, // IRQ Enables are normal 
	0x73: { write: write_int_flag },
	0x74: { write: write_int_flag },
	0x76: {}, // NMI Enables are normal
}, undef_register = {
	read: undef_read, 
	write: undef_write 
};

module.exports = {
	map_registers: function () {
		// Start mapping out registers
		for (var i = 0; i < 0x100; i++) {
			// This is normally considered dangerous, but I need the closure
			~function () {
				var layout = register_layout[i] || undef_register,
					read   = layout.read || function (reg) { return this._cpureg[reg]; },
					write  = layout.write || function (reg, data) { this._cpureg[reg] = data; };

				// Map registers to their mirrors as well
				for (var a = 0x3000; a < 0x4000; a += 0x100) {
					this._readbank[a+i] = read;
					this._writebank[a+i] = write;
				}
			}.call(this);
		}
	}
};

},{"../../util/object.js":15,"../data/ports.js":11}],9:[function(require,module,exports){

var r6502 = require("./6502.js"),
		eeprom = require("./eeprom.js"),
		registers = require("./registers.js"),
		object = require("../../util/object.js");

var ACCESS_READ		= 0x01,
		ACCESS_WRITE	= 0x02;

function system() {
	var that = this;

	this._readbank = new Array(0x10000);
	this._writebank = new Array(0x10000);

	this._cpuacc = new Uint8Array(0x10000);		// Access flags

	this._cpureg = new Uint8Array(0x100);		// Control registers
	this._dram   = new Uint8Array(0x200);		// Display memory
	this._wram	 = new Uint8Array(0x600);		// System memory
	this._eeprom = new eeprom.eeprom(12);		// new 32kb eeprom
	this._irqs = new Uint16Array(0x10000);

	this.keys	 = 0xF;

	// Convert a 16bit mask into a priority encoded IRQ table
	var irqs = new Uint16Array(this.bios, 0x3FC0, 16);
	for (var i = 0; i < this._irqs.length; i++) {
		this._irqs[i] = irqs[15 - Math.floor(i ? (Math.log(i) / Math.log(2)) : 0)];
	}

	// Configure and reset
	this.init();
	this.reset();

	this.previous_clock = 0;
	this.inserted_figure = 0;
	
	this._tbh_timer = 0; 	// HACK
}

system.prototype = Object.create(r6502.r6502);	
object.extend(system.prototype, registers);

system.prototype.PALETTE = [0xffdddddd, 0xff9e9e9e, 0xff606060, 0xff222222];

system.prototype.CLOCK_RATE = 4000000; // 4MHz
system.prototype.MAX_ADVANCE = 1;
system.prototype.LCD_ORDER = [
	0x0C0, 0x0CC, 0x0D8, 0x0E4, 
	0x0F0, 0x0FC, 0x108, 0x114, 
	0x120, 0x12C, 0x138, 0x144, 
	0x150, 0x15C, 0x168, 0x174, 
	0x0B4, 0x0A8, 0x09C, 0x090, 
	0x084, 0x078, 0x06C, 0x060, 
	0x054, 0x048, 0x03C, 0x030, 
	0x024, 0x018, 0x00C];

system.prototype.step_realtime = function () {
	var t = +new Date() / 1000,
		d = Math.min(this.MAX_ADVANCE, t - this.previous_clock) || 0;

	this.previous_clock = t;
	this.cycles += this.CLOCK_RATE * d;

	var ticks = Math.floor(this.cycles);

	this._tbh_timer += ticks;

	// Animation rate counter (HACk)
	var TBH_RATE = this.CLOCK_RATE / 2;
	while (this._tbh_timer >= TBH_RATE) {
		this.fire_irq(13);
		this._tbh_timer -= TBH_RATE;
	}

	// Fire every frame (rate unknown, HACK)
	this.fire_irq(10);
	this.fire_nmi(6);

	while(this.cycles > 0) { this.step(); }
}

system.prototype.fire_nmi = function (i) {
	// NMI was not enabled
	if (~this._cpureg[0x76] & (0x80 >> i)) { return ; }

	this.nmi();
}

system.prototype.pending_irq = function () {
	return (this._cpureg[0x73] << 8) | this._cpureg[0x74];
}

system.prototype.fire_irq = function (i) {
	// Map the pending interrupt
	var mask = (this._cpureg[0x70] << 8) | this._cpureg[0x71];

	// This IRQ is disabled
	if ((0x8000 >> i) & ~mask) { return ; }

	// Set pending IRQ to fire
	this._cpureg[0x73 + (i >> 3)] |= 0x80 >> (i & 7);
}

system.prototype.insert_figure = function (data) {
	this.spi_rom = new Uint8Array(data);
};

system.prototype.init = function () {
	var i, data;

	r6502.r6502.init.call(this);

	// Work ram
	for (i = 0x0000; i < 0x1000; i+=0x0100) {
		data = new Uint8Array(this._wram.buffer, i % this._wram.length, 0x100);
		this.ram(i>>8, data);
	}

	// Display memory
	for (i = 0x1000; i < 0x3000; i+=0x0100) {
		data = new Uint8Array(this._dram.buffer, i % this._dram.length, 0x100);
		this.ram(i>>8, data);
	}

	// CPU registers
	this.map_registers();

	// Static rom
	for (var i = 0; i < 0x40; i ++) {
		this.rom(i + 0xC0, new Uint8Array(this.bios, i << 8, 0x100));
	}

	this._readbank[0xFFFE] = function () { return this._irqs[this.pending_irq()] & 0xFF; }
	this._readbank[0xFFFF] = function () { return this._irqs[this.pending_irq()] >> 8; }

	// Bankable rom
	this.set_rom_page(0);	// Clear current rom page
}

system.prototype.read = function(addr, noack) {
	// A addressing
	if (addr === null) {
		return this.a;
	}

	if(!noack) this._cpuacc[addr] |= ACCESS_READ;

	return this._readbank[addr].call(this, addr & 0xFF);
};

system.prototype.write = function (addr, data) {
	if (addr === null) {
		this.a = data; 
		return ;
	}

	this._cpuacc[addr] |= ACCESS_WRITE;

	return this._writebank[addr].call(this, addr & 0xFF, data);
};

// Start helper functions for mapping to memory
system.prototype.set_rom_page = function (bank) {
	var offset = 0x8000 * (bank % 20);

	for (var i = 0; i < 0x80; i ++) {
		this.rom(i + 0x40, new Uint8Array(this.bios, offset + (i << 8), 0x100));
	}
}
system.prototype.ram = function (bank, data) {
	function read(reg) {
		return data[reg];
	}

	function write(reg, value) {
		data[reg] = value;
	}

	bank <<= 8;
	for (var i = 0; i < 0x100; i++) {
		this._readbank[bank+i] = read;
		this._writebank[bank+i] = write;
	}
};

system.prototype.rom = function (bank, data) {
	function nullwrite() {}
	function read(addr) {
		return data[addr];
	}

	bank <<= 8;
	for (var i = 0; i < 0x100; i++) {
		this._readbank[bank+i] = read;
		this._writebank[bank+i] = nullwrite;
	}
};

module.exports =  {
	ACCESS_WRITE: ACCESS_WRITE,
	ACCESS_READ: ACCESS_READ,
	system: system
};


},{"../../util/object.js":15,"./6502.js":3,"./eeprom.js":6,"./registers.js":8}],10:[function(require,module,exports){
/** 
  *  Instruction defintion table
  */

module.exports = {
    0: {
        "addressing": "implied",
        "instruction": "BRK",
        "cycles": "7"
    },
    1: {
        "addressing": "indirectX",
        "instruction": "ORA",
        "cycles": "6"
    },
    5: {
        "addressing": "zeropage",
        "instruction": "ORA",
        "cycles": "3"
    },
    6: {
        "addressing": "zeropage",
        "instruction": "ASL",
        "cycles": "5"
    },
    8: {
        "addressing": "implied",
        "instruction": "PHP",
        "cycles": "3"
    },
    9: {
        "addressing": "immediate",
        "instruction": "ORA",
        "cycles": "2"
    },
    10: {
        "addressing": "accumulator",
        "instruction": "ASL",
        "cycles": "2"
    },
    13: {
        "addressing": "absolute",
        "instruction": "ORA",
        "cycles": "4"
    },
    14: {
        "addressing": "absolute",
        "instruction": "ASL",
        "cycles": "6"
    },
    16: {
        "addressing": "relative",
        "instruction": "BPL",
        "cycles": "2"
    },
    17: {
        "addressing": "indirectY",
        "instruction": "ORA",
        "cycles": "5"
    },
    21: {
        "addressing": "zeropageX",
        "instruction": "ORA",
        "cycles": "4"
    },
    22: {
        "addressing": "zeropageX",
        "instruction": "ASL",
        "cycles": "6"
    },
    24: {
        "addressing": "implied",
        "instruction": "CLC",
        "cycles": "2"
    },
    25: {
        "addressing": "absoluteY",
        "instruction": "ORA",
        "cycles": "4"
    },
    29: {
        "addressing": "absoluteX",
        "instruction": "ORA",
        "cycles": "4"
    },
    30: {
        "addressing": "absoluteX",
        "instruction": "ASL",
        "cycles": "7"
    },
    32: {
        "addressing": "absolute",
        "instruction": "JSR",
        "cycles": "6"
    },
    33: {
        "addressing": "indirectX",
        "instruction": "AND",
        "cycles": "6"
    },
    36: {
        "addressing": "zeropage",
        "instruction": "BIT",
        "cycles": "3"
    },
    37: {
        "addressing": "zeropage",
        "instruction": "AND",
        "cycles": "3"
    },
    38: {
        "addressing": "zeropage",
        "instruction": "ROL",
        "cycles": "5"
    },
    40: {
        "addressing": "implied",
        "instruction": "PLP",
        "cycles": "4"
    },
    41: {
        "addressing": "immediate",
        "instruction": "AND",
        "cycles": "2"
    },
    42: {
        "addressing": "accumulator",
        "instruction": "ROL",
        "cycles": "2"
    },
    44: {
        "addressing": "absolute",
        "instruction": "BIT",
        "cycles": "4"
    },
    45: {
        "addressing": "absolute",
        "instruction": "AND",
        "cycles": "4"
    },
    46: {
        "addressing": "absolute",
        "instruction": "ROL",
        "cycles": "6"
    },
    48: {
        "addressing": "relative",
        "instruction": "BMI",
        "cycles": "2"
    },
    49: {
        "addressing": "indirectY",
        "instruction": "AND",
        "cycles": "5"
    },
    53: {
        "addressing": "zeropageX",
        "instruction": "AND",
        "cycles": "4"
    },
    54: {
        "addressing": "zeropageX",
        "instruction": "ROL",
        "cycles": "6"
    },
    56: {
        "addressing": "implied",
        "instruction": "SEC",
        "cycles": "2"
    },
    57: {
        "addressing": "absoluteY",
        "instruction": "AND",
        "cycles": "4"
    },
    61: {
        "addressing": "absoluteX",
        "instruction": "AND",
        "cycles": "4"
    },
    62: {
        "addressing": "absoluteX",
        "instruction": "ROL",
        "cycles": "7"
    },
    64: {
        "addressing": "implied",
        "instruction": "RTI",
        "cycles": "6"
    },
    65: {
        "addressing": "indirectX",
        "instruction": "EOR",
        "cycles": "6"
    },
    69: {
        "addressing": "zeropage",
        "instruction": "EOR",
        "cycles": "3"
    },
    70: {
        "addressing": "zeropage",
        "instruction": "LSR",
        "cycles": "5"
    },
    72: {
        "addressing": "implied",
        "instruction": "PHA",
        "cycles": "3"
    },
    73: {
        "addressing": "immediate",
        "instruction": "EOR",
        "cycles": "2"
    },
    74: {
        "addressing": "accumulator",
        "instruction": "LSR",
        "cycles": "2"
    },
    76: {
        "addressing": "absolute",
        "instruction": "JMP",
        "cycles": "3"
    },
    77: {
        "addressing": "absolute",
        "instruction": "EOR",
        "cycles": "4"
    },
    78: {
        "addressing": "absolute",
        "instruction": "LSR",
        "cycles": "6"
    },
    80: {
        "addressing": "relative",
        "instruction": "BVC",
        "cycles": "2"
    },
    81: {
        "addressing": "indirectY",
        "instruction": "EOR",
        "cycles": "5"
    },
    85: {
        "addressing": "zeropageX",
        "instruction": "EOR",
        "cycles": "4"
    },
    86: {
        "addressing": "zeropageX",
        "instruction": "LSR",
        "cycles": "6"
    },
    88: {
        "addressing": "implied",
        "instruction": "CLI",
        "cycles": "2"
    },
    89: {
        "addressing": "absoluteY",
        "instruction": "EOR",
        "cycles": "4"
    },
    93: {
        "addressing": "absoluteX",
        "instruction": "EOR",
        "cycles": "4"
    },
    94: {
        "addressing": "absoluteX",
        "instruction": "LSR",
        "cycles": "7"
    },
    96: {
        "addressing": "implied",
        "instruction": "RTS",
        "cycles": "6"
    },
    97: {
        "addressing": "indirectX",
        "instruction": "ADC",
        "cycles": "6"
    },
    101: {
        "addressing": "zeropage",
        "instruction": "ADC",
        "cycles": "3"
    },
    102: {
        "addressing": "zeropage",
        "instruction": "ROR",
        "cycles": "5"
    },
    104: {
        "addressing": "implied",
        "instruction": "PLA",
        "cycles": "4"
    },
    105: {
        "addressing": "immediate",
        "instruction": "ADC",
        "cycles": "2"
    },
    106: {
        "addressing": "accumulator",
        "instruction": "ROR",
        "cycles": "2"
    },
    108: {
        "addressing": "indirect",
        "instruction": "JMP",
        "cycles": "5"
    },
    109: {
        "addressing": "absolute",
        "instruction": "ADC",
        "cycles": "4"
    },
    110: {
        "addressing": "absolute",
        "instruction": "ROR",
        "cycles": "6"
    },
    112: {
        "addressing": "relative",
        "instruction": "BVS",
        "cycles": "2"
    },
    113: {
        "addressing": "indirectY",
        "instruction": "ADC",
        "cycles": "5"
    },
    117: {
        "addressing": "zeropageX",
        "instruction": "ADC",
        "cycles": "4"
    },
    118: {
        "addressing": "zeropageX",
        "instruction": "ROR",
        "cycles": "6"
    },
    120: {
        "addressing": "implied",
        "instruction": "SEI",
        "cycles": "2"
    },
    121: {
        "addressing": "absoluteY",
        "instruction": "ADC",
        "cycles": "4"
    },
    125: {
        "addressing": "absoluteX",
        "instruction": "ADC",
        "cycles": "4"
    },
    126: {
        "addressing": "absoluteX",
        "instruction": "ROR",
        "cycles": "7"
    },
    129: {
        "addressing": "indirectX",
        "instruction": "STA",
        "cycles": "6"
    },
    132: {
        "addressing": "zeropage",
        "instruction": "STY",
        "cycles": "3"
    },
    133: {
        "addressing": "zeropage",
        "instruction": "STA",
        "cycles": "3"
    },
    134: {
        "addressing": "zeropage",
        "instruction": "STX",
        "cycles": "3"
    },
    136: {
        "addressing": "implied",
        "instruction": "DEY",
        "cycles": "2"
    },
    138: {
        "addressing": "implied",
        "instruction": "TXA",
        "cycles": "2"
    },
    140: {
        "addressing": "absolute",
        "instruction": "STY",
        "cycles": "4"
    },
    141: {
        "addressing": "absolute",
        "instruction": "STA",
        "cycles": "4"
    },
    142: {
        "addressing": "absolute",
        "instruction": "STX",
        "cycles": "4"
    },
    144: {
        "addressing": "relative",
        "instruction": "BCC",
        "cycles": "2"
    },
    145: {
        "addressing": "indirectY",
        "instruction": "STA",
        "cycles": "6"
    },
    148: {
        "addressing": "zeropageX",
        "instruction": "STY",
        "cycles": "4"
    },
    149: {
        "addressing": "zeropageX",
        "instruction": "STA",
        "cycles": "4"
    },
    150: {
        "addressing": "zeropageY",
        "instruction": "STX",
        "cycles": "4"
    },
    152: {
        "addressing": "implied",
        "instruction": "TYA",
        "cycles": "2"
    },
    153: {
        "addressing": "absoluteY",
        "instruction": "STA",
        "cycles": "5"
    },
    154: {
        "addressing": "implied",
        "instruction": "TXS",
        "cycles": "2"
    },
    157: {
        "addressing": "absoluteX",
        "instruction": "STA",
        "cycles": "5"
    },
    160: {
        "addressing": "immediate",
        "instruction": "LDY",
        "cycles": "2"
    },
    161: {
        "addressing": "indirectX",
        "instruction": "LDA",
        "cycles": "6"
    },
    162: {
        "addressing": "immediate",
        "instruction": "LDX",
        "cycles": "2"
    },
    164: {
        "addressing": "zeropage",
        "instruction": "LDY",
        "cycles": "3"
    },
    165: {
        "addressing": "zeropage",
        "instruction": "LDA",
        "cycles": "3"
    },
    166: {
        "addressing": "zeropage",
        "instruction": "LDX",
        "cycles": "3"
    },
    168: {
        "addressing": "implied",
        "instruction": "TAY",
        "cycles": "2"
    },
    169: {
        "addressing": "immediate",
        "instruction": "LDA",
        "cycles": "2"
    },
    170: {
        "addressing": "implied",
        "instruction": "TAX",
        "cycles": "2"
    },
    172: {
        "addressing": "absolute",
        "instruction": "LDY",
        "cycles": "4"
    },
    173: {
        "addressing": "absolute",
        "instruction": "LDA",
        "cycles": "4"
    },
    174: {
        "addressing": "absolute",
        "instruction": "LDX",
        "cycles": "4"
    },
    176: {
        "addressing": "relative",
        "instruction": "BCS",
        "cycles": "2"
    },
    177: {
        "addressing": "indirectY",
        "instruction": "LDA",
        "cycles": "5"
    },
    180: {
        "addressing": "zeropageX",
        "instruction": "LDY",
        "cycles": "4"
    },
    181: {
        "addressing": "zeropageX",
        "instruction": "LDA",
        "cycles": "4"
    },
    182: {
        "addressing": "zeropageY",
        "instruction": "LDX",
        "cycles": "4"
    },
    184: {
        "addressing": "implied",
        "instruction": "CLV",
        "cycles": "2"
    },
    185: {
        "addressing": "absoluteY",
        "instruction": "LDA",
        "cycles": "4"
    },
    186: {
        "addressing": "implied",
        "instruction": "TSX",
        "cycles": "2"
    },
    188: {
        "addressing": "absoluteX",
        "instruction": "LDY",
        "cycles": "4"
    },
    189: {
        "addressing": "absoluteX",
        "instruction": "LDA",
        "cycles": "4"
    },
    190: {
        "addressing": "absoluteY",
        "instruction": "LDX",
        "cycles": "4"
    },
    192: {
        "addressing": "immediate",
        "instruction": "CPY",
        "cycles": "2"
    },
    193: {
        "addressing": "indirectX",
        "instruction": "CMP",
        "cycles": "6"
    },
    196: {
        "addressing": "zeropage",
        "instruction": "CPY",
        "cycles": "3"
    },
    197: {
        "addressing": "zeropage",
        "instruction": "CMP",
        "cycles": "3"
    },
    198: {
        "addressing": "zeropage",
        "instruction": "DEC",
        "cycles": "5"
    },
    200: {
        "addressing": "implied",
        "instruction": "INY",
        "cycles": "2"
    },
    201: {
        "addressing": "immediate",
        "instruction": "CMP",
        "cycles": "2"
    },
    202: {
        "addressing": "implied",
        "instruction": "DEX",
        "cycles": "2"
    },
    204: {
        "addressing": "absolute",
        "instruction": "CPY",
        "cycles": "4"
    },
    205: {
        "addressing": "absolute",
        "instruction": "CMP",
        "cycles": "4"
    },
    206: {
        "addressing": "absolute",
        "instruction": "DEC",
        "cycles": "3"
    },
    208: {
        "addressing": "relative",
        "instruction": "BNE",
        "cycles": "2"
    },
    209: {
        "addressing": "indirectY",
        "instruction": "CMP",
        "cycles": "5"
    },
    213: {
        "addressing": "zeropageX",
        "instruction": "CMP",
        "cycles": "4"
    },
    214: {
        "addressing": "zeropageX",
        "instruction": "DEC",
        "cycles": "6"
    },
    216: {
        "addressing": "implied",
        "instruction": "CLD",
        "cycles": "2"
    },
    217: {
        "addressing": "absoluteY",
        "instruction": "CMP",
        "cycles": "4"
    },
    221: {
        "addressing": "absoluteX",
        "instruction": "CMP",
        "cycles": "4"
    },
    222: {
        "addressing": "absoluteX",
        "instruction": "DEC",
        "cycles": "7"
    },
    224: {
        "addressing": "immediate",
        "instruction": "CPX",
        "cycles": "2"
    },
    225: {
        "addressing": "indirectX",
        "instruction": "SBC",
        "cycles": "6"
    },
    228: {
        "addressing": "zeropage",
        "instruction": "CPX",
        "cycles": "3"
    },
    229: {
        "addressing": "zeropage",
        "instruction": "SBC",
        "cycles": "3"
    },
    230: {
        "addressing": "zeropage",
        "instruction": "INC",
        "cycles": "5"
    },
    232: {
        "addressing": "implied",
        "instruction": "INX",
        "cycles": "2"
    },
    233: {
        "addressing": "immediate",
        "instruction": "SBC",
        "cycles": "2"
    },
    234: {
        "addressing": "implied",
        "instruction": "NOP",
        "cycles": "2"
    },
    236: {
        "addressing": "absolute",
        "instruction": "CPX",
        "cycles": "4"
    },
    237: {
        "addressing": "absolute",
        "instruction": "SBC",
        "cycles": "4"
    },
    238: {
        "addressing": "absolute",
        "instruction": "INC",
        "cycles": "6"
    },
    240: {
        "addressing": "relative",
        "instruction": "BEQ",
        "cycles": "2"
    },
    241: {
        "addressing": "indirectY",
        "instruction": "SBC",
        "cycles": "5"
    },
    245: {
        "addressing": "zeropageX",
        "instruction": "SBC",
        "cycles": "4"
    },
    246: {
        "addressing": "zeropageX",
        "instruction": "INC",
        "cycles": "6"
    },
    248: {
        "addressing": "implied",
        "instruction": "SED",
        "cycles": "2"
    },
    249: {
        "addressing": "absoluteY",
        "instruction": "SBC",
        "cycles": "4"
    },
    253: {
        "addressing": "absoluteX",
        "instruction": "SBC",
        "cycles": "4"
    },
    254: {
        "addressing": "absoluteX",
        "instruction": "INC",
        "cycles": "7"
    }
};

},{}],11:[function(require,module,exports){
/** 
  *  Port name definition table
  */

module.exports = {
	// System block
	0x3000: { 
		name: "P_CPU_Bank_Ctrl", 
		fields: [
			{ name:"bank", start: 0, length: 8 }
		]
	},
	0x3001: { 
		name: "P_CPU_Clk_Ctrl", 
	},
	0x3002: { 
		name: "P_32768_EN", 
	},
	0x3004: { 
		name: "Watchdog Timer?",
	},
	0x3006: {
		name: "Sound Related?",
	},
	0x3007: { 
		name: "P_Wakeup_Ctrl",
	},

	// Port block
	0x3010: { 
		name: "P_PortA_Config",
	},
	0x3011: { 
		name: "P_PortA_Dir",
		fields: [
			{ name:"IR Transmit", start: 7, length: 1 },
			{ name:"Cart CD2", start: 6, length: 1 },
			{ name:"Cart CD1", start: 5, length: 1 },
			{ name:"Cart Power", start: 4, length: 1 },
			{ name:"Reset", start: 3, length: 1 },
			{ name:"C", start: 2, length: 1 },
			{ name:"B", start: 1, length: 1 },
			{ name:"A", start: 0, length: 1 }
		]
	},
	0x3012: { 
		name: "P_PortA_Data",
		fields: [
			{ name:"IR Rx", start: 7, length: 1 },
			{ name:"Cart CD2", start: 6, length: 1 },
			{ name:"Cart CD1", start: 5, length: 1 },
			{ name:"Cart Power", start: 4, length: 1 },
			{ name:"Reset", start: 3, length: 1 },
			{ name:"C", start: 2, length: 1 },
			{ name:"B", start: 1, length: 1 },
			{ name:"A", start: 0, length: 1 }
		]
	},
	0x3013: { 
		name: "P_PortA_Strobe",
	},
	0x3014: { 
		name: "P_PortB_Config",
	},
	0x3015: { 
		name: "P_PortB_Dir",
		fields: [
			{ name:"SPI Rx", start: 7, length: 1 },
			{ name:"SPI Tx", start: 6, length: 1 },
			{ name:"SPI Clk", start: 5, length: 1 },
			{ name:"SPI CSN", start: 4, length: 1 },
			{ name:"IR Tx", start: 3, length: 1 },
			{ name:"I2C Power", start: 2, length: 1 },
			{ name:"I2C Clk", start: 1, length: 1 },
			{ name:"I2C Data", start: 0, length: 1 }
		]
	},
	0x3016: { 
		name: "P_PortB_Data",
		fields: [
			{ name:"SPI Rx", start: 7, length: 1 },
			{ name:"SPI Tx", start: 6, length: 1 },
			{ name:"SPI Clk", start: 5, length: 1 },
			{ name:"SPI CSN", start: 4, length: 1 },
			{ name:"IR Tx", start: 3, length: 1 },
			{ name:"I2C Power", start: 2, length: 1 },
			{ name:"I2C Clk", start: 1, length: 1 },
			{ name:"I2C Data", start: 0, length: 1 }
		]
	},

	// Timer control block
	0x3030: { 
		name: "Timer Control 0?",
	},
	0x3031: { 
		name: "Timer Control 1?",
	},
	0x3032: { 
		name: "Timer Output 0 low?",
	},
	0x3033: { 
		name: "Timer Output 0 high?",
	},
	0x3034: { 
		name: "Timer Output 1 low?",
	},
	0x3035: { 
		name: "Timer Output 1 high?",
	},
	0x303E: { 
		name: "P_Seg0_Scan_Ctrl",
	},
	0x303F: { 
		name: "P_Seg8_Scan_Ctrl",
	},

	// LCD Control block
	0x3040: { 
		name: "P_LCD_Setup1",
	},
	0x3041: { 
		name: "P_LCD_Setup2",
	},
	0x3042: { 
		name: "P_LCD_Clock1",
	},
	0x3043: { 
		name: "P_LCD_Clock2",
	},
	0x3044: { 
		name: "P_LCD_SEG_Num",
	},
	0x3045: { 
		name: "P_LCD_COM_Num",
	},
	0x3047: { 
		name: "P_LCD_Buffer_Ctrl",
	},
	0x3048: {
		name: "P_Contrast_Ctrl",
	},
	0x3049: { 
		name: "P_LCD_Pump_Ctrl",
	},

	// Sound control block
	0x3060: {
		name: "Sound Related?",
	},
	0x3062: {
		name: "Sound Related?",
	},
	0x3064: {
		name: "Sound Related?",
	},
	0x3065: {
		name: "Sound Related?",
	},

	// Interrupt control block
	0x3070: { 
		name: "P_INT_Ctrl0",
		fields: [
			{ name:"Enable TM0", start: 7, length: 1 },
			{ name:"Enable IRQ1", start: 6, length: 1 },
			{ name:"Enable IRQ2", start: 5, length: 1 },
			{ name:"Enable 2048", start: 4, length: 1 },
			{ name:"Enable 8192", start: 3, length: 1 },
			{ name:"Enable SPU", start: 2, length: 1 },
			{ name:"Enable SPI", start: 1, length: 1 },
			{ name:"Enable FP", start: 0, length: 1 }
		]
	},
	0x3071: { 
		name: "P_INT_Ctrl1",
		fields: [
			{ name:"Enable IRQ8", start: 7, length: 1 },
			{ name:"Enable IRQ9", start: 6, length: 1 },
			{ name:"Enable TM1", start: 5, length: 1 },
			{ name:"Enable IRQ11", start: 4, length: 1 },
			{ name:"Enable TBH", start: 3, length: 1 },
			{ name:"Enable TBL", start: 2, length: 1 },
			{ name:"Enable IRQ14", start: 1, length: 1 },
			{ name:"Enable IRQ15", start: 0, length: 1 }
		]
	},
	0x3072: { 
		name: "P_INT_Ctrl2 ?",
	},
	0x3073: { 
		name: "P_INT_Flag0",
		fields: [
			{ name:"Pending TM0", start: 7, length: 1 },
			{ name:"Pending IRQ1", start: 6, length: 1 },
			{ name:"Pending IRQ2", start: 5, length: 1 },
			{ name:"Pending 2048", start: 4, length: 1 },
			{ name:"Pending 8192", start: 3, length: 1 },
			{ name:"Pending SPU", start: 2, length: 1 },
			{ name:"Pending SPI", start: 1, length: 1 },
			{ name:"Pending FP", start: 0, length: 1 }
		]
	},
	0x3074: { 
		name: "P_INT_Flag1",
		fields: [
			{ name:"Pending IRQ8", start: 7, length: 1 },
			{ name:"Pending IRQ9", start: 6, length: 1 },
			{ name:"Pending TM1", start: 5, length: 1 },
			{ name:"Pending IRQ11", start: 4, length: 1 },
			{ name:"Pending TBH", start: 3, length: 1 },
			{ name:"Pending TBL", start: 2, length: 1 },
			{ name:"Pending IRQ14", start: 1, length: 1 },
			{ name:"Pending IRQ15", start: 0, length: 1 }
		]
	},
	0x3075: { 
		name: "P_INT_Flag2 ?",
	},
	0x3076: { 
		name: "P_NMI_Ctrl",
	},

	// SPI 
	0x30B0: {
		name: "P_SPI_Ctrl ?",
	},
	0x30B1: {
		name: "SPI related",
	},
	0x30B2: {
		name: "SPI related ?",
	},
	0x30B3: {
		name: "P_SPI_Write ?",
	},
	0x30B4: {
		name: "SPI related ?",
	},
	0x30B5: {
		name: "SPI related ?",
	},
	0x30B6: {
		name: "P_SPI_Read ?",
	},
	0x30B7: {
		name: "P_SPI_Status ?",
	},
	0x30BA: {
		name: "SPI related ?"
	},
};

},{}],12:[function(require,module,exports){
var config = require ("./config.js"),
	tamagotchi = require("./cpu/tamagotchi.js"),
	disassemble = require("./cpu/disassembler.js"),
	ports = require("./data/ports.js"),

	object = require("../util/object.js"),
	
	mainTemplate = require("../templates/main.html"),
	portTemplate = require("../templates/port.html");

function getBinary(path, cb) {
	var xhr = new XMLHttpRequest();
	xhr.open("GET", path, true);
	xhr.responseType = "arraybuffer";
	xhr.send();

	xhr.onreadystatechange = function () {
		if (xhr.readyState !== 4) {
			return ;
		}

		if (xhr.status !== 200) {
			throw new Error("Could not download " + path);
		}

		cb(xhr.response);
	};
}

function toHex(w, i) { 
	i = i.toString(16).toUpperCase();

	var zeros = "0";
	while (zeros.length < w) { zeros += zeros; }

	return zeros.substr(0, w).substr(i.length) + i;
}

function start(bios) {
	getBinary("files/tamago.bin", function (bios) {
		// Bind to tamago system class
		tamagotchi.system.prototype.bios = bios;

		// Start the application when BIOS is done
		[].forEach.call(document.querySelectorAll("tamago"), function (elem) {
			new Tamago(elem);
		});
	});
}

function Tamago(element) {
	var u8 = new Uint8Array(this.bios),
		that = this;

	this.system = new tamagotchi.system();

	this.configure(element);

	this._pixeldata = this.body.display.getImageData(0,0,64,31);
	this._pixels = new Uint32Array(this._pixeldata.data.buffer);
	this._disasmOffset = 0;

	this.refresh();

	document.addEventListener("keyup", function (e) {
		that.system.keys |= that.mapping[e.keyCode] || 0;
	});
	document.addEventListener("keydown", function (e) {
		that.system.keys &= ~that.mapping[e.keyCode] || 0xFF;
	});
}

// Keyboard mapping
Tamago.prototype.mapping = { 65: 1, 83: 2, 68: 4, 82: 8 };


Tamago.prototype.step = function (e) {
	this.system.step();
	this.refresh();
}

Tamago.prototype.irq = function (e) {
	this.system.fire_irq(parseInt(this.body.selects.irq.value,10));
	this.refresh();
}

Tamago.prototype.nmi = function (e) {
	this.system.fire_nmi(6);
	this.refresh();
}

Tamago.prototype.run = function (e) {
	var that = this;

	function frame() {
		if (!that.running) { return ; }

		that.system.step_realtime();
		that.refresh();
		requestAnimationFrame(frame);
	}

	this.running = !this.running;	
	frame();

	if (e) { e.target.attributes.value.value = this.running ? "stop" : "run"; }
}

Tamago.prototype.reset = function (e) {
	this.system.reset();
	this.refresh();		
}

Tamago.prototype.refresh_simple = function () {
	var a = 4, b = 0, g = 0;

	while (g < 10) {
		var glyph = (this.system._dram[a] >> b) & 3;
		if ((b -= 2) < 0) { b = 6; a++; }

		this.body.glyphs[g++].style.color = "#" + (this.system.PALETTE[glyph] & 0xFFFFFF).toString(16);
	}

	var px = 0;
	for (var y = 0; y < 31; y++) {
		var a = this.system.LCD_ORDER[y]; 

		for (var x = 0; x < 64; x += 4) {
			var d = this.system._dram[a++], b = 6;

			while (b >= 0) {
				this._pixels[px++] = this.system.PALETTE[(d >> b) & 3];
				b -= 2;
			}
		}
	}

	this.body.display.putImageData(this._pixeldata, 0, 0);
}

Tamago.prototype.drop = function (evt) {
	evt.stopPropagation();
	evt.preventDefault();
	 
	var files = evt.dataTransfer.files,
		binary = files[0],
		that = this;
	
	if (files.length < 0) { return ; }

	this.body.figure.innerHTML = binary.name + " inserted";
	
	var reader = new FileReader();
	reader.onload = function(e) {
		that.system.insert_figure(e.target.results);
	}
	reader.readAsArrayBuffer(binary);
};

Tamago.prototype.update_control = function (e) {
	if (e) {
		this._debug_port = parseInt(e.target.dataset.address);
	}

	var port = ports[this._debug_port];
	if (!port) {
		port = {
			name: "Unknown",
			description: "",
		}
	}
	if (!port.fields) {
		port.fields = [{ name:"data", start: 0, length: 8 }];
	}

	port = Object.create(port);
	port.address = this._debug_port.toString(16);

	if (port.address.length < 2) port.address = "0" + port.address;

	this.body.port.innerHTML = portTemplate(port);
	this.body.fields = this.body.port.querySelectorAll("field");
	
	this.refresh_port();
}

Tamago.prototype.refresh_port = function () {
	var d = this.system.read(this._debug_port, true);

	function pad(s, l) {
		return "00000000".substr(0, l).substr(s.length) + s;
	}

	[].forEach.call(this.body.fields, function (f) {
		var l = Number(f.dataset.length),
			s = Number(f.dataset.start),
			m = (d >> s) & ((1 << l) - 1),
			b = f.querySelector("bin"),
			h = f.querySelector("hex");

		b.innerHTML = pad(m.toString(2), l);
		h.innerHTML = pad(m.toString(16), Math.ceil(l / 4));
	})
}

Tamago.prototype.refresh_debugger = function () {
	var that = this;

	// Update basic views
	object.each(this.body.registers, function (elem, register) {
		elem.innerHTML = toHex(2, that.system[register]);
	});

	object.each(this.body.flags, function (elem, flag) {
		elem.classList.toggle("active", Boolean(that.system[flag]));
	});

	this.body.memory.forEach(function (m, i) {
		m.innerHTML = toHex(2, that.system._wram[i]);
	});

	this.body.control.forEach(function (m, i) {
		var acc = that.system._cpuacc[i+0x3000];
		that.system._cpuacc[i+0x3000] = 0;
		m.classList.toggle('read', acc & tamagotchi.ACCESS_READ);
		m.classList.toggle('write', acc & tamagotchi.ACCESS_WRITE);
		m.innerHTML = toHex(2, that.system._cpureg[i]);
	});


	var disasm = disassemble.disassemble(config.instructionCount, this._disasmOffset, this.system),
		bias = Math.floor(config.instructionCount / 2),
		current = disasm.reduce(function(acc, d, i){ return d.active ? i : acc; }, null);

	// PC isn't were it should be
	if (current === null) {
		this._disasmOffset = this.system.pc;
		disasm = disassemble.disassemble(config.instructionCount, this._disasmOffset, this.system);
	} else if (current >= bias && disasm.length == config.instructionCount) {
		this._disasmOffset = disasm[current-bias].location;
		disasm = disassemble.disassemble(config.instructionCount, this._disasmOffset, this.system);
	}

	disasm.forEach(function (g, i) {
		var row = that.body.instructions[i];

		row.location.innerHTML = toHex(4, g.location)
		row.opcode.innerHTML = g.instruction;
		row.addressing.innerHTML = ((g.data === null) ? "" : g.data).toString(16).toUpperCase();
		row.data.innerHTML = g.bytes;

		function attr(node, attr, value) {
			if(value !== undefined) { node.setAttribute(attr, value) }
			else node.removeAttribute(attr);
		}

		row.instruction.classList.toggle("active", g.active === true);
		attr(row.addressing, 'mode', g.mode);
		attr(row.addressing, 'address', (g.address || 0).toString(16).toUpperCase());
		attr(row.instruction, 'port', g.port);
	});

	for (var i = disasm.length; i < config.instructionCount; i++) {
		var row = that.body.instructions[i];

		row.location.innerHTML = "";
		row.opcode.innerHTML = "";
		row.addressing.innerHTML = "";
		row.data.innerHTML = "";
		row.addressing.removeAttribute('mode');
	}

	this.refresh_port();
	this.refresh_simple();
}

Tamago.prototype.configure = function(element) {
	var data = Object.create(config),
		that = this;

	data.toHex = toHex;
	data.ramBytes = this.system._wram.length;
	data.registerBytes = this.system._cpureg.length;

	data.debug = Boolean(element.attributes.debugger);

	element.innerHTML = mainTemplate(data);

	function noopHandler(evt) {
		evt.stopPropagation();
		evt.preventDefault();
	}

	element.addEventListener("dragenter", noopHandler, false);
	element.addEventListener("dragexit", noopHandler, false);
	element.addEventListener("dragover", noopHandler, false);
	element.addEventListener("drop", this.drop.bind(this), false);

	// Bind to HTML
	if (data.debug) {
		[].forEach.call(document.querySelectorAll("input[type=button]"), function (el) {
			el.addEventListener("click", that[el.attributes.action.value].bind(that))
		});

		this.body = {
			glyphs: element.querySelectorAll("i.glyph"),
			port: element.querySelector("port"),
			selects: [].reduce.call(element.querySelectorAll("select"), function (acc, f) { 
				acc[f.attributes.action.value.toLowerCase()] = f;
				return acc; 
			}, {}),
			flags: [].reduce.call(element.querySelectorAll("flag"), function (acc, f) { 
				acc[f.attributes.name.value.toLowerCase()] = f;
				return acc; 
			}, {}),
			registers: [].reduce.call(element.querySelectorAll("register"), function (acc, r) { 
				acc[r.attributes.name.value.toLowerCase()] = r;
				return acc; 
			}, {}),
			instructions: [].map.call(element.querySelectorAll("instruction"), function (i) {
				return {
					instruction: i,
					location: i.querySelector("location"),
					opcode: i.querySelector("opcode"),
					data: i.querySelector("data"),
					addressing: i.querySelector("addressing"),
				};
			}),
			control: [].map.call(element.querySelectorAll("control byte"), function (b) {
				b.addEventListener("click", that.update_control.bind(that));

				return b;
			}),
			memory: [].map.call(element.querySelectorAll("memory byte"), function (b) {
				return b;
			}),
			display: element.querySelector("display canvas").getContext("2d"),
			figure: element.querySelector("display figure")
		};

		document.querySelector("select[action=figure]").addEventListener("change", function(e) {
			that.system.inserted_figure = Number(e.target.value);
		});

		this._debug_port = 0x3000;
		this.update_control();

		this.refresh = this.refresh_debugger;
	} else {
		this.body = { 
			glyphs: element.querySelectorAll("i.glyph"),
			display: element.querySelector("display canvas").getContext("2d"),
			figure: element.querySelector("display figure")
		};

		this.refresh = this.refresh_simple;
		// Start running soon
		setTimeout(function() { that.run(); }, 10);
	}
};

module.exports = {
	start: start
};

},{"../templates/main.html":13,"../templates/port.html":14,"../util/object.js":15,"./config.js":2,"./cpu/disassembler.js":5,"./cpu/tamagotchi.js":9,"./data/ports.js":11}],13:[function(require,module,exports){
module.exports=(function() {var t = function anonymous(locals, filters, escape, rethrow) {
escape = escape || function (html){
  return String(html)
    .replace(/&(?!#?[a-zA-Z0-9]+;)/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/'/g, '&#39;')
    .replace(/"/g, '&quot;');
};
var buf = [];
with (locals || {}) { (function(){ 
 buf.push('<column>\n	<display>\n		<figure></figure>\n		<div>\n			<i class="icon icon-dashboard glyph"></i>\n			<i class="icon icon-food glyph"></i>\n			<i class="icon icon-trash glyph"></i>\n			<i class="icon icon-globe glyph"></i>\n			<i class="icon icon-user glyph"></i>\n		</div>\n		<canvas width="48" height="31"></canvas>\n		<div>\n			<i class="icon icon-comments glyph"></i>\n			<i class="icon icon-medkit glyph"></i>\n			<i class="icon icon-heart glyph"></i>\n			<i class="icon icon-book glyph"></i>\n			<i class="icon icon-bell glyph"></i>\n		</div>\n	</display>\n	\n	');21; if (debug) { ; buf.push('\n		<buttons>\n			<input type="button" value="step" action="step"></input>\n			<input type="button" value="run" action="run"></input>\n			<input type="button" value="reset" action="reset"></input>\n			<input type="button" value="nmi" action="nmi"></input>\n		</buttons>\n		<buttons>\n			<select action="figure">\n				<option value="0">No Figure</option>\n				<option value="1">Fig1</option>\n				<option value="2">Fig2</option>\n				<option value="3">Fig3</option>\n			</select>\n\n			<select action="irq">\n				<option value="0">0: TIM0</option>\n				<option value="3">3: 2048</option>\n				<option value="4">4: 8192</option>\n				<option value="5">5: SPU</option>\n				<option value="6">6: SPI</option>\n				<option value="7">7: FP</option>\n				<option value="10">10: TIM1</option>\n				<option value="12">12: TBH</option>\n				<option value="13">13: TBL</option>\n			</select>\n			<input action="irq" type="button" value="irq"></input>\n		</buttons>\n		<cpu>\n			<flags>\n				<flag name="C"></flag>\n				<flag name="Z"></flag>\n				<flag name="I"></flag>\n				<flag name="D"></flag>\n				<flag name="V"></flag>\n				<flag name="N"></flag>\n			</flags>\n\n			<registers>\n				<register name="A"></register>\n				<register name="X"></register>\n				<register name="Y"></register>\n				<register name="S"></register>\n				<register name="PC"></register>\n			</registers>\n		</cpu>\n		<control>\n			');68; for (var i = 0; i < registerBytes; i += registerBytesPerLine ) { ; buf.push('\n				<row>\n					<address>', escape((70,  toHex(4, i+0x3000) )), '</address>\n					');71; for (var b = 0; b < registerBytesPerLine; b ++ ) { ; buf.push('\n						<byte data-address="', escape((72,  i+b+0x3000 )), '"></byte>\n					');73; } ; buf.push('\n				</row>\n			');75; } ; buf.push('\n		</control>\n	</column>\n	<column>\n		<disassembly>\n			');80; for (var i = 0; i < instructionCount; i++ ) { ; buf.push('\n			<instruction port>\n				<location></location>\n				<opcode></opcode>\n				<addressing mode address></addressing>\n				<data></data>\n			</instruction>\n			');87; } ; buf.push('\n		</disassembly>\n	</column>\n	<column>\n		<port></port>\n		<memory>\n			');93; for (var i = 0; i < ramBytes; i += memoryBytesPerLine ) { ; buf.push('\n				<row>\n					<address>', escape((95,  toHex(4, i) )), '</address>\n					');96; for (var b = 0; b < memoryBytesPerLine; b ++ ) { ; buf.push('\n						<byte data-address="', escape((97,  i+b )), '"></byte>\n					');98; } ; buf.push('\n				</row>\n			');100; } ; buf.push('\n		</memory>\n	');102; } ; buf.push('\n</column>'); })();
} 
return buf.join('');
}; return function(l) { return t(l) }}())
},{}],14:[function(require,module,exports){
module.exports=(function() {var t = function anonymous(locals, filters, escape, rethrow) {
escape = escape || function (html){
  return String(html)
    .replace(/&(?!#?[a-zA-Z0-9]+;)/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/'/g, '&#39;')
    .replace(/"/g, '&quot;');
};
var buf = [];
with (locals || {}) { (function(){ 
 buf.push('<h1>', escape((1,  name )), ' (0x', escape((1,  address )), ')</h1>\n\n');3; for (var i = 0; i < fields.length; i++ ) { var field = fields[i] ; buf.push('\n	<field name="', escape((4,  field.name )), '" data-start="', escape((4,  field.start )), '" data-length="', escape((4,  field.length )), '">\n		<range> \n			[', escape((6,  field.start )), '');6; if(field.length > 1) { ; buf.push(':', escape((6,  field.length + field.start - 1 )), '');6; } ; buf.push(']\n		</range>\n		<hex></hex>\n		<bin></bin>\n	</field>\n');11; } ; buf.push('   '); })();
} 
return buf.join('');
}; return function(l) { return t(l) }}())
},{}],15:[function(require,module,exports){
function each(o, cb) {
	if (Array.isArray(o)) { o.forEach(cb); }
	Object.getOwnPropertyNames(o).forEach(function (n) { cb(o[n], n, o); })
}

function extend(a, b) {
	each(b, function (v, k) { a[k] = v; })
}

function fill(c, v) {
	var a = []; 
	if (v === undefined) { v = 0; }
	while(c-- > 0) { a.push(0); }
	return a;
}

module.exports =  {
	each: each,
	extend: extend,
	fill: fill
};

},{}]},{},[1,2,3,4,5,6,7,8,9,10,11,12,15])
//@ sourceMappingURL=data:application/json;base64,eyJ2ZXJzaW9uIjozLCJzb3VyY2VzIjpbIi9ob21lL3Rlc3QvdGFtYS90YW1hZ28vc3JjL3RhbWFnby5qcyIsIi9ob21lL3Rlc3QvdGFtYS90YW1hZ28vc3JjL3RhbWFnby9jb25maWcuanMiLCIvaG9tZS90ZXN0L3RhbWEvdGFtYWdvL3NyYy90YW1hZ28vY3B1LzY1MDIuanMiLCIvaG9tZS90ZXN0L3RhbWEvdGFtYWdvL3NyYy90YW1hZ28vY3B1L2FkZHJlc3MuanMiLCIvaG9tZS90ZXN0L3RhbWEvdGFtYWdvL3NyYy90YW1hZ28vY3B1L2Rpc2Fzc2VtYmxlci5qcyIsIi9ob21lL3Rlc3QvdGFtYS90YW1hZ28vc3JjL3RhbWFnby9jcHUvZWVwcm9tLmpzIiwiL2hvbWUvdGVzdC90YW1hL3RhbWFnby9zcmMvdGFtYWdvL2NwdS9vcGVyYXRpb25zLmpzIiwiL2hvbWUvdGVzdC90YW1hL3RhbWFnby9zcmMvdGFtYWdvL2NwdS9yZWdpc3RlcnMuanMiLCIvaG9tZS90ZXN0L3RhbWEvdGFtYWdvL3NyYy90YW1hZ28vY3B1L3RhbWFnb3RjaGkuanMiLCIvaG9tZS90ZXN0L3RhbWEvdGFtYWdvL3NyYy90YW1hZ28vZGF0YS9pbnN0cnVjdGlvbnMuanMiLCIvaG9tZS90ZXN0L3RhbWEvdGFtYWdvL3NyYy90YW1hZ28vZGF0YS9wb3J0cy5qcyIsIi9ob21lL3Rlc3QvdGFtYS90YW1hZ28vc3JjL3RhbWFnby9tYWluLmpzIiwiL2hvbWUvdGVzdC90YW1hL3RhbWFnby9zcmMvdGVtcGxhdGVzL21haW4uaHRtbCIsIi9ob21lL3Rlc3QvdGFtYS90YW1hZ28vc3JjL3RlbXBsYXRlcy9wb3J0Lmh0bWwiLCIvaG9tZS90ZXN0L3RhbWEvdGFtYWdvL3NyYy91dGlsL29iamVjdC5qcyJdLCJuYW1lcyI6W10sIm1hcHBpbmdzIjoiO0FBQUE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTs7QUM1QkE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOztBQ0xBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7O0FDN0dBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOztBQ3ZGQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7O0FDMUtBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOztBQ2hKQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTs7QUMxUkE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOztBQzFIQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7O0FDOU1BO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTs7QUN6dkJBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOztBQzVQQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOztBQzVXQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7O0FDZEE7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBOztBQ2RBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBO0FBQ0E7QUFDQTtBQUNBIiwiZmlsZSI6ImdlbmVyYXRlZC5qcyIsInNvdXJjZVJvb3QiOiIiLCJzb3VyY2VzQ29udGVudCI6WyJ2YXIgdGFtYWdvID0gcmVxdWlyZShcIi4vdGFtYWdvL21haW4uanNcIik7XG5cbnZhciBsYXN0VGltZSA9IDA7XG52YXIgdmVuZG9ycyA9IFsnbXMnLCAnbW96JywgJ3dlYmtpdCcsICdvJ107XG5cbmZvcih2YXIgeCA9IDA7IHggPCB2ZW5kb3JzLmxlbmd0aCAmJiAhd2luZG93LnJlcXVlc3RBbmltYXRpb25GcmFtZTsgKyt4KSB7XG4gICAgd2luZG93LnJlcXVlc3RBbmltYXRpb25GcmFtZSA9IHdpbmRvd1t2ZW5kb3JzW3hdKydSZXF1ZXN0QW5pbWF0aW9uRnJhbWUnXTtcbiAgICB3aW5kb3cuY2FuY2VsQW5pbWF0aW9uRnJhbWUgPSB3aW5kb3dbdmVuZG9yc1t4XSsnQ2FuY2VsQW5pbWF0aW9uRnJhbWUnXSBcbiAgICAgICAgICAgICAgICAgICAgICAgICAgICAgICB8fCB3aW5kb3dbdmVuZG9yc1t4XSsnQ2FuY2VsUmVxdWVzdEFuaW1hdGlvbkZyYW1lJ107XG59XG5cbmlmICghd2luZG93LnJlcXVlc3RBbmltYXRpb25GcmFtZSlcbiAgICB3aW5kb3cucmVxdWVzdEFuaW1hdGlvbkZyYW1lID0gZnVuY3Rpb24oY2FsbGJhY2ssIGVsZW1lbnQpIHtcbiAgICAgICAgdmFyIGN1cnJUaW1lID0gbmV3IERhdGUoKS5nZXRUaW1lKCk7XG4gICAgICAgIHZhciB0aW1lVG9DYWxsID0gTWF0aC5tYXgoMCwgMTYgLSAoY3VyclRpbWUgLSBsYXN0VGltZSkpO1xuICAgICAgICB2YXIgaWQgPSB3aW5kb3cuc2V0VGltZW91dChmdW5jdGlvbigpIHsgY2FsbGJhY2soY3VyclRpbWUgKyB0aW1lVG9DYWxsKTsgfSwgXG4gICAgICAgICAgdGltZVRvQ2FsbCk7XG4gICAgICAgIGxhc3RUaW1lID0gY3VyclRpbWUgKyB0aW1lVG9DYWxsO1xuICAgICAgICByZXR1cm4gaWQ7XG4gICAgfTtcblxuaWYgKCF3aW5kb3cuY2FuY2VsQW5pbWF0aW9uRnJhbWUpIHtcbiAgICB3aW5kb3cuY2FuY2VsQW5pbWF0aW9uRnJhbWUgPSBmdW5jdGlvbihpZCkge1xuICAgICAgICBjbGVhclRpbWVvdXQoaWQpO1xuICAgIH07XG59XG5cbnRhbWFnby5zdGFydCgpO1xuIiwibW9kdWxlLmV4cG9ydHMgPSB7XG5cdG1lbW9yeUJ5dGVzUGVyTGluZTogMTYsXG5cdHJlZ2lzdGVyQnl0ZXNQZXJMaW5lOiA4LFxuXHRpbnN0cnVjdGlvbkNvdW50OiA1NVxufTtcbiIsInZhciBhZGRyZXNzaW5nID0gcmVxdWlyZShcIi4vYWRkcmVzcy5qc1wiKSxcblx0XHRvcGVyYXRpb25zID0gcmVxdWlyZShcIi4vb3BlcmF0aW9ucy5qc1wiKSxcblx0XHRpbnN0cnVjdGlvbnMgPSByZXF1aXJlKFwiLi4vZGF0YS9pbnN0cnVjdGlvbnMuanNcIiksXG5cdFx0b2JqZWN0ID0gcmVxdWlyZShcIi4uLy4uL3V0aWwvb2JqZWN0LmpzXCIpO1xuXG52YXIgcjY1MDIgPSB7fSxcblx0XHRvcHMgPSB7fTtcblxucjY1MDIuaW5pdCA9IGZ1bmN0aW9uICgpIHtcblx0dGhpcy5hID0gMDtcblx0dGhpcy54ID0gMDtcblx0dGhpcy55ID0gMDtcblx0dGhpcy5zID0gMDtcblx0dGhpcy5wID0gMDtcblx0dGhpcy5jeWNsZXMgPSAwO1xuXHR0aGlzLnBjID0gMDtcblxuXHRvYmplY3QuZWFjaChpbnN0cnVjdGlvbnMsIGZ1bmN0aW9uKG9wLCBjb2RlKSB7XG5cdFx0b3BzW2NvZGVdID0ge1xuXHRcdFx0b3BlcmF0aW9uOiBvcGVyYXRpb25zW29wLmluc3RydWN0aW9uXSxcblx0XHRcdGFkZHJlc3M6IGFkZHJlc3Npbmdbb3AuYWRkcmVzc2luZ10sXG5cdFx0XHRjeWNsZXM6IG9wLmN5Y2xlc1xuXHRcdH07XG5cdH0pO1xufVxuXG5yNjUwMi5yZXNldCA9IGZ1bmN0aW9uICgpIHtcblx0dGhpcy5wYyA9IHRoaXMucmVhZF8xNigweEZGRkMpO1xufVxuXG5yNjUwMi5ubWkgPSBmdW5jdGlvbiAoKSB7XG5cdHRoaXMucHVzaCh0aGlzLnBjID4+IDgpO1xuXHR0aGlzLnB1c2godGhpcy5wYyAmIDB4RkYpO1xuXHR0aGlzLnB1c2godGhpcy5wKTtcblxuXHR0aGlzLnBjID0gdGhpcy5yZWFkXzE2KDB4RkZGQSk7XG59O1xuXG5yNjUwMi5pcnEgPSBmdW5jdGlvbiAoYnJrKSB7XG5cdHRoaXMucHVzaCh0aGlzLnBjID4+IDgpO1xuXHR0aGlzLnB1c2godGhpcy5wYyAmIDB4RkYpO1xuXHR0aGlzLnB1c2godGhpcy5wIHwgKGJyayA/IDB4MTAgOiAwKSk7XG5cblx0dGhpcy5wYyA9IHRoaXMucmVhZF8xNigweEZGRkUpO1xuXG5cdHRoaXMuaSA9IDE7XG59O1xuXG5yNjUwMi5zdGVwID0gZnVuY3Rpb24gKCkge1xuXHQvLyBGaXJlIHBlbmRpbmcgSVJRc1xuXHRpZiAoIXRoaXMuaSAmJiB0aGlzLnBlbmRpbmdfaXJxKCkpIHsgdGhpcy5pcnEoKTsgfVxuXG5cdHZhciBuZXh0ID0gb3BzW3RoaXMubmV4dCgpXTtcblx0aWYgKG5leHQgPT09IHVuZGVmaW5lZCkgeyB0aHJvdyBuZXcgRXJyb3IoXCJTeXN0ZW0gaGFzIGNyYXNoZWQgKGludmFsaWQgb3BlcmF0aW9uKVwiKTsgfVxuXHRuZXh0Lm9wZXJhdGlvbih0aGlzLCBuZXh0LmFkZHJlc3ModGhpcykpO1xuXHR0aGlzLmN5Y2xlcyAtPSBuZXh0LmN5Y2xlcztcbn07XG5cbnI2NTAyLm5leHQgPSBmdW5jdGlvbiAoKSB7XG5cdHZhciBkID0gdGhpcy5yZWFkKHRoaXMucGMrKyk7XG5cdHRoaXMucGMgJj0gMHhGRkZGO1xuXHRyZXR1cm4gZDtcbn07XG5cbnI2NTAyLm5leHRfMTYgPSBmdW5jdGlvbiAoKSB7XG5cdHZhciBsID0gdGhpcy5uZXh0KCksXG5cdFx0aCA9IHRoaXMubmV4dCgpO1xuXG5cdHJldHVybiBsIHwgKGggPDwgOCk7XG59O1xuXG5yNjUwMi5yZWFkXzE2ID0gZnVuY3Rpb24gKGFkZHIpIHtcblx0dmFyIGwgPSB0aGlzLnJlYWQoYWRkciksXG5cdFx0aCA9IHRoaXMucmVhZCgoYWRkcisxKSAmIDB4RkZGRik7XG5cblx0cmV0dXJuIGwgfCAoaCA8PCA4KTtcbn07XG5cbnI2NTAyLnB1bGwgPSBmdW5jdGlvbiAoKSB7XG5cdHRoaXMucyA9ICh0aGlzLnMgKyAxKSAmIDB4RkY7XG5cdHJldHVybiB0aGlzLnJlYWQodGhpcy5zIHwgMHgxMDApO1xufTtcblxucjY1MDIucHVzaCA9IGZ1bmN0aW9uIChkYXRhKSB7XG5cdHRoaXMud3JpdGUodGhpcy5zIHwgMHgxMDAsIGRhdGEpO1xuXHR0aGlzLnMgPSAodGhpcy5zIC0gMSkgJiAweEZGO1xufTtcblxuT2JqZWN0LmRlZmluZVByb3BlcnR5KHI2NTAyLCBcInBcIiwge1xuXHRnZXQ6IGZ1bmN0aW9uICgpIHtcblx0XHRyZXR1cm4gKCh0aGlzLmMpID8gMHgwMTogMCkgfFxuXHRcdFx0KCh0aGlzLnopID8gMHgwMjogMCkgfFxuXHRcdFx0KCh0aGlzLmkpID8gMHgwNDogMCkgfFxuXHRcdFx0KCh0aGlzLmQpID8gMHgwODogMCkgfFxuXHRcdFx0MHgyMCB8IC8vIEFsd2F5cyBzZXRcblx0XHRcdCgodGhpcy52KSA/IDB4NDA6IDApIHxcblx0XHRcdCgodGhpcy5uKSA/IDB4ODA6IDApO1xuXHR9LFxuXHRzZXQ6IGZ1bmN0aW9uICh2KSB7XG5cdFx0dGhpcy5jID0gdiAmIDB4MDE7XG5cdFx0dGhpcy56ID0gdiAmIDB4MDI7XG5cdFx0dGhpcy5pID0gdiAmIDB4MDQ7XG5cdFx0dGhpcy5kID0gdiAmIDB4MDg7XG5cdFx0dGhpcy52ID0gdiAmIDB4NDA7XG5cdFx0dGhpcy5uID0gdiAmIDB4ODA7XG5cdH1cbn0pO1xuXG5tb2R1bGUuZXhwb3J0cyA9IHsgcjY1MDI6IHI2NTAyIH07XG4iLCIvKipcbiAqKiBUaGlzIGNhbGN1bGF0ZXMgZWZmZWN0aXZlIGFkZHJlc3MgZm9yIHZhcmlvdXMgbW9kZXNcbiAqKiBOT1RFOiBJIGFsd2F5cyByZXR1cm4gYWRkciBmb3IgZnV0dXJlIGF1dG8taW5saW5pbmdcbiAqKi9cblxubW9kdWxlLmV4cG9ydHMgPSB7XG5cdGltcGxpZWQ6IGZ1bmN0aW9uIChjcHUpIHtcblx0XHR2YXIgYWRkciA9IG51bGw7XG5cdFx0cmV0dXJuIGFkZHI7XG5cdH0sIFxuXG5cdGFjY3VtdWxhdG9yOiBmdW5jdGlvbiAoY3B1KSB7XG5cdFx0dmFyIGFkZHIgPSBudWxsO1xuXHRcdHJldHVybiBhZGRyO1xuXHR9LCBcblxuXHRpbW1lZGlhdGU6IGZ1bmN0aW9uIChjcHUpIHtcblx0XHR2YXIgYWRkciA9IGNwdS5wYysrO1xuXHRcdGNwdS5wYyAmPSAweEZGRkY7XG5cdFx0cmV0dXJuIGFkZHI7XG5cdH0sIFxuXG5cdHJlbGF0aXZlOiBmdW5jdGlvbiAoY3B1KSB7XG5cdFx0Ly8gU2lnbiBleHRlbmQgOC1iaXQgdmFsdWUsIGFuZCBhZGQgaXQgdG8gdGhlIG5vdyBpbmNyZW1lbnRlZCBQQy5cblx0XHR2YXIgYWRkciA9ICgoY3B1Lm5leHQoKSA8PCAyNCA+PiAyNCkgKyBjcHUucGMpICYgMHhGRkZGO1xuXG5cdFx0cmV0dXJuIGFkZHI7XG5cdH0sIFxuXG5cdHplcm9wYWdlOiBmdW5jdGlvbiAoY3B1KSB7XG5cdFx0dmFyIGFkZHIgPSBjcHUubmV4dCgpO1xuXHRcdHJldHVybiBhZGRyO1xuXHR9LFxuXG5cdHplcm9wYWdlWDogZnVuY3Rpb24gKGNwdSkge1xuXHRcdHZhciBhZGRyID0gKGNwdS5uZXh0KCkgKyBjcHUueCkgJiAweEZGO1xuXHRcdHJldHVybiBhZGRyO1xuXHR9LCBcblxuXHR6ZXJvcGFnZVk6IGZ1bmN0aW9uIChjcHUpIHtcblx0XHR2YXIgYWRkciA9IChjcHUubmV4dCgpICsgY3B1LnkpICYgMHhGRjtcblx0XHRyZXR1cm4gYWRkcjtcblx0fSxcblxuXHRhYnNvbHV0ZTogZnVuY3Rpb24gKGNwdSkge1xuXHRcdHZhciBhZGRyID0gY3B1Lm5leHRfMTYoKTtcblx0XHRyZXR1cm4gYWRkcjtcblx0fSxcblxuXHRhYnNvbHV0ZVg6IGZ1bmN0aW9uIChjcHUpIHtcblx0XHR2YXIgYWRkciA9IChjcHUubmV4dF8xNigpICsgY3B1LngpICYgMHhGRkZGO1xuXHRcdHJldHVybiBhZGRyO1xuXHR9LCBcblxuXHRhYnNvbHV0ZVk6IGZ1bmN0aW9uIChjcHUpIHtcblx0XHR2YXIgYWRkciA9IChjcHUubmV4dF8xNigpICsgY3B1LnkpICYgMHhGRkZGO1xuXHRcdHJldHVybiBhZGRyO1xuXHR9LFxuXG5cdGluZGlyZWN0OiBmdW5jdGlvbiAoY3B1KSB7XG5cdFx0dmFyIGFkZHIgPSBjcHUubmV4dF8xNigpLFxuXHRcdFx0YWRkcl9sID0gY3B1LnJlYWQoYWRkciksXG5cdFx0XHRhZGRyX2ggPSBjcHUucmVhZCgoKGFkZHIgKyAxKSAmIDB4MDBGRikgfCAoYWRkciAmIDB4RkYwMCkpO1xuXG5cdFx0YWRkciA9IGFkZHJfbCB8IChhZGRyX2ggPDwgOCk7XG5cblx0XHRyZXR1cm4gYWRkcjtcblx0fSwgXG5cblx0aW5kaXJlY3RYOiBmdW5jdGlvbiAoY3B1KSB7XG5cdFx0dmFyIGFkZHIgPSAoY3B1Lm5leHQoKSArIGNwdS54KSAmIDB4RkYsXG5cdFx0XHRhZGRyX2wgPSBjcHUucmVhZChhZGRyKSxcblx0XHRcdGFkZHJfaCA9IGNwdS5yZWFkKChhZGRyKzEpICYgMHhGRik7XG5cdFx0XG5cdFx0YWRkciA9IGFkZHJfbCB8IChhZGRyX2ggPDwgOCk7XG5cdFx0cmV0dXJuIGFkZHI7XG5cdH0sXG5cblx0aW5kaXJlY3RZOiBmdW5jdGlvbiAoY3B1KSB7XG5cdFx0dmFyIGFkZHIgPSBjcHUubmV4dCgpLFxuXHRcdFx0YWRkcl9sID0gY3B1LnJlYWQoYWRkciksXG5cdFx0XHRhZGRyX2ggPSBjcHUucmVhZCgoYWRkcisxKSAmIDB4RkYpO1xuXG5cdFx0YWRkciA9ICgoYWRkcl9sIHwgKGFkZHJfaCA8PCA4KSkgKyBjcHUueSkgJiAweEZGRkY7XG5cdFx0cmV0dXJuIGFkZHI7XG5cdH1cbn07XG4iLCJ2YXIgaW5zdHJ1Y3Rpb25zID0gcmVxdWlyZShcIi4uL2RhdGEvaW5zdHJ1Y3Rpb25zLmpzXCIpLFxuXHRcdHBvcnRzID0gcmVxdWlyZShcIi4uL2RhdGEvcG9ydHMuanNcIiksXG5cdFx0Y29uZmlnID0gcmVxdWlyZShcIi4uL2NvbmZpZy5qc1wiKTtcblxuZnVuY3Rpb24gZGlzYXNzZW1ibGUoY291bnQsIGFkZHJlc3MsIGNwdSkge1xuXHR2YXIgaSA9IFtdO1xuXG5cdGZ1bmN0aW9uIHoxNihhZGRyKSB7XG5cdFx0dmFyIGwgPSBjcHUucmVhZChhZGRyICYgMHhGRiksXG5cdFx0XHRoID0gY3B1LnJlYWQoKGFkZHIrMSkgJiAweEZGKTtcblxuXHRcdHJldHVybiBsIHwgKGggPDwgOCk7XG5cdH1cblxuXHRmdW5jdGlvbiBnMTYoYWRkcikge1xuXHRcdHZhciBsID0gY3B1LnJlYWQoYWRkciAmIDB4RkZGRiksXG5cdFx0XHRoID0gY3B1LnJlYWQoKGFkZHIrMSkgJiAweEZGRkYpO1xuXG5cdFx0cmV0dXJuIGwgfCAoaCA8PCA4KTtcblx0fVxuXG5cdGZ1bmN0aW9uIHI4KCkge1xuXHRcdHJldHVybiBjcHUucmVhZChhZGRyZXNzKyspO1xuXHR9XG5cblx0ZnVuY3Rpb24gcjE2KCkge1xuXHRcdHZhciBsID0gcjgoKSxcblx0XHRcdGggPSByOCgpO1xuXHRcdFxuXHRcdHJldHVybiBsIHwgKGggPDwgOCk7XG5cdH1cblxuXHR3aGlsZSAoY291bnQtLSA+IDApIHtcblx0XHR2YXIgcG9zID0gYWRkcmVzcyxcblx0XHRcdG9wID0gaW5zdHJ1Y3Rpb25zW3I4KCldLFxuXHRcdFx0b3V0cHV0O1xuXHRcdFxuXHRcdC8vIFVuZGVmaW5lZCBvcGVyYXRpb25cblx0XHRpZiAoIW9wKSB7IGJyZWFrIDsgfVxuXG5cdFx0c3dpdGNoIChvcC5hZGRyZXNzaW5nKSB7XG5cdFx0Y2FzZSBcImltcGxpZWRcIjpcblx0XHRcdG91dHB1dCA9IHsgXG5cdFx0XHRcdGluc3RydWN0aW9uOiBvcC5pbnN0cnVjdGlvbixcblx0XHRcdFx0ZGF0YTogbnVsbCxcblx0XHRcdFx0YWRkcmVzczogbnVsbFxuXHRcdFx0fTtcblx0XHRcdGJyZWFrIDtcblx0XHRjYXNlIFwiYWNjdW11bGF0b3JcIjpcblx0XHRcdG91dHB1dCA9IHtcblx0XHRcdFx0aW5zdHJ1Y3Rpb246IG9wLmluc3RydWN0aW9uLFxuXHRcdFx0XHRhZGRyZXNzOiBudWxsLFxuXHRcdFx0XHRkYXRhOiBcIkFcIlxuXHRcdFx0fTtcblx0XHRcdGJyZWFrIDtcblx0XHRjYXNlIFwiaW1tZWRpYXRlXCI6XG5cdFx0XHRvdXRwdXQgPSB7XG5cdFx0XHRcdGluc3RydWN0aW9uOiBvcC5pbnN0cnVjdGlvbixcblx0XHRcdFx0YWRkcmVzczogYWRkcmVzcyxcblx0XHRcdFx0ZGF0YTogcjgoKVxuXHRcdFx0fTtcblx0XHRcdGJyZWFrIDtcblx0XHRjYXNlIFwiaW5kaXJlY3RcIjpcblx0XHRcdGQgPSByMTYoKTtcblx0XHRcdG91dHB1dCA9IHtcblx0XHRcdFx0aW5zdHJ1Y3Rpb246IG9wLmluc3RydWN0aW9uLFxuXHRcdFx0XHRhZGRyZXNzOiBnMTYoZCksXG5cdFx0XHRcdGRhdGE6IGRcblx0XHRcdH07XG5cdFx0XHRicmVhayA7XG5cdFx0Y2FzZSBcImluZGlyZWN0WFwiOlxuXHRcdFx0ZCA9IHI4KCk7XG5cdFx0XHRvdXRwdXQgPSB7XG5cdFx0XHRcdGluc3RydWN0aW9uOiBvcC5pbnN0cnVjdGlvbixcblx0XHRcdFx0YWRkcmVzczogejE2KGQgKyBjcHUueCksXG5cdFx0XHRcdGRhdGE6IGRcblx0XHRcdH07XG5cdFx0XHRicmVhayA7XG5cdFx0Y2FzZSBcImluZGlyZWN0WVwiOlxuXHRcdFx0ZCA9IHI4KCk7XG5cdFx0XHRvdXRwdXQgPSB7XG5cdFx0XHRcdGluc3RydWN0aW9uOiBvcC5pbnN0cnVjdGlvbixcblx0XHRcdFx0YWRkcmVzczogKHoxNihkKSArIGNwdS55KSAmIDB4RkZGRixcblx0XHRcdFx0ZGF0YTogZFxuXHRcdFx0fTtcblx0XHRcdGJyZWFrIDtcblx0XHRjYXNlIFwiemVyb3BhZ2VcIjpcblx0XHRcdGQgPSByOCgpO1xuXHRcdFx0b3V0cHV0ID0ge1xuXHRcdFx0XHRpbnN0cnVjdGlvbjogb3AuaW5zdHJ1Y3Rpb24sXG5cdFx0XHRcdGFkZHJlc3M6IGQsXG5cdFx0XHRcdGRhdGE6IGRcblx0XHRcdH07XG5cdFx0XHRicmVhayA7XG5cdFx0Y2FzZSBcInplcm9wYWdlWFwiOlxuXHRcdFx0ZCA9IHI4KCk7XG5cdFx0XHRvdXRwdXQgPSB7XG5cdFx0XHRcdGluc3RydWN0aW9uOiBvcC5pbnN0cnVjdGlvbixcblx0XHRcdFx0YWRkcmVzczogKGQgKyBjcHUueCkgJiAweEZGLFxuXHRcdFx0XHRkYXRhOiBkXG5cdFx0XHR9O1xuXHRcdFx0YnJlYWsgO1xuXHRcdGNhc2UgXCJ6ZXJvcGFnZVlcIjpcblx0XHRcdGQgPSByOCgpO1xuXHRcdFx0b3V0cHV0ID0ge1xuXHRcdFx0XHRpbnN0cnVjdGlvbjogb3AuaW5zdHJ1Y3Rpb24sXG5cdFx0XHRcdGFkZHJlc3M6IChkICsgY3B1LnkpICYgMHhGRixcblx0XHRcdFx0ZGF0YTogZFxuXHRcdFx0fTtcblx0XHRcdGJyZWFrIDtcblx0XHRjYXNlIFwiYWJzb2x1dGVcIjpcblx0XHRcdGQgPSByMTYoKTtcblx0XHRcdG91dHB1dCA9IHtcblx0XHRcdFx0aW5zdHJ1Y3Rpb246IG9wLmluc3RydWN0aW9uLFxuXHRcdFx0XHRhZGRyZXNzOiBkLFxuXHRcdFx0XHRkYXRhOiBkXG5cdFx0XHR9O1xuXHRcdFx0YnJlYWsgO1xuXHRcdGNhc2UgXCJhYnNvbHV0ZVhcIjpcblx0XHRcdGQgPSByMTYoKTtcblx0XHRcdG91dHB1dCA9IHtcblx0XHRcdFx0aW5zdHJ1Y3Rpb246IG9wLmluc3RydWN0aW9uLFxuXHRcdFx0XHRhZGRyZXNzOiAoZCArIGNwdS54KSAmIDB4RkZGRixcblx0XHRcdFx0ZGF0YTogZFxuXHRcdFx0fTtcblx0XHRcdGJyZWFrIDtcblx0XHRjYXNlIFwiYWJzb2x1dGVZXCI6XG5cdFx0XHRkID0gcjE2KCk7XG5cdFx0XHRvdXRwdXQgPSB7XG5cdFx0XHRcdGluc3RydWN0aW9uOiBvcC5pbnN0cnVjdGlvbixcblx0XHRcdFx0YWRkcmVzczogKGQgKyBjcHUueSkgJiAweEZGRkYsXG5cdFx0XHRcdGRhdGE6IGRcblx0XHRcdH07XG5cdFx0XHRicmVhayA7XG5cdFx0Y2FzZSBcInJlbGF0aXZlXCI6XG5cdFx0XHRkID0gcjgoKTtcblx0XHRcdG91dHB1dCA9IHtcblx0XHRcdFx0aW5zdHJ1Y3Rpb246IG9wLmluc3RydWN0aW9uLFxuXHRcdFx0XHRhZGRyZXNzOiAoZCArIGFkZHJlc3MpICYgMHhGRkZGLFxuXHRcdFx0XHRkYXRhOiAoZCAmIDB4ODApID8gKGQgLSAweDEwMCkgOiBkXG5cdFx0XHR9O1xuXHRcdFx0YnJlYWsgO1xuXHRcdGRlZmF1bHQ6XG5cdFx0XHR0aHJvdyBuZXcgRXJyb3IoXCJVbmhhbmRsZWQgYWRkcmVzc2luZyBtb2RlOiBcIiArIG9wLmFkZHJlc3NpbmcpO1xuXHRcdH1cblxuXHRcdGlmIChwb3MgPT09IGNwdS5wYykgeyBvdXRwdXQuYWN0aXZlID0gdHJ1ZSA7IH1cblx0XHRvdXRwdXQucG9ydCA9IChwb3J0c1tvdXRwdXQuYWRkcmVzc10gfHwge30pLm5hbWU7XG5cdFx0b3V0cHV0Lm1vZGUgPSBvcC5hZGRyZXNzaW5nO1xuXHRcdG91dHB1dC5sb2NhdGlvbiA9IHBvcztcblxuXHRcdG91dHB1dC5ieXRlcyA9IFtdXG5cdFx0d2hpbGUgKHBvcyA8IGFkZHJlc3MpIHtcblx0XHRcdHZhciBpbnQgPSBjcHUucmVhZChwb3MrKykudG9TdHJpbmcoMTYpLnRvVXBwZXJDYXNlKCk7XG5cblx0XHRcdHdoaWxlIChpbnQubGVuZ3RoIDwgMikgeyBpbnQgPSBcIjBcIiArIGludDsgfVxuXG5cdFx0XHRvdXRwdXQuYnl0ZXMucHVzaChpbnQpO1xuXHRcdH1cblx0XHRvdXRwdXQuYnl0ZXMgPSBvdXRwdXQuYnl0ZXMuam9pbihcIiBcIik7XG5cblx0XHRpLnB1c2gob3V0cHV0KTtcblx0fVxuXG5cdHJldHVybiBpO1xufVxuXG5tb2R1bGUuZXhwb3J0cyA9ICB7XG5cdGRpc2Fzc2VtYmxlOiBkaXNhc3NlbWJsZVxufTtcbiIsInZhciBvYmplY3QgPSByZXF1aXJlKFwiLi4vLi4vdXRpbC9vYmplY3QuanNcIik7XG5cbnZhciBESVNBQkxFRCA9IDAsXG5cdENPTU1BTkQgPSAxLFxuXHRBRERSRVNTID0gMixcblx0UkVBRCA9IDMsXG5cdFdSSVRFID0gNDtcblxuZnVuY3Rpb24gZGVjb2RlKGRhdGEpIHtcblx0cmV0dXJuIGRhdGEubWF0Y2goLy4uL2cpLm1hcChmdW5jdGlvbih2KXtcblx0XHRyZXR1cm4gcGFyc2VJbnQodiwxNik7XG5cdH0pO1xufVxuXG5mdW5jdGlvbiBlbmNvZGUoZGF0YSkge1xuXHRyZXR1cm4gZGF0YS5tYXAoZnVuY3Rpb24gKHYpIHtcblx0XHRyZXR1cm4gKDB4MTAwIHwgdikudG9TdHJpbmcoMTYpLnN1YnN0cigxKTtcblx0fSkuam9pbihcIlwiKTtcbn1cblxuZnVuY3Rpb24gZWVwcm9tKGJpdF93aWR0aCkge1xuXHRiaXRfd2lkdGggfHwgKGJpdF93aWR0aCA9IDEyKTtcblx0dmFyIGJ5dGVfc2l6ZSA9IDEgPDwgYml0X3dpZHRoO1xuXG5cdC8vIEluaXRhbGl6ZSBlZXByb20gZGF0YSAoNGtCIGJ5IGRlZmF1bHQpXG5cdHRyeSB7XG5cdFx0dGhpcy5kYXRhID0gZGVjb2RlKHdpbmRvdy5sb2NhbFN0b3JhZ2UuZWVwcm9tX2RhdGEpO1xuXHR9IGNhdGNoKGUpIHtcblx0XHR0aGlzLmRhdGEgPSBvYmplY3QuZmlsbChieXRlX3NpemUsIDApO1xuXHR9XG5cblx0dGhpcy5hZGRyZXNzX3dpZHRoID0gTWF0aC5jZWlsKGJpdF93aWR0aCAvIDgpO1xuXHR0aGlzLm1hc2sgPSAoMSA8PCBiaXRfd2lkdGgpIC0gMTtcblxuXHR0aGlzLnVwZGF0ZShmYWxzZSk7XG59XG5cbmVlcHJvbS5wcm90b3R5cGUudXBkYXRlID0gZnVuY3Rpb24ocG93ZXIsIGNsaywgZGF0YSkge1xuXHQvLyBDb2Vyc2UgY2xrIC8gZGF0YSBsaW5lcyBpbnRvIGludGVnZXIgYm9vbGVhbnNcblx0Y2xrID0gY2xrID8gMSA6IDA7XG5cdGRhdGEgPSBkYXRhID8gMSA6IDA7XG5cblx0dmFyIGNsa19kID0gY2xrIC0gdGhpcy5sYXN0X2Nsayxcblx0XHRkYXRhX2QgPSBkYXRhIC0gdGhpcy5sYXN0X2RhdGE7XG5cblx0dGhpcy5sYXN0X3BvdyA9IHBvd2VyO1xuXHR0aGlzLmxhc3RfY2xrID0gY2xrO1xuXHR0aGlzLmxhc3RfZGF0YSA9IGRhdGE7XG5cblx0Ly8gVGhpcyBjaGlwIGlzIG5vdCByZWNlaXZpbmcgcG93ZXIsIHNvIGl0IGlzIGlkbGUuXG5cdGlmICghcG93ZXIpIHtcblx0XHR0aGlzLnN0YXRlID0gRElTQUJMRUQ7XG5cdFx0dGhpcy5vdXRwdXQgPSAxOyAvLyBOQUNLXG5cdFx0cmV0dXJuIDtcblx0fVxuXG5cdC8vIFRoZXJlIGhhcyBiZWVuIG5vIGJ1cyBjaGFuZ2UgKGlkbGUpXG5cdGlmICghY2xrX2QgJiYgIWRhdGFfZCkgeyByZXR1cm4gOyB9XG5cblx0Ly8gR2l2ZSBmcmllbmRseSB3YXJuaW5nIGFib3V0IHRoZSBob3N0IGJlaGF2aW5nIHBvb3JseVxuXHRpZiAoY2xrX2QgJiYgZGF0YV9kKSB7XG5cdFx0Y29uc29sZS5lcnJvcihcIldBUk5JTkc6IERhdGEgYW5kIGNsb2NrIGxpbmVzIGFyZSB0cmFuc2l0aW9uaW5nIGF0IHRoZSBzYW1lIHRpbWVcIik7XG5cdH1cblxuXHQvLyBEYXRhIHRyYW5zaXRpb24gd2hpbGUgQ0xLIGlzIGhpZ2hcblx0aWYgKGNsayAmJiBkYXRhX2QpIHtcblx0XHRpZiAoZGF0YV9kID4gMCkgeyBcblx0XHRcdGlmICh0aGlzLnN0YXRlID09PSBXUklURSAmJiB3aW5kb3cubG9jYWxTdG9yYWdlKSB7XG5cdFx0XHRcdHdpbmRvdy5sb2NhbFN0b3JhZ2UuZWVwcm9tX2RhdGEgPSBlbmNvZGUodGhpcy5kYXRhKTtcblx0XHRcdH1cblxuXHRcdFx0Ly8gU3RvcFxuXHRcdFx0dGhpcy5zdGF0ZSA9IERJU0FCTEVEO1xuXHRcdFx0dGhpcy5vdXRwdXQgPSAwO1xuXHRcdH0gZWxzZSB7XG5cdFx0XHQvLyBTdGFydFxuXHRcdFx0dGhpcy5zdGF0ZSA9IENPTU1BTkQ7XG5cdFx0XHR0aGlzLm91dHB1dCA9IDA7XG5cblx0XHRcdHRoaXMuYml0c190eCA9IDA7XG5cdFx0XHR0aGlzLnJlYWQgPSAwO1xuXHRcdH1cblx0fVxuXG5cdC8vIFdlIGFyZSBub3QgcHJvY2Vzc2luZyBhbnkgZGF0YSByaWdodCBub3dcblx0aWYgKHRoaXMuc3RhdGUgPT09IERJU0FCTEVEKSB7IHJldHVybiA7IH1cblxuXHRpZiAoY2xrX2QgPiAwKSB7XG5cdFx0Ly8gUmlzaW5nIGVkZ2UgY2xvY2sgKGlucHV0KVxuXHRcdHRoaXMucmVhZCA9ICgodGhpcy5yZWFkIDw8IDEpICYgMHhGRikgfCAoZGF0YSA/IDEgOiAwKTtcblx0fSBlbHNlIGlmIChjbGtfZCA8IDApIHtcblx0XHQvLyBGYWxsaW5nIGVkZ2UgKGRlbGl2ZXJ5KVxuXHRcdGlmICh0aGlzLmJpdHNfdHggPCA4KSB7XG5cdFx0XHQvLyBTaW1wbHkgdXBkYXRlIG91dHB1dCBidWZmZXJcblx0XHRcdGlmICh0aGlzLnN0YXRlID09PSBSRUFEKSB7XG5cdFx0XHRcdHRoaXMub3V0cHV0ID0gKCh0aGlzLmRhdGFbdGhpcy5hZGRyZXNzXSA8PCB0aGlzLmJpdHNfdHgpICYgMHg4MCkgPyAxIDogMDtcblx0XHRcdH0gZWxzZSB7XG5cdFx0XHRcdHRoaXMub3V0cHV0ID0gMTtcblx0XHRcdH1cblx0XHR9IGVsc2UgaWYgKHRoaXMuYml0c190eCA9PT0gOCkge1xuXHRcdFx0dGhpcy5vdXRwdXQgPSAwOyAvLyBBQ0tcblxuXHRcdFx0Ly8gV2UgaGF2ZSByZWNlaXZlZCBhIGZ1bGwgY29tbWFuZCAvIG91dHB1dCBhIHZhbHVlXG5cdFx0XHRzd2l0Y2ggKHRoaXMuc3RhdGUpIHtcblx0XHRcdGNhc2UgQ09NTUFORDpcblx0XHRcdFx0c3dpdGNoKHRoaXMucmVhZCAmIDB4RjEpIHtcblx0XHRcdFx0Y2FzZSAweEEwOiAvLyBXcml0ZVxuXHRcdFx0XHRcdHRoaXMuc3RhdGUgPSBBRERSRVNTO1xuXHRcdFx0XHRcdHRoaXMuYWRkcmVzc2J5dGUgPSAwO1xuXHRcdFx0XHRcdHRoaXMuYWRkcmVzcyA9IDA7XG5cdFx0XHRcdFx0YnJlYWsgO1xuXHRcdFx0XHRjYXNlIDB4QTE6IC8vIFJlYWRcblx0XHRcdFx0XHR0aGlzLnN0YXRlID0gUkVBRDtcblx0XHRcdFx0XHRicmVhayA7XG5cdFx0XHRcdGRlZmF1bHQ6XG5cdFx0XHRcdFx0dGhpcy5vdXRwdXQgPSAxOyAvLyBOQUNLXG5cdFx0XHRcdFx0YnJlYWsgO1xuXHRcdFx0XHR9XG5cdFx0XHRcdGJyZWFrIDtcblx0XHRcdGNhc2UgQUREUkVTUzpcblx0XHRcdFx0Ly8gVXBkYXRlIGFkZHJlc3Ncblx0XHRcdFx0dGhpcy5hZGRyZXNzID0gKHRoaXMuYWRkcmVzcyA8PCA4KSB8IHRoaXMucmVhZDtcblx0XHRcdFx0aWYgKCsrdGhpcy5hZGRyZXNzYnl0ZSA+PSB0aGlzLmFkZHJlc3Nfd2lkdGgpIHtcblx0XHRcdFx0XHR0aGlzLnN0YXRlID0gV1JJVEU7XG5cdFx0XHRcdH1cblx0XHRcdFx0YnJlYWsgO1xuXHRcdFx0Y2FzZSBXUklURTpcblx0XHRcdFx0dGhpcy5kYXRhW3RoaXMuYWRkcmVzc10gPSB0aGlzLnJlYWQgJiAweEZGO1xuXHRcdFx0XHR0aGlzLmFkZHJlc3MgPSAodGhpcy5hZGRyZXNzICsgMSkgJiB0aGlzLm1hc2s7XG5cdFx0XHRcdGJyZWFrIDtcblx0XHRcdGNhc2UgUkVBRDpcblx0XHRcdFx0dGhpcy5hZGRyZXNzID0gKHRoaXMuYWRkcmVzcyArIDEpICYgdGhpcy5tYXNrO1xuXHRcdFx0XHRicmVhayA7XG5cdFx0XHR9XG5cdFx0fVxuXG5cdFx0Ly8gSW5jcmVtZW50IGJpdCBjbG9ja1xuXHRcdHRoaXMuYml0c190eCA9ICh0aGlzLmJpdHNfdHggKyAxKSAlIDk7XG5cdH1cbn1cblxubW9kdWxlLmV4cG9ydHMgPSB7XG5cdGVlcHJvbTogZWVwcm9tXG59O1xuIiwiLyoqXG4gKiogVGhlc2Ugb3BlcmF0ZSBvbiB0aGUgZWZmZWN0aXZlIGFkZHJlc3NcbiAqKi9cbmZ1bmN0aW9uIHNldF9ueihjcHUsIGQpIHtcbiAgICBjcHUubiA9IGQgJiAweDgwO1xuICAgIGNwdS56ID0gIWQ7XG59XG5cbm1vZHVsZS5leHBvcnRzID0gIHtcbiAgICAvLyBPdGhlcih2ZXJpZmllZClcbiAgICBOT1A6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHt9LFxuXG4gICAgLy8gRmxhZ3ModmVyaWZpZWQpXG4gICAgQ0xDOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIGNwdS5jID0gMDtcbiAgICB9LFxuICAgIENMSTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBjcHUuaSA9IDA7XG4gICAgfSxcbiAgICBDTFY6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgY3B1LnYgPSAwO1xuICAgIH0sXG4gICAgQ0xEOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIGNwdS5kID0gMDtcbiAgICB9LFxuICAgIFNFQzogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBjcHUuYyA9IDE7XG4gICAgfSxcbiAgICBTRUQ6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgY3B1LmQgPSAxO1xuICAgIH0sXG4gICAgU0VJOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIGNwdS5pID0gMTtcbiAgICB9LFxuXG4gICAgLy8gVHJhbnNmZXIgKHZlcmlmaWVkKVxuICAgIFRYQTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBzZXRfbnooY3B1LCBjcHUuYSA9IGNwdS54KTtcbiAgICB9LFxuICAgIFRZQTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBzZXRfbnooY3B1LCBjcHUuYSA9IGNwdS55KTtcbiAgICB9LFxuICAgIFRYUzogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBjcHUucyA9IGNwdS54O1xuICAgIH0sXG4gICAgVEFYOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS54ID0gY3B1LmEpO1xuICAgIH0sXG4gICAgVEFZOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS55ID0gY3B1LmEpO1xuICAgIH0sXG4gICAgVFNYOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS54ID0gY3B1LnMpO1xuICAgIH0sXG4gICAgTERBOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS5hID0gY3B1LnJlYWQoYWRkcikpO1xuICAgIH0sXG4gICAgTERYOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS54ID0gY3B1LnJlYWQoYWRkcikpO1xuICAgIH0sXG4gICAgTERZOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS55ID0gY3B1LnJlYWQoYWRkcikpO1xuICAgIH0sXG4gICAgU1RBOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIGNwdS53cml0ZShhZGRyLCBjcHUuYSk7XG4gICAgfSxcbiAgICBTVFg6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgY3B1LndyaXRlKGFkZHIsIGNwdS54KTtcbiAgICB9LFxuICAgIFNUWTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBjcHUud3JpdGUoYWRkciwgY3B1LnkpO1xuICAgIH0sXG5cbiAgICAvLyBCaXQgT3BlcmF0aW9ucyAodmVyaWZpZWQpXG4gICAgQklUOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHZhciBkYXRhID0gY3B1LnJlYWQoYWRkcik7XG4gICAgICAgIGNwdS5uID0gZGF0YSAmIDB4ODA7XG4gICAgICAgIGNwdS52ID0gZGF0YSAmIDB4NDA7XG4gICAgICAgIGNwdS56ID0gIShjcHUuYSAmIGRhdGEpO1xuICAgIH0sXG4gICAgT1JBOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS5hIHw9IGNwdS5yZWFkKGFkZHIpKTtcbiAgICB9LFxuICAgIEVPUjogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBzZXRfbnooY3B1LCBjcHUuYSBePSBjcHUucmVhZChhZGRyKSk7XG4gICAgfSxcbiAgICBBTkQ6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgc2V0X256KGNwdSwgY3B1LmEgJj0gY3B1LnJlYWQoYWRkcikpO1xuICAgIH0sXG5cbiAgICAvLyBTaGlmdGVyXG4gICAgQVNMOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHZhciBkYXRhID0gY3B1LnJlYWQoYWRkciksXG4gICAgICAgICAgICBvdXQgPSAoZGF0YSA8PCAxKSAmIDB4RkY7XG5cbiAgICAgICAgY3B1LmMgPSBkYXRhICYgMHg4MDtcblxuICAgICAgICBzZXRfbnooY3B1LCBvdXQpO1xuICAgICAgICBjcHUud3JpdGUoYWRkciwgb3V0KTtcbiAgICB9LFxuICAgIExTUjogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICB2YXIgZGF0YSA9IGNwdS5yZWFkKGFkZHIpLFxuICAgICAgICAgICAgb3V0ID0gZGF0YSA+PiAxO1xuXG4gICAgICAgIGNwdS5jID0gZGF0YSAmIDB4MDE7XG5cbiAgICAgICAgc2V0X256KGNwdSwgb3V0KTtcbiAgICAgICAgY3B1LndyaXRlKGFkZHIsIG91dCk7XG4gICAgfSxcblxuICAgIFJPTDogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICB2YXIgZGF0YSA9IGNwdS5yZWFkKGFkZHIpLFxuICAgICAgICAgICAgb3V0ID0gKChkYXRhIDw8IDEpICYgMHhGRikgfCAoY3B1LmMgPyAxIDogMCk7XG5cbiAgICAgICAgY3B1LmMgPSBkYXRhICYgMHg4MDtcblxuICAgICAgICBzZXRfbnooY3B1LCBvdXQpO1xuICAgICAgICBjcHUud3JpdGUoYWRkciwgb3V0KTtcbiAgICB9LFxuXG4gICAgUk9SOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHZhciBkYXRhID0gY3B1LnJlYWQoYWRkciksXG4gICAgICAgICAgICBvdXQgPSAoZGF0YSA+PiAxKSB8IChjcHUuYyA/IDB4ODAgOiAwKTtcblxuICAgICAgICBjcHUuYyA9IGRhdGEgJiAweDAxO1xuXG4gICAgICAgIHNldF9ueihjcHUsIG91dCk7XG4gICAgICAgIGNwdS53cml0ZShhZGRyLCBvdXQpO1xuICAgIH0sXG5cbiAgICAvLyBBcml0aG1hdGljXG4gICAgREVDOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHZhciBkYXRhID0gKGNwdS5yZWFkKGFkZHIpIC0gMSkgJiAweEZGO1xuICAgICAgICBzZXRfbnooY3B1LCBkYXRhKTtcbiAgICAgICAgY3B1LndyaXRlKGFkZHIsIGRhdGEpXG4gICAgfSxcbiAgICBERVg6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgc2V0X256KGNwdSwgY3B1LnggPSAoY3B1LnggLSAxKSAmIDB4RkYpO1xuICAgIH0sXG4gICAgREVZOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS55ID0gKGNwdS55IC0gMSkgJiAweEZGKTtcbiAgICB9LFxuICAgIElOQzogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICB2YXIgZGF0YSA9IChjcHUucmVhZChhZGRyKSArIDEpICYgMHhGRjtcbiAgICAgICAgc2V0X256KGNwdSwgZGF0YSk7XG4gICAgICAgIGNwdS53cml0ZShhZGRyLCBkYXRhKVxuICAgIH0sXG4gICAgSU5YOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIHNldF9ueihjcHUsIGNwdS54ID0gKGNwdS54ICsgMSkgJiAweEZGKTtcbiAgICB9LFxuICAgIElOWTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBzZXRfbnooY3B1LCBjcHUueSA9IChjcHUueSArIDEpICYgMHhGRik7XG4gICAgfSxcbiAgICBBREM6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgdmFyIGRhdGEgPSBjcHUucmVhZChhZGRyKSxcbiAgICAgICAgICAgIG8gPSBjcHUuYSArIGRhdGEgKyAoY3B1LmMgPyAxIDogMCk7XG5cbiAgICAgICAgY3B1LnYgPSB+KGNwdS5hIF4gZGF0YSkgJiAoY3B1LmEgXiBvKSAmIDB4ODA7XG4gICAgICAgIHNldF9ueihjcHUsIG8gJiAweEZGKTtcblxuICAgICAgICBpZiAoY3B1LmQpIHtcbiAgICAgICAgICAgIHZhciBhbCA9IChjcHUuYSAmIDB4MEYpICsgKGRhdGEgJiAweDBGKSArIChjcHUuYyA/IDEgOiAwKSxcbiAgICAgICAgICAgICAgICBhaCA9IChjcHUuYSAmIDB4RjApICsgKGRhdGEgJiAweEYwKSArICgoYWwgPj0gMHgxMCkgPyAweDEwIDogMCk7XG4gICAgICAgICAgICBcbiAgICAgICAgICAgIC8vIERlY2ltYWwgbW9kZSBmaXh1cFxuICAgICAgICAgICAgaWYgKGFsID4gMHgwOSkgeyBhbCArPSAweDA2OyB9XG4gICAgICAgICAgICBpZiAoYWggPiAweDkwKSB7IGFoICs9IDB4NjA7IH1cblxuICAgICAgICAgICAgLy8gV2UgZml4ZWQgdXAgdGhlIGRlY2ltYWwsIHJlY29tYmluZVxuICAgICAgICAgICAgbyA9IChhbCAmIDB4MEYpICsgYWg7IFxuICAgICAgICB9XG5cbiAgICAgICAgY3B1LmMgPSBvICYgfjB4RkY7XG5cbiAgICAgICAgY3B1LmEgPSBvICYgMHhGRjtcbiAgICB9LFxuICAgIFNCQzogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICB2YXIgZGF0YSA9IGNwdS5yZWFkKGFkZHIpLFxuICAgICAgICAgICAgbyA9IGNwdS5hIC0gZGF0YSAtIChjcHUuYyA/IDAgOiAxKTtcblxuICAgICAgICAvLyBBbGwgZmxhZ3MgYXJlIGxpa2UgYmluYXJ5IG1vZGVcbiAgICAgICAgY3B1LnYgPSAoY3B1LmEgXiBkYXRhKSAmIChjcHUuYSBeIG8pICYgMHg4MDtcbiAgICAgICAgc2V0X256KGNwdSwgbyAmIDB4RkYpO1xuICAgICAgICBjcHUuYyA9ICEobyAmIH4weEZGKTtcblxuICAgICAgICBpZiAoY3B1LmQpIHtcbiAgICAgICAgICAgIHZhciBhbCA9IChjcHUuYSAmIDB4MEYpIC0gKGRhdGEgJiAweDBGKSAtIChjcHUuYyA/IDAgOiAxKSxcbiAgICAgICAgICAgICAgICBhaCA9IChjcHUuYSAmIDB4RjApIC0gKGRhdGEgJiAweEYwKSAtICgoYWwgPCAwKSA/IDB4MTAgOiAwKTtcblxuICAgICAgICAgICAgLy8gQ2FsY3VsYXRlIGZpeCB1cCBkZWNpbWFsIG1vZGVcbiAgICAgICAgICAgIGlmIChhbCA8IDB4MDApIHsgYWwgLT0gMHgwNjsgfVxuICAgICAgICAgICAgaWYgKGFoIDwgMHgwMCkgeyBhaCAtPSAweDYwOyB9XG5cbiAgICAgICAgICAgIG8gPSAoYWwgJiAweDBGKSArIGFoO1xuICAgICAgICB9XG5cbiAgICAgICAgY3B1LmEgPSBvICYgMHhGRjtcbiAgICB9LFxuICAgIENNUDogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICB2YXIgZGF0YSA9IGNwdS5yZWFkKGFkZHIpO1xuXG4gICAgICAgIGNwdS5jID0gY3B1LmEgPj0gZGF0YTtcbiAgICAgICAgc2V0X256KGNwdSwgKGNwdS5hIC0gZGF0YSkgJiAweEZGKTtcbiAgICB9LFxuICAgIENQWDogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICB2YXIgZGF0YSA9IGNwdS5yZWFkKGFkZHIpO1xuXG4gICAgICAgIGNwdS5jID0gY3B1LnggPj0gZGF0YTtcbiAgICAgICAgc2V0X256KGNwdSwgKGNwdS54IC0gZGF0YSkgJiAweEZGKTtcbiAgICB9LFxuICAgIENQWTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICB2YXIgZGF0YSA9IGNwdS5yZWFkKGFkZHIpO1xuXG4gICAgICAgIGNwdS5jID0gY3B1LnkgPj0gZGF0YTtcbiAgICAgICAgc2V0X256KGNwdSwgKGNwdS55IC0gZGF0YSkgJiAweEZGKTtcbiAgICB9LFxuXG4gICAgLy8gU3RhY2sgT3BlcmF0aW9uc1xuICAgIFBIUDogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBjcHUucHVzaChjcHUucCB8IDB4MTApO1xuICAgIH0sXG4gICAgUEhBOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIGNwdS5wdXNoKGNwdS5hKTtcbiAgICB9LFxuICAgIFBMQTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBzZXRfbnooY3B1LCBjcHUuYSA9IGNwdS5wdWxsKCkpO1xuICAgIH0sXG4gICAgUExQOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIGNwdS5wID0gY3B1LnB1bGwoKTtcbiAgICB9LFxuXG4gICAgLy8gSW50ZXJydXB0cyAvIEJyYW5jaCAodmVyaWZpZWQpXG4gICAgSk1QOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIGNwdS5wYyA9IGFkZHI7XG4gICAgfSxcbiAgICBKU1I6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgY3B1LnBjID0gKGNwdS5wYyAtIDEpICYgMHhGRkZGO1xuICAgICAgICBjcHUucHVzaChjcHUucGMgPj4gOCk7XG4gICAgICAgIGNwdS5wdXNoKGNwdS5wYyAmIDB4RkYpO1xuICAgICAgICBjcHUucGMgPSBhZGRyO1xuICAgIH0sXG4gICAgUlRJOiBmdW5jdGlvbiAoY3B1LCBhZGRyKSB7XG4gICAgICAgIGNwdS5wID0gY3B1LnB1bGwoKTtcbiAgICAgICAgY3B1LnBjID0gY3B1LnB1bGwoKTtcbiAgICAgICAgY3B1LnBjIHw9IGNwdS5wdWxsKCkgPDwgODtcbiAgICB9LFxuICAgIFJUUzogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBjcHUucGMgPSBjcHUucHVsbCgpO1xuICAgICAgICBjcHUucGMgfD0gY3B1LnB1bGwoKSA8PCA4O1xuICAgICAgICBjcHUucGMgPSAoY3B1LnBjICsgMSkgJiAweEZGRkY7XG4gICAgfSxcbiAgICBCUks6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgLy8gVGhpcyBzaG91bGQgcHJvYmFibHkgYWN0dWFsbHkgZmluZCBvdXQgd2hpY2ggSVJRIHRvIHNlcnZpY2VcbiAgICAgICAgY3B1LnBjID0gKGNwdS5wYyArIDEpICYgMHhGRkZGO1xuICAgICAgICBjcHUuaXJxKHRydWUpO1xuICAgIH0sXG5cbiAgICBCTkU6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgaWYgKCFjcHUueikgY3B1LnBjID0gYWRkcjtcbiAgICB9LFxuICAgIEJFUTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBpZiAoY3B1LnopIGNwdS5wYyA9IGFkZHI7XG4gICAgfSxcbiAgICBCUEw6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgaWYgKCFjcHUubikgY3B1LnBjID0gYWRkcjtcbiAgICB9LFxuICAgIEJNSTogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBpZiAoY3B1Lm4pIGNwdS5wYyA9IGFkZHI7XG4gICAgfSxcbiAgICBCQ0M6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgaWYgKCFjcHUuYykgY3B1LnBjID0gYWRkcjtcbiAgICB9LFxuICAgIEJDUzogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBpZiAoY3B1LmMpIGNwdS5wYyA9IGFkZHI7XG4gICAgfSxcbiAgICBCVkM6IGZ1bmN0aW9uIChjcHUsIGFkZHIpIHtcbiAgICAgICAgaWYgKCFjcHUudikgY3B1LnBjID0gYWRkcjtcbiAgICB9LFxuICAgIEJWUzogZnVuY3Rpb24gKGNwdSwgYWRkcikge1xuICAgICAgICBpZiAoY3B1LnYpIGNwdS5wYyA9IGFkZHI7XG4gICAgfVxufTtcbiIsInZhciBwb3J0cyA9IHJlcXVpcmUoXCIuLi9kYXRhL3BvcnRzLmpzXCIpLFxuXHRcdG9iamVjdCA9IHJlcXVpcmUoXCIuLi8uLi91dGlsL29iamVjdC5qc1wiKTtcblxuLy8gPT09PSBCYW5rIFN3aXRjaCA9PT09XG5mdW5jdGlvbiB3cml0ZV9iYW5rKHJlZywgdmFsdWUpIHtcblx0dGhpcy5fY3B1cmVnW3JlZ10gPSB2YWx1ZTtcblx0dGhpcy5zZXRfcm9tX3BhZ2UodmFsdWUpO1xufVxuXG4vLyA9PT09IElSUSBMb2dpYyA9PT1cbmZ1bmN0aW9uIHdyaXRlX2ludF9mbGFnKHJlZywgdmFsdWUpIHtcblx0dGhpcy5fY3B1cmVnW3JlZ10gJj0gfnZhbHVlO1xufVxuXG4vLyA9PT09IFBvcnRBID09PT1cbmZ1bmN0aW9uIHdyaXRlX3BvcnRhX2Rpcl9kYXRhKHJlZywgdmFsdWUpIHtcblx0dGhpcy5fY3B1cmVnW3JlZ10gPSB2YWx1ZTtcblx0Ly8gbm8gd3JpdGVzIHlldC5cbn1cblxuZnVuY3Rpb24gcmVhZF9wb3J0YV9kYXRhKHJlZywgdmFsdWUpIHtcblx0dmFyIG1hc2sgPSB0aGlzLl9jcHVyZWdbMHgxMV0sXG5cdFx0dmFsdWUgPSB0aGlzLl9jcHVyZWdbMHgxMl0sXG5cdFx0c3BpX3Bvd2VyID0gbWFzayAmIHZhbHVlICYgMHgxMCxcblx0XHRpbnB1dCA9IHRoaXMua2V5cyB8IFxuXHRcdFx0XHQoKHNwaV9wb3dlciA/IDAgOiB0aGlzLmluc2VydGVkX2ZpZ3VyZSkgPDwgNSk7XG5cblx0cmV0dXJuIChtYXNrICYgdmFsdWUpIHwgKH5tYXNrICYgaW5wdXQpO1xufVxuXG4vLyA9PT09IFBvcnRCID09PT1cbmZ1bmN0aW9uIHdyaXRlX3BvcnRiX2Rpcl9kYXRhKHJlZywgdmFsdWUpIHtcblx0dGhpcy5fY3B1cmVnW3JlZ10gPSB2YWx1ZTtcblxuXHR2YXIgbWFzayA9IHRoaXMuX2NwdXJlZ1sweDE1XSxcblx0XHRkID0gfm1hc2sgfCB0aGlzLl9jcHVyZWdbMHgxNl07XHQvLyBWYWx1ZXMgYXJlIHB1bGxlZCB1cFxuXG5cdHRoaXMuX2VlcHJvbS51cGRhdGUoZCY0LCBkJjIsIGQmMSk7XG59XG5cbmZ1bmN0aW9uIHJlYWRfcG9ydGJfZGF0YShyZWcsIHZhbHVlKSB7XG5cdHZhciBtYXNrID0gdGhpcy5fY3B1cmVnWzB4MTVdLFxuXHRcdGlucHV0ID0gKHRoaXMuX2VlcHJvbS5vdXRwdXQgPyAxIDogMCk7XG5cdFxuXHRyZXR1cm4gKG1hc2sgJiB0aGlzLl9jcHVyZWdbMHgxNl0pIHwgKH5tYXNrICYgaW5wdXQpO1xufVxuXG4vLyAtLS0gUkVHSVNURVIgTEFZT1VUIC0tLVxuZnVuY3Rpb24gcGFkKHMsIGwpIHtcblx0cmV0dXJuIFwiMDAwMDAwMDBcIi5zdWJzdHIoMCwgbCkuc3Vic3RyKHMubGVuZ3RoKSArIHM7XG59XG5cbi8vIERlZmF1bHQgcmVnaXN0ZXIgYWN0aW9uc1xuZnVuY3Rpb24gdW5kZWZfcmVhZChyZWcpIHtcblx0Y29uc29sZS5sb2coXG5cdFx0cGFkKHRoaXMuX2NwdXJlZ1swXS50b1N0cmluZygxNiksIDIpLFxuXHRcdHRoaXMucGMudG9TdHJpbmcoMTYpLFxuXHRcdFwiVW5oYW5kbGVkIHJlZ2lzdGVyIHJlYWQgIChcIiArICgweDMwMDArcmVnKS50b1N0cmluZygxNikgKyBcIilcIiwgXG5cdFx0XCIgICAgICAgICAgICAgXCIsIFxuXHRcdChwb3J0c1tyZWd8MHgzMDAwXSB8fCB7fSkubmFtZSB8fCBcIi0tLVwiKTtcblxuXHRpZiAocmVnID09IDB4QjcpIHJldHVybiAweEZGO1xuXG5cdHJldHVybiB0aGlzLl9jcHVyZWdbcmVnXTtcbn1cblxuZnVuY3Rpb24gdW5kZWZfd3JpdGUocmVnLCBkYXRhKSB7XG5cdGNvbnNvbGUubG9nKFxuXHRcdHBhZCh0aGlzLl9jcHVyZWdbMF0udG9TdHJpbmcoMTYpLCAyKSxcdFx0XHRcdFx0XG5cdFx0dGhpcy5wYy50b1N0cmluZygxNiksXG5cdFx0XCJVbmhhbmRsZWQgcmVnaXN0ZXIgd3JpdGUgKFwiICsgKDB4MzAwMCtyZWcpLnRvU3RyaW5nKDE2KSArIFwiKVwiLCBcblx0XHRwYWQoZGF0YS50b1N0cmluZygxNiksMiksIFxuXHRcdFwiLVwiLCBcblx0XHRwYWQoZGF0YS50b1N0cmluZygyKSwgOCksIFxuXHRcdChwb3J0c1tyZWd8MHgzMDAwXSB8fCB7fSkubmFtZSB8fCBcIi0tLVwiKTtcblx0dGhpcy5fY3B1cmVnW3JlZ10gPSBkYXRhO1xufVxuXG52YXIgcmVnaXN0ZXJfbGF5b3V0ID0ge1xuXHQweDAwOiB7IHdyaXRlOiB3cml0ZV9iYW5rIH0sXG5cdDB4MDE6IHt9LCAvLyBTSUxFTkNFXG5cdDB4MDQ6IHt9LCAvLyBTSUxFTkNFXG5cdDB4MzE6IHt9LCAvLyBTSUxFTkNFXG5cblx0Ly8gLS0tIERBVEEgUG9ydHNcblx0MHgxMDoge30sIC8vIFNJTEVOQ0UgQ09ORklHXG5cdDB4MTE6IHsgd3JpdGU6IHdyaXRlX3BvcnRhX2Rpcl9kYXRhIH0sXG5cdDB4MTI6IHsgd3JpdGU6IHdyaXRlX3BvcnRhX2Rpcl9kYXRhLCByZWFkOiByZWFkX3BvcnRhX2RhdGEgfSxcblx0MHgxNDoge30sIC8vIFNJTEVOQ0UgQ09ORklHXG5cdDB4MTU6IHsgd3JpdGU6IHdyaXRlX3BvcnRiX2Rpcl9kYXRhIH0sXG5cdDB4MTY6IHsgd3JpdGU6IHdyaXRlX3BvcnRiX2Rpcl9kYXRhLCByZWFkOiByZWFkX3BvcnRiX2RhdGEgfSxcblxuXHQvLyAtLS0gSVJRIEJsb2NrXG5cdDB4NzA6IHt9LCAvLyBJUlEgRW5hYmxlcyBhcmUgbm9ybWFsIFxuXHQweDcxOiB7fSwgLy8gSVJRIEVuYWJsZXMgYXJlIG5vcm1hbCBcblx0MHg3MzogeyB3cml0ZTogd3JpdGVfaW50X2ZsYWcgfSxcblx0MHg3NDogeyB3cml0ZTogd3JpdGVfaW50X2ZsYWcgfSxcblx0MHg3Njoge30sIC8vIE5NSSBFbmFibGVzIGFyZSBub3JtYWxcbn0sIHVuZGVmX3JlZ2lzdGVyID0ge1xuXHRyZWFkOiB1bmRlZl9yZWFkLCBcblx0d3JpdGU6IHVuZGVmX3dyaXRlIFxufTtcblxubW9kdWxlLmV4cG9ydHMgPSB7XG5cdG1hcF9yZWdpc3RlcnM6IGZ1bmN0aW9uICgpIHtcblx0XHQvLyBTdGFydCBtYXBwaW5nIG91dCByZWdpc3RlcnNcblx0XHRmb3IgKHZhciBpID0gMDsgaSA8IDB4MTAwOyBpKyspIHtcblx0XHRcdC8vIFRoaXMgaXMgbm9ybWFsbHkgY29uc2lkZXJlZCBkYW5nZXJvdXMsIGJ1dCBJIG5lZWQgdGhlIGNsb3N1cmVcblx0XHRcdH5mdW5jdGlvbiAoKSB7XG5cdFx0XHRcdHZhciBsYXlvdXQgPSByZWdpc3Rlcl9sYXlvdXRbaV0gfHwgdW5kZWZfcmVnaXN0ZXIsXG5cdFx0XHRcdFx0cmVhZCAgID0gbGF5b3V0LnJlYWQgfHwgZnVuY3Rpb24gKHJlZykgeyByZXR1cm4gdGhpcy5fY3B1cmVnW3JlZ107IH0sXG5cdFx0XHRcdFx0d3JpdGUgID0gbGF5b3V0LndyaXRlIHx8IGZ1bmN0aW9uIChyZWcsIGRhdGEpIHsgdGhpcy5fY3B1cmVnW3JlZ10gPSBkYXRhOyB9O1xuXG5cdFx0XHRcdC8vIE1hcCByZWdpc3RlcnMgdG8gdGhlaXIgbWlycm9ycyBhcyB3ZWxsXG5cdFx0XHRcdGZvciAodmFyIGEgPSAweDMwMDA7IGEgPCAweDQwMDA7IGEgKz0gMHgxMDApIHtcblx0XHRcdFx0XHR0aGlzLl9yZWFkYmFua1thK2ldID0gcmVhZDtcblx0XHRcdFx0XHR0aGlzLl93cml0ZWJhbmtbYStpXSA9IHdyaXRlO1xuXHRcdFx0XHR9XG5cdFx0XHR9LmNhbGwodGhpcyk7XG5cdFx0fVxuXHR9XG59O1xuIiwiXG52YXIgcjY1MDIgPSByZXF1aXJlKFwiLi82NTAyLmpzXCIpLFxuXHRcdGVlcHJvbSA9IHJlcXVpcmUoXCIuL2VlcHJvbS5qc1wiKSxcblx0XHRyZWdpc3RlcnMgPSByZXF1aXJlKFwiLi9yZWdpc3RlcnMuanNcIiksXG5cdFx0b2JqZWN0ID0gcmVxdWlyZShcIi4uLy4uL3V0aWwvb2JqZWN0LmpzXCIpO1xuXG52YXIgQUNDRVNTX1JFQURcdFx0PSAweDAxLFxuXHRcdEFDQ0VTU19XUklURVx0PSAweDAyO1xuXG5mdW5jdGlvbiBzeXN0ZW0oKSB7XG5cdHZhciB0aGF0ID0gdGhpcztcblxuXHR0aGlzLl9yZWFkYmFuayA9IG5ldyBBcnJheSgweDEwMDAwKTtcblx0dGhpcy5fd3JpdGViYW5rID0gbmV3IEFycmF5KDB4MTAwMDApO1xuXG5cdHRoaXMuX2NwdWFjYyA9IG5ldyBVaW50OEFycmF5KDB4MTAwMDApO1x0XHQvLyBBY2Nlc3MgZmxhZ3NcblxuXHR0aGlzLl9jcHVyZWcgPSBuZXcgVWludDhBcnJheSgweDEwMCk7XHRcdC8vIENvbnRyb2wgcmVnaXN0ZXJzXG5cdHRoaXMuX2RyYW0gICA9IG5ldyBVaW50OEFycmF5KDB4MjAwKTtcdFx0Ly8gRGlzcGxheSBtZW1vcnlcblx0dGhpcy5fd3JhbVx0ID0gbmV3IFVpbnQ4QXJyYXkoMHg2MDApO1x0XHQvLyBTeXN0ZW0gbWVtb3J5XG5cdHRoaXMuX2VlcHJvbSA9IG5ldyBlZXByb20uZWVwcm9tKDEyKTtcdFx0Ly8gbmV3IDMya2IgZWVwcm9tXG5cdHRoaXMuX2lycXMgPSBuZXcgVWludDE2QXJyYXkoMHgxMDAwMCk7XG5cblx0dGhpcy5rZXlzXHQgPSAweEY7XG5cblx0Ly8gQ29udmVydCBhIDE2Yml0IG1hc2sgaW50byBhIHByaW9yaXR5IGVuY29kZWQgSVJRIHRhYmxlXG5cdHZhciBpcnFzID0gbmV3IFVpbnQxNkFycmF5KHRoaXMuYmlvcywgMHgzRkMwLCAxNik7XG5cdGZvciAodmFyIGkgPSAwOyBpIDwgdGhpcy5faXJxcy5sZW5ndGg7IGkrKykge1xuXHRcdHRoaXMuX2lycXNbaV0gPSBpcnFzWzE1IC0gTWF0aC5mbG9vcihpID8gKE1hdGgubG9nKGkpIC8gTWF0aC5sb2coMikpIDogMCldO1xuXHR9XG5cblx0Ly8gQ29uZmlndXJlIGFuZCByZXNldFxuXHR0aGlzLmluaXQoKTtcblx0dGhpcy5yZXNldCgpO1xuXG5cdHRoaXMucHJldmlvdXNfY2xvY2sgPSAwO1xuXHR0aGlzLmluc2VydGVkX2ZpZ3VyZSA9IDA7XG5cdFxuXHR0aGlzLl90YmhfdGltZXIgPSAwOyBcdC8vIEhBQ0tcbn1cblxuc3lzdGVtLnByb3RvdHlwZSA9IE9iamVjdC5jcmVhdGUocjY1MDIucjY1MDIpO1x0XG5vYmplY3QuZXh0ZW5kKHN5c3RlbS5wcm90b3R5cGUsIHJlZ2lzdGVycyk7XG5cbnN5c3RlbS5wcm90b3R5cGUuUEFMRVRURSA9IFsweGZmZGRkZGRkLCAweGZmOWU5ZTllLCAweGZmNjA2MDYwLCAweGZmMjIyMjIyXTtcblxuc3lzdGVtLnByb3RvdHlwZS5DTE9DS19SQVRFID0gNDAwMDAwMDsgLy8gNE1Ielxuc3lzdGVtLnByb3RvdHlwZS5NQVhfQURWQU5DRSA9IDE7XG5zeXN0ZW0ucHJvdG90eXBlLkxDRF9PUkRFUiA9IFtcblx0MHgwQzAsIDB4MENDLCAweDBEOCwgMHgwRTQsIFxuXHQweDBGMCwgMHgwRkMsIDB4MTA4LCAweDExNCwgXG5cdDB4MTIwLCAweDEyQywgMHgxMzgsIDB4MTQ0LCBcblx0MHgxNTAsIDB4MTVDLCAweDE2OCwgMHgxNzQsIFxuXHQweDBCNCwgMHgwQTgsIDB4MDlDLCAweDA5MCwgXG5cdDB4MDg0LCAweDA3OCwgMHgwNkMsIDB4MDYwLCBcblx0MHgwNTQsIDB4MDQ4LCAweDAzQywgMHgwMzAsIFxuXHQweDAyNCwgMHgwMTgsIDB4MDBDXTtcblxuc3lzdGVtLnByb3RvdHlwZS5zdGVwX3JlYWx0aW1lID0gZnVuY3Rpb24gKCkge1xuXHR2YXIgdCA9ICtuZXcgRGF0ZSgpIC8gMTAwMCxcblx0XHRkID0gTWF0aC5taW4odGhpcy5NQVhfQURWQU5DRSwgdCAtIHRoaXMucHJldmlvdXNfY2xvY2spIHx8IDA7XG5cblx0dGhpcy5wcmV2aW91c19jbG9jayA9IHQ7XG5cdHRoaXMuY3ljbGVzICs9IHRoaXMuQ0xPQ0tfUkFURSAqIGQ7XG5cblx0dmFyIHRpY2tzID0gTWF0aC5mbG9vcih0aGlzLmN5Y2xlcyk7XG5cblx0dGhpcy5fdGJoX3RpbWVyICs9IHRpY2tzO1xuXG5cdC8vIEFuaW1hdGlvbiByYXRlIGNvdW50ZXIgKEhBQ2spXG5cdHZhciBUQkhfUkFURSA9IHRoaXMuQ0xPQ0tfUkFURSAvIDI7XG5cdHdoaWxlICh0aGlzLl90YmhfdGltZXIgPj0gVEJIX1JBVEUpIHtcblx0XHR0aGlzLmZpcmVfaXJxKDEzKTtcblx0XHR0aGlzLl90YmhfdGltZXIgLT0gVEJIX1JBVEU7XG5cdH1cblxuXHQvLyBGaXJlIGV2ZXJ5IGZyYW1lIChyYXRlIHVua25vd24sIEhBQ0spXG5cdHRoaXMuZmlyZV9pcnEoMTApO1xuXHR0aGlzLmZpcmVfbm1pKDYpO1xuXG5cdHdoaWxlKHRoaXMuY3ljbGVzID4gMCkgeyB0aGlzLnN0ZXAoKTsgfVxufVxuXG5zeXN0ZW0ucHJvdG90eXBlLmZpcmVfbm1pID0gZnVuY3Rpb24gKGkpIHtcblx0Ly8gTk1JIHdhcyBub3QgZW5hYmxlZFxuXHRpZiAofnRoaXMuX2NwdXJlZ1sweDc2XSAmICgweDgwID4+IGkpKSB7IHJldHVybiA7IH1cblxuXHR0aGlzLm5taSgpO1xufVxuXG5zeXN0ZW0ucHJvdG90eXBlLnBlbmRpbmdfaXJxID0gZnVuY3Rpb24gKCkge1xuXHRyZXR1cm4gKHRoaXMuX2NwdXJlZ1sweDczXSA8PCA4KSB8IHRoaXMuX2NwdXJlZ1sweDc0XTtcbn1cblxuc3lzdGVtLnByb3RvdHlwZS5maXJlX2lycSA9IGZ1bmN0aW9uIChpKSB7XG5cdC8vIE1hcCB0aGUgcGVuZGluZyBpbnRlcnJ1cHRcblx0dmFyIG1hc2sgPSAodGhpcy5fY3B1cmVnWzB4NzBdIDw8IDgpIHwgdGhpcy5fY3B1cmVnWzB4NzFdO1xuXG5cdC8vIFRoaXMgSVJRIGlzIGRpc2FibGVkXG5cdGlmICgoMHg4MDAwID4+IGkpICYgfm1hc2spIHsgcmV0dXJuIDsgfVxuXG5cdC8vIFNldCBwZW5kaW5nIElSUSB0byBmaXJlXG5cdHRoaXMuX2NwdXJlZ1sweDczICsgKGkgPj4gMyldIHw9IDB4ODAgPj4gKGkgJiA3KTtcbn1cblxuc3lzdGVtLnByb3RvdHlwZS5pbnNlcnRfZmlndXJlID0gZnVuY3Rpb24gKGRhdGEpIHtcblx0dGhpcy5zcGlfcm9tID0gbmV3IFVpbnQ4QXJyYXkoZGF0YSk7XG59O1xuXG5zeXN0ZW0ucHJvdG90eXBlLmluaXQgPSBmdW5jdGlvbiAoKSB7XG5cdHZhciBpLCBkYXRhO1xuXG5cdHI2NTAyLnI2NTAyLmluaXQuY2FsbCh0aGlzKTtcblxuXHQvLyBXb3JrIHJhbVxuXHRmb3IgKGkgPSAweDAwMDA7IGkgPCAweDEwMDA7IGkrPTB4MDEwMCkge1xuXHRcdGRhdGEgPSBuZXcgVWludDhBcnJheSh0aGlzLl93cmFtLmJ1ZmZlciwgaSAlIHRoaXMuX3dyYW0ubGVuZ3RoLCAweDEwMCk7XG5cdFx0dGhpcy5yYW0oaT4+OCwgZGF0YSk7XG5cdH1cblxuXHQvLyBEaXNwbGF5IG1lbW9yeVxuXHRmb3IgKGkgPSAweDEwMDA7IGkgPCAweDMwMDA7IGkrPTB4MDEwMCkge1xuXHRcdGRhdGEgPSBuZXcgVWludDhBcnJheSh0aGlzLl9kcmFtLmJ1ZmZlciwgaSAlIHRoaXMuX2RyYW0ubGVuZ3RoLCAweDEwMCk7XG5cdFx0dGhpcy5yYW0oaT4+OCwgZGF0YSk7XG5cdH1cblxuXHQvLyBDUFUgcmVnaXN0ZXJzXG5cdHRoaXMubWFwX3JlZ2lzdGVycygpO1xuXG5cdC8vIFN0YXRpYyByb21cblx0Zm9yICh2YXIgaSA9IDA7IGkgPCAweDQwOyBpICsrKSB7XG5cdFx0dGhpcy5yb20oaSArIDB4QzAsIG5ldyBVaW50OEFycmF5KHRoaXMuYmlvcywgaSA8PCA4LCAweDEwMCkpO1xuXHR9XG5cblx0dGhpcy5fcmVhZGJhbmtbMHhGRkZFXSA9IGZ1bmN0aW9uICgpIHsgcmV0dXJuIHRoaXMuX2lycXNbdGhpcy5wZW5kaW5nX2lycSgpXSAmIDB4RkY7IH1cblx0dGhpcy5fcmVhZGJhbmtbMHhGRkZGXSA9IGZ1bmN0aW9uICgpIHsgcmV0dXJuIHRoaXMuX2lycXNbdGhpcy5wZW5kaW5nX2lycSgpXSA+PiA4OyB9XG5cblx0Ly8gQmFua2FibGUgcm9tXG5cdHRoaXMuc2V0X3JvbV9wYWdlKDApO1x0Ly8gQ2xlYXIgY3VycmVudCByb20gcGFnZVxufVxuXG5zeXN0ZW0ucHJvdG90eXBlLnJlYWQgPSBmdW5jdGlvbihhZGRyLCBub2Fjaykge1xuXHQvLyBBIGFkZHJlc3Npbmdcblx0aWYgKGFkZHIgPT09IG51bGwpIHtcblx0XHRyZXR1cm4gdGhpcy5hO1xuXHR9XG5cblx0aWYoIW5vYWNrKSB0aGlzLl9jcHVhY2NbYWRkcl0gfD0gQUNDRVNTX1JFQUQ7XG5cblx0cmV0dXJuIHRoaXMuX3JlYWRiYW5rW2FkZHJdLmNhbGwodGhpcywgYWRkciAmIDB4RkYpO1xufTtcblxuc3lzdGVtLnByb3RvdHlwZS53cml0ZSA9IGZ1bmN0aW9uIChhZGRyLCBkYXRhKSB7XG5cdGlmIChhZGRyID09PSBudWxsKSB7XG5cdFx0dGhpcy5hID0gZGF0YTsgXG5cdFx0cmV0dXJuIDtcblx0fVxuXG5cdHRoaXMuX2NwdWFjY1thZGRyXSB8PSBBQ0NFU1NfV1JJVEU7XG5cblx0cmV0dXJuIHRoaXMuX3dyaXRlYmFua1thZGRyXS5jYWxsKHRoaXMsIGFkZHIgJiAweEZGLCBkYXRhKTtcbn07XG5cbi8vIFN0YXJ0IGhlbHBlciBmdW5jdGlvbnMgZm9yIG1hcHBpbmcgdG8gbWVtb3J5XG5zeXN0ZW0ucHJvdG90eXBlLnNldF9yb21fcGFnZSA9IGZ1bmN0aW9uIChiYW5rKSB7XG5cdHZhciBvZmZzZXQgPSAweDgwMDAgKiAoYmFuayAlIDIwKTtcblxuXHRmb3IgKHZhciBpID0gMDsgaSA8IDB4ODA7IGkgKyspIHtcblx0XHR0aGlzLnJvbShpICsgMHg0MCwgbmV3IFVpbnQ4QXJyYXkodGhpcy5iaW9zLCBvZmZzZXQgKyAoaSA8PCA4KSwgMHgxMDApKTtcblx0fVxufVxuc3lzdGVtLnByb3RvdHlwZS5yYW0gPSBmdW5jdGlvbiAoYmFuaywgZGF0YSkge1xuXHRmdW5jdGlvbiByZWFkKHJlZykge1xuXHRcdHJldHVybiBkYXRhW3JlZ107XG5cdH1cblxuXHRmdW5jdGlvbiB3cml0ZShyZWcsIHZhbHVlKSB7XG5cdFx0ZGF0YVtyZWddID0gdmFsdWU7XG5cdH1cblxuXHRiYW5rIDw8PSA4O1xuXHRmb3IgKHZhciBpID0gMDsgaSA8IDB4MTAwOyBpKyspIHtcblx0XHR0aGlzLl9yZWFkYmFua1tiYW5rK2ldID0gcmVhZDtcblx0XHR0aGlzLl93cml0ZWJhbmtbYmFuaytpXSA9IHdyaXRlO1xuXHR9XG59O1xuXG5zeXN0ZW0ucHJvdG90eXBlLnJvbSA9IGZ1bmN0aW9uIChiYW5rLCBkYXRhKSB7XG5cdGZ1bmN0aW9uIG51bGx3cml0ZSgpIHt9XG5cdGZ1bmN0aW9uIHJlYWQoYWRkcikge1xuXHRcdHJldHVybiBkYXRhW2FkZHJdO1xuXHR9XG5cblx0YmFuayA8PD0gODtcblx0Zm9yICh2YXIgaSA9IDA7IGkgPCAweDEwMDsgaSsrKSB7XG5cdFx0dGhpcy5fcmVhZGJhbmtbYmFuaytpXSA9IHJlYWQ7XG5cdFx0dGhpcy5fd3JpdGViYW5rW2JhbmsraV0gPSBudWxsd3JpdGU7XG5cdH1cbn07XG5cbm1vZHVsZS5leHBvcnRzID0gIHtcblx0QUNDRVNTX1dSSVRFOiBBQ0NFU1NfV1JJVEUsXG5cdEFDQ0VTU19SRUFEOiBBQ0NFU1NfUkVBRCxcblx0c3lzdGVtOiBzeXN0ZW1cbn07XG5cbiIsIi8qKiBcbiAgKiAgSW5zdHJ1Y3Rpb24gZGVmaW50aW9uIHRhYmxlXG4gICovXG5cbm1vZHVsZS5leHBvcnRzID0ge1xuICAgIDA6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1wbGllZFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQlJLXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiN1wiXG4gICAgfSxcbiAgICAxOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImluZGlyZWN0WFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiT1JBXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNlwiXG4gICAgfSxcbiAgICA1OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJPUkFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIzXCJcbiAgICB9LFxuICAgIDY6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkFTTFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjVcIlxuICAgIH0sXG4gICAgODoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJQSFBcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIzXCJcbiAgICB9LFxuICAgIDk6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1tZWRpYXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJPUkFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDEwOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFjY3VtdWxhdG9yXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBU0xcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDEzOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJPUkFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDE0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBU0xcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDE2OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInJlbGF0aXZlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJCUExcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDE3OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImluZGlyZWN0WVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiT1JBXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNVwiXG4gICAgfSxcbiAgICAyMToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIk9SQVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMjI6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBU0xcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDI0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImltcGxpZWRcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkNMQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgMjU6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJPUkFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDI5OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiT1JBXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAzMDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkFTTFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjdcIlxuICAgIH0sXG4gICAgMzI6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkpTUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjZcIlxuICAgIH0sXG4gICAgMzM6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW5kaXJlY3RYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBTkRcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDM2OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJCSVRcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIzXCJcbiAgICB9LFxuICAgIDM3OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBTkRcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIzXCJcbiAgICB9LFxuICAgIDM4OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJST0xcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI1XCJcbiAgICB9LFxuICAgIDQwOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImltcGxpZWRcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlBMUFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgNDE6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1tZWRpYXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBTkRcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDQyOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFjY3VtdWxhdG9yXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJST0xcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDQ0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJCSVRcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDQ1OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBTkRcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDQ2OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJST0xcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDQ4OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInJlbGF0aXZlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJCTUlcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDQ5OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImluZGlyZWN0WVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQU5EXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNVwiXG4gICAgfSxcbiAgICA1Mzoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkFORFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgNTQ6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJST0xcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDU2OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImltcGxpZWRcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlNFQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgNTc6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBTkRcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDYxOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQU5EXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICA2Mjoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlJPTFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjdcIlxuICAgIH0sXG4gICAgNjQ6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1wbGllZFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiUlRJXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNlwiXG4gICAgfSxcbiAgICA2NToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbmRpcmVjdFhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkVPUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjZcIlxuICAgIH0sXG4gICAgNjk6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkVPUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjNcIlxuICAgIH0sXG4gICAgNzA6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxTUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjVcIlxuICAgIH0sXG4gICAgNzI6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1wbGllZFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiUEhBXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiM1wiXG4gICAgfSxcbiAgICA3Mzoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbW1lZGlhdGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkVPUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgNzQ6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWNjdW11bGF0b3JcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxTUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgNzY6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkpNUFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjNcIlxuICAgIH0sXG4gICAgNzc6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkVPUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgNzg6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxTUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjZcIlxuICAgIH0sXG4gICAgODA6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwicmVsYXRpdmVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkJWQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgODE6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW5kaXJlY3RZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJFT1JcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI1XCJcbiAgICB9LFxuICAgIDg1OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiRU9SXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICA4Njoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxTUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjZcIlxuICAgIH0sXG4gICAgODg6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1wbGllZFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQ0xJXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICA4OToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVlcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkVPUlwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgOTM6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJFT1JcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDk0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiTFNSXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiN1wiXG4gICAgfSxcbiAgICA5Njoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJSVFNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDk3OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImluZGlyZWN0WFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQURDXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNlwiXG4gICAgfSxcbiAgICAxMDE6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkFEQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjNcIlxuICAgIH0sXG4gICAgMTAyOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJST1JcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI1XCJcbiAgICB9LFxuICAgIDEwNDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJQTEFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDEwNToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbW1lZGlhdGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkFEQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgMTA2OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFjY3VtdWxhdG9yXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJST1JcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDEwODoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbmRpcmVjdFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiSk1QXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNVwiXG4gICAgfSxcbiAgICAxMDk6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkFEQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTEwOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJST1JcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDExMjoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJyZWxhdGl2ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQlZTXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAxMTM6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW5kaXJlY3RZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBRENcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI1XCJcbiAgICB9LFxuICAgIDExNzoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkFEQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTE4OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiUk9SXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNlwiXG4gICAgfSxcbiAgICAxMjA6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1wbGllZFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiU0VJXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAxMjE6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJBRENcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDEyNToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkFEQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTI2OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiUk9SXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiN1wiXG4gICAgfSxcbiAgICAxMjk6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW5kaXJlY3RYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTVEFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDEzMjoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiU1RZXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiM1wiXG4gICAgfSxcbiAgICAxMzM6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlNUQVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjNcIlxuICAgIH0sXG4gICAgMTM0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTVFhcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIzXCJcbiAgICB9LFxuICAgIDEzNjoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJERVlcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDEzODoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJUWEFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDE0MDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiU1RZXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAxNDE6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlNUQVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTQyOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTVFhcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDE0NDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJyZWxhdGl2ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQkNDXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAxNDU6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW5kaXJlY3RZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTVEFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDE0ODoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlNUWVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTQ5OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiU1RBXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAxNTA6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTVFhcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDE1Mjoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJUWUFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDE1Mzoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVlcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlNUQVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjVcIlxuICAgIH0sXG4gICAgMTU0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImltcGxpZWRcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlRYU1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgMTU3OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiU1RBXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNVwiXG4gICAgfSxcbiAgICAxNjA6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1tZWRpYXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJMRFlcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDE2MToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbmRpcmVjdFhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxEQVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjZcIlxuICAgIH0sXG4gICAgMTYyOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImltbWVkaWF0ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiTERYXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAxNjQ6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxEWVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjNcIlxuICAgIH0sXG4gICAgMTY1OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJMREFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIzXCJcbiAgICB9LFxuICAgIDE2Njoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiTERYXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiM1wiXG4gICAgfSxcbiAgICAxNjg6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1wbGllZFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiVEFZXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAxNjk6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1tZWRpYXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJMREFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDE3MDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJUQVhcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDE3Mjoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiTERZXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAxNzM6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxEQVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTc0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJMRFhcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDE3Njoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJyZWxhdGl2ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQkNTXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAxNzc6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW5kaXJlY3RZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJMREFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI1XCJcbiAgICB9LFxuICAgIDE4MDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxEWVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTgxOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiTERBXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAxODI6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJMRFhcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDE4NDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJDTFZcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDE4NToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVlcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxEQVwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTg2OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImltcGxpZWRcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlRTWFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgMTg4OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiTERZXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAxODk6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJMREFcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDE5MDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVlcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkxEWFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMTkyOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImltbWVkaWF0ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQ1BZXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAxOTM6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW5kaXJlY3RYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJDTVBcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDE5Njoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQ1BZXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiM1wiXG4gICAgfSxcbiAgICAxOTc6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkNNUFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjNcIlxuICAgIH0sXG4gICAgMTk4OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJERUNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI1XCJcbiAgICB9LFxuICAgIDIwMDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJJTllcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDIwMToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbW1lZGlhdGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkNNUFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgMjAyOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImltcGxpZWRcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkRFWFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgMjA0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJDUFlcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDIwNToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQ01QXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAyMDY6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkRFQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjNcIlxuICAgIH0sXG4gICAgMjA4OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInJlbGF0aXZlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJCTkVcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDIwOToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbmRpcmVjdFlcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkNNUFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjVcIlxuICAgIH0sXG4gICAgMjEzOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQ01QXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAyMTQ6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJERUNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDIxNjoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJDTERcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDIxNzoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVlcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkNNUFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMjIxOiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQ01QXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAyMjI6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVYXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJERUNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI3XCJcbiAgICB9LFxuICAgIDIyNDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbW1lZGlhdGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkNQWFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjJcIlxuICAgIH0sXG4gICAgMjI1OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImluZGlyZWN0WFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiU0JDXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNlwiXG4gICAgfSxcbiAgICAyMjg6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiemVyb3BhZ2VcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIkNQWFwiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjNcIlxuICAgIH0sXG4gICAgMjI5OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTQkNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIzXCJcbiAgICB9LFxuICAgIDIzMDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiSU5DXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNVwiXG4gICAgfSxcbiAgICAyMzI6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1wbGllZFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiSU5YXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAyMzM6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1tZWRpYXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTQkNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDIzNDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJpbXBsaWVkXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJOT1BcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCIyXCJcbiAgICB9LFxuICAgIDIzNjoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQ1BYXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNFwiXG4gICAgfSxcbiAgICAyMzc6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlNCQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMjM4OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJJTkNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI2XCJcbiAgICB9LFxuICAgIDI0MDoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJyZWxhdGl2ZVwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiQkVRXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAyNDE6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW5kaXJlY3RZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTQkNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI1XCJcbiAgICB9LFxuICAgIDI0NToge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJ6ZXJvcGFnZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlNCQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMjQ2OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcInplcm9wYWdlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiSU5DXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiNlwiXG4gICAgfSxcbiAgICAyNDg6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiaW1wbGllZFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiU0VEXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiMlwiXG4gICAgfSxcbiAgICAyNDk6IHtcbiAgICAgICAgXCJhZGRyZXNzaW5nXCI6IFwiYWJzb2x1dGVZXCIsXG4gICAgICAgIFwiaW5zdHJ1Y3Rpb25cIjogXCJTQkNcIixcbiAgICAgICAgXCJjeWNsZXNcIjogXCI0XCJcbiAgICB9LFxuICAgIDI1Mzoge1xuICAgICAgICBcImFkZHJlc3NpbmdcIjogXCJhYnNvbHV0ZVhcIixcbiAgICAgICAgXCJpbnN0cnVjdGlvblwiOiBcIlNCQ1wiLFxuICAgICAgICBcImN5Y2xlc1wiOiBcIjRcIlxuICAgIH0sXG4gICAgMjU0OiB7XG4gICAgICAgIFwiYWRkcmVzc2luZ1wiOiBcImFic29sdXRlWFwiLFxuICAgICAgICBcImluc3RydWN0aW9uXCI6IFwiSU5DXCIsXG4gICAgICAgIFwiY3ljbGVzXCI6IFwiN1wiXG4gICAgfVxufTtcbiIsIi8qKiBcbiAgKiAgUG9ydCBuYW1lIGRlZmluaXRpb24gdGFibGVcbiAgKi9cblxubW9kdWxlLmV4cG9ydHMgPSB7XG5cdC8vIFN5c3RlbSBibG9ja1xuXHQweDMwMDA6IHsgXG5cdFx0bmFtZTogXCJQX0NQVV9CYW5rX0N0cmxcIiwgXG5cdFx0ZmllbGRzOiBbXG5cdFx0XHR7IG5hbWU6XCJiYW5rXCIsIHN0YXJ0OiAwLCBsZW5ndGg6IDggfVxuXHRcdF1cblx0fSxcblx0MHgzMDAxOiB7IFxuXHRcdG5hbWU6IFwiUF9DUFVfQ2xrX0N0cmxcIiwgXG5cdH0sXG5cdDB4MzAwMjogeyBcblx0XHRuYW1lOiBcIlBfMzI3NjhfRU5cIiwgXG5cdH0sXG5cdDB4MzAwNDogeyBcblx0XHRuYW1lOiBcIldhdGNoZG9nIFRpbWVyP1wiLFxuXHR9LFxuXHQweDMwMDY6IHtcblx0XHRuYW1lOiBcIlNvdW5kIFJlbGF0ZWQ/XCIsXG5cdH0sXG5cdDB4MzAwNzogeyBcblx0XHRuYW1lOiBcIlBfV2FrZXVwX0N0cmxcIixcblx0fSxcblxuXHQvLyBQb3J0IGJsb2NrXG5cdDB4MzAxMDogeyBcblx0XHRuYW1lOiBcIlBfUG9ydEFfQ29uZmlnXCIsXG5cdH0sXG5cdDB4MzAxMTogeyBcblx0XHRuYW1lOiBcIlBfUG9ydEFfRGlyXCIsXG5cdFx0ZmllbGRzOiBbXG5cdFx0XHR7IG5hbWU6XCJJUiBUcmFuc21pdFwiLCBzdGFydDogNywgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJDYXJ0IENEMlwiLCBzdGFydDogNiwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJDYXJ0IENEMVwiLCBzdGFydDogNSwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJDYXJ0IFBvd2VyXCIsIHN0YXJ0OiA0LCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIlJlc2V0XCIsIHN0YXJ0OiAzLCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIkNcIiwgc3RhcnQ6IDIsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiQlwiLCBzdGFydDogMSwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJBXCIsIHN0YXJ0OiAwLCBsZW5ndGg6IDEgfVxuXHRcdF1cblx0fSxcblx0MHgzMDEyOiB7IFxuXHRcdG5hbWU6IFwiUF9Qb3J0QV9EYXRhXCIsXG5cdFx0ZmllbGRzOiBbXG5cdFx0XHR7IG5hbWU6XCJJUiBSeFwiLCBzdGFydDogNywgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJDYXJ0IENEMlwiLCBzdGFydDogNiwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJDYXJ0IENEMVwiLCBzdGFydDogNSwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJDYXJ0IFBvd2VyXCIsIHN0YXJ0OiA0LCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIlJlc2V0XCIsIHN0YXJ0OiAzLCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIkNcIiwgc3RhcnQ6IDIsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiQlwiLCBzdGFydDogMSwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJBXCIsIHN0YXJ0OiAwLCBsZW5ndGg6IDEgfVxuXHRcdF1cblx0fSxcblx0MHgzMDEzOiB7IFxuXHRcdG5hbWU6IFwiUF9Qb3J0QV9TdHJvYmVcIixcblx0fSxcblx0MHgzMDE0OiB7IFxuXHRcdG5hbWU6IFwiUF9Qb3J0Ql9Db25maWdcIixcblx0fSxcblx0MHgzMDE1OiB7IFxuXHRcdG5hbWU6IFwiUF9Qb3J0Ql9EaXJcIixcblx0XHRmaWVsZHM6IFtcblx0XHRcdHsgbmFtZTpcIlNQSSBSeFwiLCBzdGFydDogNywgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJTUEkgVHhcIiwgc3RhcnQ6IDYsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiU1BJIENsa1wiLCBzdGFydDogNSwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJTUEkgQ1NOXCIsIHN0YXJ0OiA0LCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIklSIFR4XCIsIHN0YXJ0OiAzLCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIkkyQyBQb3dlclwiLCBzdGFydDogMiwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJJMkMgQ2xrXCIsIHN0YXJ0OiAxLCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIkkyQyBEYXRhXCIsIHN0YXJ0OiAwLCBsZW5ndGg6IDEgfVxuXHRcdF1cblx0fSxcblx0MHgzMDE2OiB7IFxuXHRcdG5hbWU6IFwiUF9Qb3J0Ql9EYXRhXCIsXG5cdFx0ZmllbGRzOiBbXG5cdFx0XHR7IG5hbWU6XCJTUEkgUnhcIiwgc3RhcnQ6IDcsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiU1BJIFR4XCIsIHN0YXJ0OiA2LCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIlNQSSBDbGtcIiwgc3RhcnQ6IDUsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiU1BJIENTTlwiLCBzdGFydDogNCwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJJUiBUeFwiLCBzdGFydDogMywgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJJMkMgUG93ZXJcIiwgc3RhcnQ6IDIsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiSTJDIENsa1wiLCBzdGFydDogMSwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJJMkMgRGF0YVwiLCBzdGFydDogMCwgbGVuZ3RoOiAxIH1cblx0XHRdXG5cdH0sXG5cblx0Ly8gVGltZXIgY29udHJvbCBibG9ja1xuXHQweDMwMzA6IHsgXG5cdFx0bmFtZTogXCJUaW1lciBDb250cm9sIDA/XCIsXG5cdH0sXG5cdDB4MzAzMTogeyBcblx0XHRuYW1lOiBcIlRpbWVyIENvbnRyb2wgMT9cIixcblx0fSxcblx0MHgzMDMyOiB7IFxuXHRcdG5hbWU6IFwiVGltZXIgT3V0cHV0IDAgbG93P1wiLFxuXHR9LFxuXHQweDMwMzM6IHsgXG5cdFx0bmFtZTogXCJUaW1lciBPdXRwdXQgMCBoaWdoP1wiLFxuXHR9LFxuXHQweDMwMzQ6IHsgXG5cdFx0bmFtZTogXCJUaW1lciBPdXRwdXQgMSBsb3c/XCIsXG5cdH0sXG5cdDB4MzAzNTogeyBcblx0XHRuYW1lOiBcIlRpbWVyIE91dHB1dCAxIGhpZ2g/XCIsXG5cdH0sXG5cdDB4MzAzRTogeyBcblx0XHRuYW1lOiBcIlBfU2VnMF9TY2FuX0N0cmxcIixcblx0fSxcblx0MHgzMDNGOiB7IFxuXHRcdG5hbWU6IFwiUF9TZWc4X1NjYW5fQ3RybFwiLFxuXHR9LFxuXG5cdC8vIExDRCBDb250cm9sIGJsb2NrXG5cdDB4MzA0MDogeyBcblx0XHRuYW1lOiBcIlBfTENEX1NldHVwMVwiLFxuXHR9LFxuXHQweDMwNDE6IHsgXG5cdFx0bmFtZTogXCJQX0xDRF9TZXR1cDJcIixcblx0fSxcblx0MHgzMDQyOiB7IFxuXHRcdG5hbWU6IFwiUF9MQ0RfQ2xvY2sxXCIsXG5cdH0sXG5cdDB4MzA0MzogeyBcblx0XHRuYW1lOiBcIlBfTENEX0Nsb2NrMlwiLFxuXHR9LFxuXHQweDMwNDQ6IHsgXG5cdFx0bmFtZTogXCJQX0xDRF9TRUdfTnVtXCIsXG5cdH0sXG5cdDB4MzA0NTogeyBcblx0XHRuYW1lOiBcIlBfTENEX0NPTV9OdW1cIixcblx0fSxcblx0MHgzMDQ3OiB7IFxuXHRcdG5hbWU6IFwiUF9MQ0RfQnVmZmVyX0N0cmxcIixcblx0fSxcblx0MHgzMDQ4OiB7XG5cdFx0bmFtZTogXCJQX0NvbnRyYXN0X0N0cmxcIixcblx0fSxcblx0MHgzMDQ5OiB7IFxuXHRcdG5hbWU6IFwiUF9MQ0RfUHVtcF9DdHJsXCIsXG5cdH0sXG5cblx0Ly8gU291bmQgY29udHJvbCBibG9ja1xuXHQweDMwNjA6IHtcblx0XHRuYW1lOiBcIlNvdW5kIFJlbGF0ZWQ/XCIsXG5cdH0sXG5cdDB4MzA2Mjoge1xuXHRcdG5hbWU6IFwiU291bmQgUmVsYXRlZD9cIixcblx0fSxcblx0MHgzMDY0OiB7XG5cdFx0bmFtZTogXCJTb3VuZCBSZWxhdGVkP1wiLFxuXHR9LFxuXHQweDMwNjU6IHtcblx0XHRuYW1lOiBcIlNvdW5kIFJlbGF0ZWQ/XCIsXG5cdH0sXG5cblx0Ly8gSW50ZXJydXB0IGNvbnRyb2wgYmxvY2tcblx0MHgzMDcwOiB7IFxuXHRcdG5hbWU6IFwiUF9JTlRfQ3RybDBcIixcblx0XHRmaWVsZHM6IFtcblx0XHRcdHsgbmFtZTpcIkVuYWJsZSBUTTBcIiwgc3RhcnQ6IDcsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiRW5hYmxlIElSUTFcIiwgc3RhcnQ6IDYsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiRW5hYmxlIElSUTJcIiwgc3RhcnQ6IDUsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiRW5hYmxlIDIwNDhcIiwgc3RhcnQ6IDQsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiRW5hYmxlIDgxOTJcIiwgc3RhcnQ6IDMsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiRW5hYmxlIFNQVVwiLCBzdGFydDogMiwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJFbmFibGUgU1BJXCIsIHN0YXJ0OiAxLCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIkVuYWJsZSBGUFwiLCBzdGFydDogMCwgbGVuZ3RoOiAxIH1cblx0XHRdXG5cdH0sXG5cdDB4MzA3MTogeyBcblx0XHRuYW1lOiBcIlBfSU5UX0N0cmwxXCIsXG5cdFx0ZmllbGRzOiBbXG5cdFx0XHR7IG5hbWU6XCJFbmFibGUgSVJROFwiLCBzdGFydDogNywgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJFbmFibGUgSVJROVwiLCBzdGFydDogNiwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJFbmFibGUgVE0xXCIsIHN0YXJ0OiA1LCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIkVuYWJsZSBJUlExMVwiLCBzdGFydDogNCwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJFbmFibGUgVEJIXCIsIHN0YXJ0OiAzLCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIkVuYWJsZSBUQkxcIiwgc3RhcnQ6IDIsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiRW5hYmxlIElSUTE0XCIsIHN0YXJ0OiAxLCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIkVuYWJsZSBJUlExNVwiLCBzdGFydDogMCwgbGVuZ3RoOiAxIH1cblx0XHRdXG5cdH0sXG5cdDB4MzA3MjogeyBcblx0XHRuYW1lOiBcIlBfSU5UX0N0cmwyID9cIixcblx0fSxcblx0MHgzMDczOiB7IFxuXHRcdG5hbWU6IFwiUF9JTlRfRmxhZzBcIixcblx0XHRmaWVsZHM6IFtcblx0XHRcdHsgbmFtZTpcIlBlbmRpbmcgVE0wXCIsIHN0YXJ0OiA3LCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIlBlbmRpbmcgSVJRMVwiLCBzdGFydDogNiwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJQZW5kaW5nIElSUTJcIiwgc3RhcnQ6IDUsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiUGVuZGluZyAyMDQ4XCIsIHN0YXJ0OiA0LCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIlBlbmRpbmcgODE5MlwiLCBzdGFydDogMywgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJQZW5kaW5nIFNQVVwiLCBzdGFydDogMiwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJQZW5kaW5nIFNQSVwiLCBzdGFydDogMSwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJQZW5kaW5nIEZQXCIsIHN0YXJ0OiAwLCBsZW5ndGg6IDEgfVxuXHRcdF1cblx0fSxcblx0MHgzMDc0OiB7IFxuXHRcdG5hbWU6IFwiUF9JTlRfRmxhZzFcIixcblx0XHRmaWVsZHM6IFtcblx0XHRcdHsgbmFtZTpcIlBlbmRpbmcgSVJROFwiLCBzdGFydDogNywgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJQZW5kaW5nIElSUTlcIiwgc3RhcnQ6IDYsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiUGVuZGluZyBUTTFcIiwgc3RhcnQ6IDUsIGxlbmd0aDogMSB9LFxuXHRcdFx0eyBuYW1lOlwiUGVuZGluZyBJUlExMVwiLCBzdGFydDogNCwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJQZW5kaW5nIFRCSFwiLCBzdGFydDogMywgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJQZW5kaW5nIFRCTFwiLCBzdGFydDogMiwgbGVuZ3RoOiAxIH0sXG5cdFx0XHR7IG5hbWU6XCJQZW5kaW5nIElSUTE0XCIsIHN0YXJ0OiAxLCBsZW5ndGg6IDEgfSxcblx0XHRcdHsgbmFtZTpcIlBlbmRpbmcgSVJRMTVcIiwgc3RhcnQ6IDAsIGxlbmd0aDogMSB9XG5cdFx0XVxuXHR9LFxuXHQweDMwNzU6IHsgXG5cdFx0bmFtZTogXCJQX0lOVF9GbGFnMiA/XCIsXG5cdH0sXG5cdDB4MzA3NjogeyBcblx0XHRuYW1lOiBcIlBfTk1JX0N0cmxcIixcblx0fSxcblxuXHQvLyBTUEkgXG5cdDB4MzBCMDoge1xuXHRcdG5hbWU6IFwiUF9TUElfQ3RybCA/XCIsXG5cdH0sXG5cdDB4MzBCMToge1xuXHRcdG5hbWU6IFwiU1BJIHJlbGF0ZWRcIixcblx0fSxcblx0MHgzMEIyOiB7XG5cdFx0bmFtZTogXCJTUEkgcmVsYXRlZCA/XCIsXG5cdH0sXG5cdDB4MzBCMzoge1xuXHRcdG5hbWU6IFwiUF9TUElfV3JpdGUgP1wiLFxuXHR9LFxuXHQweDMwQjQ6IHtcblx0XHRuYW1lOiBcIlNQSSByZWxhdGVkID9cIixcblx0fSxcblx0MHgzMEI1OiB7XG5cdFx0bmFtZTogXCJTUEkgcmVsYXRlZCA/XCIsXG5cdH0sXG5cdDB4MzBCNjoge1xuXHRcdG5hbWU6IFwiUF9TUElfUmVhZCA/XCIsXG5cdH0sXG5cdDB4MzBCNzoge1xuXHRcdG5hbWU6IFwiUF9TUElfU3RhdHVzID9cIixcblx0fSxcblx0MHgzMEJBOiB7XG5cdFx0bmFtZTogXCJTUEkgcmVsYXRlZCA/XCJcblx0fSxcbn07XG4iLCJ2YXIgY29uZmlnID0gcmVxdWlyZSAoXCIuL2NvbmZpZy5qc1wiKSxcblx0dGFtYWdvdGNoaSA9IHJlcXVpcmUoXCIuL2NwdS90YW1hZ290Y2hpLmpzXCIpLFxuXHRkaXNhc3NlbWJsZSA9IHJlcXVpcmUoXCIuL2NwdS9kaXNhc3NlbWJsZXIuanNcIiksXG5cdHBvcnRzID0gcmVxdWlyZShcIi4vZGF0YS9wb3J0cy5qc1wiKSxcblxuXHRvYmplY3QgPSByZXF1aXJlKFwiLi4vdXRpbC9vYmplY3QuanNcIiksXG5cdFxuXHRtYWluVGVtcGxhdGUgPSByZXF1aXJlKFwiLi4vdGVtcGxhdGVzL21haW4uaHRtbFwiKSxcblx0cG9ydFRlbXBsYXRlID0gcmVxdWlyZShcIi4uL3RlbXBsYXRlcy9wb3J0Lmh0bWxcIik7XG5cbmZ1bmN0aW9uIGdldEJpbmFyeShwYXRoLCBjYikge1xuXHR2YXIgeGhyID0gbmV3IFhNTEh0dHBSZXF1ZXN0KCk7XG5cdHhoci5vcGVuKFwiR0VUXCIsIHBhdGgsIHRydWUpO1xuXHR4aHIucmVzcG9uc2VUeXBlID0gXCJhcnJheWJ1ZmZlclwiO1xuXHR4aHIuc2VuZCgpO1xuXG5cdHhoci5vbnJlYWR5c3RhdGVjaGFuZ2UgPSBmdW5jdGlvbiAoKSB7XG5cdFx0aWYgKHhoci5yZWFkeVN0YXRlICE9PSA0KSB7XG5cdFx0XHRyZXR1cm4gO1xuXHRcdH1cblxuXHRcdGlmICh4aHIuc3RhdHVzICE9PSAyMDApIHtcblx0XHRcdHRocm93IG5ldyBFcnJvcihcIkNvdWxkIG5vdCBkb3dubG9hZCBcIiArIHBhdGgpO1xuXHRcdH1cblxuXHRcdGNiKHhoci5yZXNwb25zZSk7XG5cdH07XG59XG5cbmZ1bmN0aW9uIHRvSGV4KHcsIGkpIHsgXG5cdGkgPSBpLnRvU3RyaW5nKDE2KS50b1VwcGVyQ2FzZSgpO1xuXG5cdHZhciB6ZXJvcyA9IFwiMFwiO1xuXHR3aGlsZSAoemVyb3MubGVuZ3RoIDwgdykgeyB6ZXJvcyArPSB6ZXJvczsgfVxuXG5cdHJldHVybiB6ZXJvcy5zdWJzdHIoMCwgdykuc3Vic3RyKGkubGVuZ3RoKSArIGk7XG59XG5cbmZ1bmN0aW9uIHN0YXJ0KGJpb3MpIHtcblx0Z2V0QmluYXJ5KFwiZmlsZXMvdGFtYWdvLmJpblwiLCBmdW5jdGlvbiAoYmlvcykge1xuXHRcdC8vIEJpbmQgdG8gdGFtYWdvIHN5c3RlbSBjbGFzc1xuXHRcdHRhbWFnb3RjaGkuc3lzdGVtLnByb3RvdHlwZS5iaW9zID0gYmlvcztcblxuXHRcdC8vIFN0YXJ0IHRoZSBhcHBsaWNhdGlvbiB3aGVuIEJJT1MgaXMgZG9uZVxuXHRcdFtdLmZvckVhY2guY2FsbChkb2N1bWVudC5xdWVyeVNlbGVjdG9yQWxsKFwidGFtYWdvXCIpLCBmdW5jdGlvbiAoZWxlbSkge1xuXHRcdFx0bmV3IFRhbWFnbyhlbGVtKTtcblx0XHR9KTtcblx0fSk7XG59XG5cbmZ1bmN0aW9uIFRhbWFnbyhlbGVtZW50KSB7XG5cdHZhciB1OCA9IG5ldyBVaW50OEFycmF5KHRoaXMuYmlvcyksXG5cdFx0dGhhdCA9IHRoaXM7XG5cblx0dGhpcy5zeXN0ZW0gPSBuZXcgdGFtYWdvdGNoaS5zeXN0ZW0oKTtcblxuXHR0aGlzLmNvbmZpZ3VyZShlbGVtZW50KTtcblxuXHR0aGlzLl9waXhlbGRhdGEgPSB0aGlzLmJvZHkuZGlzcGxheS5nZXRJbWFnZURhdGEoMCwwLDY0LDMxKTtcblx0dGhpcy5fcGl4ZWxzID0gbmV3IFVpbnQzMkFycmF5KHRoaXMuX3BpeGVsZGF0YS5kYXRhLmJ1ZmZlcik7XG5cdHRoaXMuX2Rpc2FzbU9mZnNldCA9IDA7XG5cblx0dGhpcy5yZWZyZXNoKCk7XG5cblx0ZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcImtleXVwXCIsIGZ1bmN0aW9uIChlKSB7XG5cdFx0dGhhdC5zeXN0ZW0ua2V5cyB8PSB0aGF0Lm1hcHBpbmdbZS5rZXlDb2RlXSB8fCAwO1xuXHR9KTtcblx0ZG9jdW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcImtleWRvd25cIiwgZnVuY3Rpb24gKGUpIHtcblx0XHR0aGF0LnN5c3RlbS5rZXlzICY9IH50aGF0Lm1hcHBpbmdbZS5rZXlDb2RlXSB8fCAweEZGO1xuXHR9KTtcbn1cblxuLy8gS2V5Ym9hcmQgbWFwcGluZ1xuVGFtYWdvLnByb3RvdHlwZS5tYXBwaW5nID0geyA2NTogMSwgODM6IDIsIDY4OiA0LCA4MjogOCB9O1xuXG5cblRhbWFnby5wcm90b3R5cGUuc3RlcCA9IGZ1bmN0aW9uIChlKSB7XG5cdHRoaXMuc3lzdGVtLnN0ZXAoKTtcblx0dGhpcy5yZWZyZXNoKCk7XG59XG5cblRhbWFnby5wcm90b3R5cGUuaXJxID0gZnVuY3Rpb24gKGUpIHtcblx0dGhpcy5zeXN0ZW0uZmlyZV9pcnEocGFyc2VJbnQodGhpcy5ib2R5LnNlbGVjdHMuaXJxLnZhbHVlLDEwKSk7XG5cdHRoaXMucmVmcmVzaCgpO1xufVxuXG5UYW1hZ28ucHJvdG90eXBlLm5taSA9IGZ1bmN0aW9uIChlKSB7XG5cdHRoaXMuc3lzdGVtLmZpcmVfbm1pKDYpO1xuXHR0aGlzLnJlZnJlc2goKTtcbn1cblxuVGFtYWdvLnByb3RvdHlwZS5ydW4gPSBmdW5jdGlvbiAoZSkge1xuXHR2YXIgdGhhdCA9IHRoaXM7XG5cblx0ZnVuY3Rpb24gZnJhbWUoKSB7XG5cdFx0aWYgKCF0aGF0LnJ1bm5pbmcpIHsgcmV0dXJuIDsgfVxuXG5cdFx0dGhhdC5zeXN0ZW0uc3RlcF9yZWFsdGltZSgpO1xuXHRcdHRoYXQucmVmcmVzaCgpO1xuXHRcdHJlcXVlc3RBbmltYXRpb25GcmFtZShmcmFtZSk7XG5cdH1cblxuXHR0aGlzLnJ1bm5pbmcgPSAhdGhpcy5ydW5uaW5nO1x0XG5cdGZyYW1lKCk7XG5cblx0aWYgKGUpIHsgZS50YXJnZXQuYXR0cmlidXRlcy52YWx1ZS52YWx1ZSA9IHRoaXMucnVubmluZyA/IFwic3RvcFwiIDogXCJydW5cIjsgfVxufVxuXG5UYW1hZ28ucHJvdG90eXBlLnJlc2V0ID0gZnVuY3Rpb24gKGUpIHtcblx0dGhpcy5zeXN0ZW0ucmVzZXQoKTtcblx0dGhpcy5yZWZyZXNoKCk7XHRcdFxufVxuXG5UYW1hZ28ucHJvdG90eXBlLnJlZnJlc2hfc2ltcGxlID0gZnVuY3Rpb24gKCkge1xuXHR2YXIgYSA9IDQsIGIgPSAwLCBnID0gMDtcblxuXHR3aGlsZSAoZyA8IDEwKSB7XG5cdFx0dmFyIGdseXBoID0gKHRoaXMuc3lzdGVtLl9kcmFtW2FdID4+IGIpICYgMztcblx0XHRpZiAoKGIgLT0gMikgPCAwKSB7IGIgPSA2OyBhKys7IH1cblxuXHRcdHRoaXMuYm9keS5nbHlwaHNbZysrXS5zdHlsZS5jb2xvciA9IFwiI1wiICsgKHRoaXMuc3lzdGVtLlBBTEVUVEVbZ2x5cGhdICYgMHhGRkZGRkYpLnRvU3RyaW5nKDE2KTtcblx0fVxuXG5cdHZhciBweCA9IDA7XG5cdGZvciAodmFyIHkgPSAwOyB5IDwgMzE7IHkrKykge1xuXHRcdHZhciBhID0gdGhpcy5zeXN0ZW0uTENEX09SREVSW3ldOyBcblxuXHRcdGZvciAodmFyIHggPSAwOyB4IDwgNjQ7IHggKz0gNCkge1xuXHRcdFx0dmFyIGQgPSB0aGlzLnN5c3RlbS5fZHJhbVthKytdLCBiID0gNjtcblxuXHRcdFx0d2hpbGUgKGIgPj0gMCkge1xuXHRcdFx0XHR0aGlzLl9waXhlbHNbcHgrK10gPSB0aGlzLnN5c3RlbS5QQUxFVFRFWyhkID4+IGIpICYgM107XG5cdFx0XHRcdGIgLT0gMjtcblx0XHRcdH1cblx0XHR9XG5cdH1cblxuXHR0aGlzLmJvZHkuZGlzcGxheS5wdXRJbWFnZURhdGEodGhpcy5fcGl4ZWxkYXRhLCAwLCAwKTtcbn1cblxuVGFtYWdvLnByb3RvdHlwZS5kcm9wID0gZnVuY3Rpb24gKGV2dCkge1xuXHRldnQuc3RvcFByb3BhZ2F0aW9uKCk7XG5cdGV2dC5wcmV2ZW50RGVmYXVsdCgpO1xuXHQgXG5cdHZhciBmaWxlcyA9IGV2dC5kYXRhVHJhbnNmZXIuZmlsZXMsXG5cdFx0YmluYXJ5ID0gZmlsZXNbMF0sXG5cdFx0dGhhdCA9IHRoaXM7XG5cdFxuXHRpZiAoZmlsZXMubGVuZ3RoIDwgMCkgeyByZXR1cm4gOyB9XG5cblx0dGhpcy5ib2R5LmZpZ3VyZS5pbm5lckhUTUwgPSBiaW5hcnkubmFtZSArIFwiIGluc2VydGVkXCI7XG5cdFxuXHR2YXIgcmVhZGVyID0gbmV3IEZpbGVSZWFkZXIoKTtcblx0cmVhZGVyLm9ubG9hZCA9IGZ1bmN0aW9uKGUpIHtcblx0XHR0aGF0LnN5c3RlbS5pbnNlcnRfZmlndXJlKGUudGFyZ2V0LnJlc3VsdHMpO1xuXHR9XG5cdHJlYWRlci5yZWFkQXNBcnJheUJ1ZmZlcihiaW5hcnkpO1xufTtcblxuVGFtYWdvLnByb3RvdHlwZS51cGRhdGVfY29udHJvbCA9IGZ1bmN0aW9uIChlKSB7XG5cdGlmIChlKSB7XG5cdFx0dGhpcy5fZGVidWdfcG9ydCA9IHBhcnNlSW50KGUudGFyZ2V0LmRhdGFzZXQuYWRkcmVzcyk7XG5cdH1cblxuXHR2YXIgcG9ydCA9IHBvcnRzW3RoaXMuX2RlYnVnX3BvcnRdO1xuXHRpZiAoIXBvcnQpIHtcblx0XHRwb3J0ID0ge1xuXHRcdFx0bmFtZTogXCJVbmtub3duXCIsXG5cdFx0XHRkZXNjcmlwdGlvbjogXCJcIixcblx0XHR9XG5cdH1cblx0aWYgKCFwb3J0LmZpZWxkcykge1xuXHRcdHBvcnQuZmllbGRzID0gW3sgbmFtZTpcImRhdGFcIiwgc3RhcnQ6IDAsIGxlbmd0aDogOCB9XTtcblx0fVxuXG5cdHBvcnQgPSBPYmplY3QuY3JlYXRlKHBvcnQpO1xuXHRwb3J0LmFkZHJlc3MgPSB0aGlzLl9kZWJ1Z19wb3J0LnRvU3RyaW5nKDE2KTtcblxuXHRpZiAocG9ydC5hZGRyZXNzLmxlbmd0aCA8IDIpIHBvcnQuYWRkcmVzcyA9IFwiMFwiICsgcG9ydC5hZGRyZXNzO1xuXG5cdHRoaXMuYm9keS5wb3J0LmlubmVySFRNTCA9IHBvcnRUZW1wbGF0ZShwb3J0KTtcblx0dGhpcy5ib2R5LmZpZWxkcyA9IHRoaXMuYm9keS5wb3J0LnF1ZXJ5U2VsZWN0b3JBbGwoXCJmaWVsZFwiKTtcblx0XG5cdHRoaXMucmVmcmVzaF9wb3J0KCk7XG59XG5cblRhbWFnby5wcm90b3R5cGUucmVmcmVzaF9wb3J0ID0gZnVuY3Rpb24gKCkge1xuXHR2YXIgZCA9IHRoaXMuc3lzdGVtLnJlYWQodGhpcy5fZGVidWdfcG9ydCwgdHJ1ZSk7XG5cblx0ZnVuY3Rpb24gcGFkKHMsIGwpIHtcblx0XHRyZXR1cm4gXCIwMDAwMDAwMFwiLnN1YnN0cigwLCBsKS5zdWJzdHIocy5sZW5ndGgpICsgcztcblx0fVxuXG5cdFtdLmZvckVhY2guY2FsbCh0aGlzLmJvZHkuZmllbGRzLCBmdW5jdGlvbiAoZikge1xuXHRcdHZhciBsID0gTnVtYmVyKGYuZGF0YXNldC5sZW5ndGgpLFxuXHRcdFx0cyA9IE51bWJlcihmLmRhdGFzZXQuc3RhcnQpLFxuXHRcdFx0bSA9IChkID4+IHMpICYgKCgxIDw8IGwpIC0gMSksXG5cdFx0XHRiID0gZi5xdWVyeVNlbGVjdG9yKFwiYmluXCIpLFxuXHRcdFx0aCA9IGYucXVlcnlTZWxlY3RvcihcImhleFwiKTtcblxuXHRcdGIuaW5uZXJIVE1MID0gcGFkKG0udG9TdHJpbmcoMiksIGwpO1xuXHRcdGguaW5uZXJIVE1MID0gcGFkKG0udG9TdHJpbmcoMTYpLCBNYXRoLmNlaWwobCAvIDQpKTtcblx0fSlcbn1cblxuVGFtYWdvLnByb3RvdHlwZS5yZWZyZXNoX2RlYnVnZ2VyID0gZnVuY3Rpb24gKCkge1xuXHR2YXIgdGhhdCA9IHRoaXM7XG5cblx0Ly8gVXBkYXRlIGJhc2ljIHZpZXdzXG5cdG9iamVjdC5lYWNoKHRoaXMuYm9keS5yZWdpc3RlcnMsIGZ1bmN0aW9uIChlbGVtLCByZWdpc3Rlcikge1xuXHRcdGVsZW0uaW5uZXJIVE1MID0gdG9IZXgoMiwgdGhhdC5zeXN0ZW1bcmVnaXN0ZXJdKTtcblx0fSk7XG5cblx0b2JqZWN0LmVhY2godGhpcy5ib2R5LmZsYWdzLCBmdW5jdGlvbiAoZWxlbSwgZmxhZykge1xuXHRcdGVsZW0uY2xhc3NMaXN0LnRvZ2dsZShcImFjdGl2ZVwiLCBCb29sZWFuKHRoYXQuc3lzdGVtW2ZsYWddKSk7XG5cdH0pO1xuXG5cdHRoaXMuYm9keS5tZW1vcnkuZm9yRWFjaChmdW5jdGlvbiAobSwgaSkge1xuXHRcdG0uaW5uZXJIVE1MID0gdG9IZXgoMiwgdGhhdC5zeXN0ZW0uX3dyYW1baV0pO1xuXHR9KTtcblxuXHR0aGlzLmJvZHkuY29udHJvbC5mb3JFYWNoKGZ1bmN0aW9uIChtLCBpKSB7XG5cdFx0dmFyIGFjYyA9IHRoYXQuc3lzdGVtLl9jcHVhY2NbaSsweDMwMDBdO1xuXHRcdHRoYXQuc3lzdGVtLl9jcHVhY2NbaSsweDMwMDBdID0gMDtcblx0XHRtLmNsYXNzTGlzdC50b2dnbGUoJ3JlYWQnLCBhY2MgJiB0YW1hZ290Y2hpLkFDQ0VTU19SRUFEKTtcblx0XHRtLmNsYXNzTGlzdC50b2dnbGUoJ3dyaXRlJywgYWNjICYgdGFtYWdvdGNoaS5BQ0NFU1NfV1JJVEUpO1xuXHRcdG0uaW5uZXJIVE1MID0gdG9IZXgoMiwgdGhhdC5zeXN0ZW0uX2NwdXJlZ1tpXSk7XG5cdH0pO1xuXG5cblx0dmFyIGRpc2FzbSA9IGRpc2Fzc2VtYmxlLmRpc2Fzc2VtYmxlKGNvbmZpZy5pbnN0cnVjdGlvbkNvdW50LCB0aGlzLl9kaXNhc21PZmZzZXQsIHRoaXMuc3lzdGVtKSxcblx0XHRiaWFzID0gTWF0aC5mbG9vcihjb25maWcuaW5zdHJ1Y3Rpb25Db3VudCAvIDIpLFxuXHRcdGN1cnJlbnQgPSBkaXNhc20ucmVkdWNlKGZ1bmN0aW9uKGFjYywgZCwgaSl7IHJldHVybiBkLmFjdGl2ZSA/IGkgOiBhY2M7IH0sIG51bGwpO1xuXG5cdC8vIFBDIGlzbid0IHdlcmUgaXQgc2hvdWxkIGJlXG5cdGlmIChjdXJyZW50ID09PSBudWxsKSB7XG5cdFx0dGhpcy5fZGlzYXNtT2Zmc2V0ID0gdGhpcy5zeXN0ZW0ucGM7XG5cdFx0ZGlzYXNtID0gZGlzYXNzZW1ibGUuZGlzYXNzZW1ibGUoY29uZmlnLmluc3RydWN0aW9uQ291bnQsIHRoaXMuX2Rpc2FzbU9mZnNldCwgdGhpcy5zeXN0ZW0pO1xuXHR9IGVsc2UgaWYgKGN1cnJlbnQgPj0gYmlhcyAmJiBkaXNhc20ubGVuZ3RoID09IGNvbmZpZy5pbnN0cnVjdGlvbkNvdW50KSB7XG5cdFx0dGhpcy5fZGlzYXNtT2Zmc2V0ID0gZGlzYXNtW2N1cnJlbnQtYmlhc10ubG9jYXRpb247XG5cdFx0ZGlzYXNtID0gZGlzYXNzZW1ibGUuZGlzYXNzZW1ibGUoY29uZmlnLmluc3RydWN0aW9uQ291bnQsIHRoaXMuX2Rpc2FzbU9mZnNldCwgdGhpcy5zeXN0ZW0pO1xuXHR9XG5cblx0ZGlzYXNtLmZvckVhY2goZnVuY3Rpb24gKGcsIGkpIHtcblx0XHR2YXIgcm93ID0gdGhhdC5ib2R5Lmluc3RydWN0aW9uc1tpXTtcblxuXHRcdHJvdy5sb2NhdGlvbi5pbm5lckhUTUwgPSB0b0hleCg0LCBnLmxvY2F0aW9uKVxuXHRcdHJvdy5vcGNvZGUuaW5uZXJIVE1MID0gZy5pbnN0cnVjdGlvbjtcblx0XHRyb3cuYWRkcmVzc2luZy5pbm5lckhUTUwgPSAoKGcuZGF0YSA9PT0gbnVsbCkgPyBcIlwiIDogZy5kYXRhKS50b1N0cmluZygxNikudG9VcHBlckNhc2UoKTtcblx0XHRyb3cuZGF0YS5pbm5lckhUTUwgPSBnLmJ5dGVzO1xuXG5cdFx0ZnVuY3Rpb24gYXR0cihub2RlLCBhdHRyLCB2YWx1ZSkge1xuXHRcdFx0aWYodmFsdWUgIT09IHVuZGVmaW5lZCkgeyBub2RlLnNldEF0dHJpYnV0ZShhdHRyLCB2YWx1ZSkgfVxuXHRcdFx0ZWxzZSBub2RlLnJlbW92ZUF0dHJpYnV0ZShhdHRyKTtcblx0XHR9XG5cblx0XHRyb3cuaW5zdHJ1Y3Rpb24uY2xhc3NMaXN0LnRvZ2dsZShcImFjdGl2ZVwiLCBnLmFjdGl2ZSA9PT0gdHJ1ZSk7XG5cdFx0YXR0cihyb3cuYWRkcmVzc2luZywgJ21vZGUnLCBnLm1vZGUpO1xuXHRcdGF0dHIocm93LmFkZHJlc3NpbmcsICdhZGRyZXNzJywgKGcuYWRkcmVzcyB8fCAwKS50b1N0cmluZygxNikudG9VcHBlckNhc2UoKSk7XG5cdFx0YXR0cihyb3cuaW5zdHJ1Y3Rpb24sICdwb3J0JywgZy5wb3J0KTtcblx0fSk7XG5cblx0Zm9yICh2YXIgaSA9IGRpc2FzbS5sZW5ndGg7IGkgPCBjb25maWcuaW5zdHJ1Y3Rpb25Db3VudDsgaSsrKSB7XG5cdFx0dmFyIHJvdyA9IHRoYXQuYm9keS5pbnN0cnVjdGlvbnNbaV07XG5cblx0XHRyb3cubG9jYXRpb24uaW5uZXJIVE1MID0gXCJcIjtcblx0XHRyb3cub3Bjb2RlLmlubmVySFRNTCA9IFwiXCI7XG5cdFx0cm93LmFkZHJlc3NpbmcuaW5uZXJIVE1MID0gXCJcIjtcblx0XHRyb3cuZGF0YS5pbm5lckhUTUwgPSBcIlwiO1xuXHRcdHJvdy5hZGRyZXNzaW5nLnJlbW92ZUF0dHJpYnV0ZSgnbW9kZScpO1xuXHR9XG5cblx0dGhpcy5yZWZyZXNoX3BvcnQoKTtcblx0dGhpcy5yZWZyZXNoX3NpbXBsZSgpO1xufVxuXG5UYW1hZ28ucHJvdG90eXBlLmNvbmZpZ3VyZSA9IGZ1bmN0aW9uKGVsZW1lbnQpIHtcblx0dmFyIGRhdGEgPSBPYmplY3QuY3JlYXRlKGNvbmZpZyksXG5cdFx0dGhhdCA9IHRoaXM7XG5cblx0ZGF0YS50b0hleCA9IHRvSGV4O1xuXHRkYXRhLnJhbUJ5dGVzID0gdGhpcy5zeXN0ZW0uX3dyYW0ubGVuZ3RoO1xuXHRkYXRhLnJlZ2lzdGVyQnl0ZXMgPSB0aGlzLnN5c3RlbS5fY3B1cmVnLmxlbmd0aDtcblxuXHRkYXRhLmRlYnVnID0gQm9vbGVhbihlbGVtZW50LmF0dHJpYnV0ZXMuZGVidWdnZXIpO1xuXG5cdGVsZW1lbnQuaW5uZXJIVE1MID0gbWFpblRlbXBsYXRlKGRhdGEpO1xuXG5cdGZ1bmN0aW9uIG5vb3BIYW5kbGVyKGV2dCkge1xuXHRcdGV2dC5zdG9wUHJvcGFnYXRpb24oKTtcblx0XHRldnQucHJldmVudERlZmF1bHQoKTtcblx0fVxuXG5cdGVsZW1lbnQuYWRkRXZlbnRMaXN0ZW5lcihcImRyYWdlbnRlclwiLCBub29wSGFuZGxlciwgZmFsc2UpO1xuXHRlbGVtZW50LmFkZEV2ZW50TGlzdGVuZXIoXCJkcmFnZXhpdFwiLCBub29wSGFuZGxlciwgZmFsc2UpO1xuXHRlbGVtZW50LmFkZEV2ZW50TGlzdGVuZXIoXCJkcmFnb3ZlclwiLCBub29wSGFuZGxlciwgZmFsc2UpO1xuXHRlbGVtZW50LmFkZEV2ZW50TGlzdGVuZXIoXCJkcm9wXCIsIHRoaXMuZHJvcC5iaW5kKHRoaXMpLCBmYWxzZSk7XG5cblx0Ly8gQmluZCB0byBIVE1MXG5cdGlmIChkYXRhLmRlYnVnKSB7XG5cdFx0W10uZm9yRWFjaC5jYWxsKGRvY3VtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoXCJpbnB1dFt0eXBlPWJ1dHRvbl1cIiksIGZ1bmN0aW9uIChlbCkge1xuXHRcdFx0ZWwuYWRkRXZlbnRMaXN0ZW5lcihcImNsaWNrXCIsIHRoYXRbZWwuYXR0cmlidXRlcy5hY3Rpb24udmFsdWVdLmJpbmQodGhhdCkpXG5cdFx0fSk7XG5cblx0XHR0aGlzLmJvZHkgPSB7XG5cdFx0XHRnbHlwaHM6IGVsZW1lbnQucXVlcnlTZWxlY3RvckFsbChcImkuZ2x5cGhcIiksXG5cdFx0XHRwb3J0OiBlbGVtZW50LnF1ZXJ5U2VsZWN0b3IoXCJwb3J0XCIpLFxuXHRcdFx0c2VsZWN0czogW10ucmVkdWNlLmNhbGwoZWxlbWVudC5xdWVyeVNlbGVjdG9yQWxsKFwic2VsZWN0XCIpLCBmdW5jdGlvbiAoYWNjLCBmKSB7IFxuXHRcdFx0XHRhY2NbZi5hdHRyaWJ1dGVzLmFjdGlvbi52YWx1ZS50b0xvd2VyQ2FzZSgpXSA9IGY7XG5cdFx0XHRcdHJldHVybiBhY2M7IFxuXHRcdFx0fSwge30pLFxuXHRcdFx0ZmxhZ3M6IFtdLnJlZHVjZS5jYWxsKGVsZW1lbnQucXVlcnlTZWxlY3RvckFsbChcImZsYWdcIiksIGZ1bmN0aW9uIChhY2MsIGYpIHsgXG5cdFx0XHRcdGFjY1tmLmF0dHJpYnV0ZXMubmFtZS52YWx1ZS50b0xvd2VyQ2FzZSgpXSA9IGY7XG5cdFx0XHRcdHJldHVybiBhY2M7IFxuXHRcdFx0fSwge30pLFxuXHRcdFx0cmVnaXN0ZXJzOiBbXS5yZWR1Y2UuY2FsbChlbGVtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoXCJyZWdpc3RlclwiKSwgZnVuY3Rpb24gKGFjYywgcikgeyBcblx0XHRcdFx0YWNjW3IuYXR0cmlidXRlcy5uYW1lLnZhbHVlLnRvTG93ZXJDYXNlKCldID0gcjtcblx0XHRcdFx0cmV0dXJuIGFjYzsgXG5cdFx0XHR9LCB7fSksXG5cdFx0XHRpbnN0cnVjdGlvbnM6IFtdLm1hcC5jYWxsKGVsZW1lbnQucXVlcnlTZWxlY3RvckFsbChcImluc3RydWN0aW9uXCIpLCBmdW5jdGlvbiAoaSkge1xuXHRcdFx0XHRyZXR1cm4ge1xuXHRcdFx0XHRcdGluc3RydWN0aW9uOiBpLFxuXHRcdFx0XHRcdGxvY2F0aW9uOiBpLnF1ZXJ5U2VsZWN0b3IoXCJsb2NhdGlvblwiKSxcblx0XHRcdFx0XHRvcGNvZGU6IGkucXVlcnlTZWxlY3RvcihcIm9wY29kZVwiKSxcblx0XHRcdFx0XHRkYXRhOiBpLnF1ZXJ5U2VsZWN0b3IoXCJkYXRhXCIpLFxuXHRcdFx0XHRcdGFkZHJlc3Npbmc6IGkucXVlcnlTZWxlY3RvcihcImFkZHJlc3NpbmdcIiksXG5cdFx0XHRcdH07XG5cdFx0XHR9KSxcblx0XHRcdGNvbnRyb2w6IFtdLm1hcC5jYWxsKGVsZW1lbnQucXVlcnlTZWxlY3RvckFsbChcImNvbnRyb2wgYnl0ZVwiKSwgZnVuY3Rpb24gKGIpIHtcblx0XHRcdFx0Yi5hZGRFdmVudExpc3RlbmVyKFwiY2xpY2tcIiwgdGhhdC51cGRhdGVfY29udHJvbC5iaW5kKHRoYXQpKTtcblxuXHRcdFx0XHRyZXR1cm4gYjtcblx0XHRcdH0pLFxuXHRcdFx0bWVtb3J5OiBbXS5tYXAuY2FsbChlbGVtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoXCJtZW1vcnkgYnl0ZVwiKSwgZnVuY3Rpb24gKGIpIHtcblx0XHRcdFx0cmV0dXJuIGI7XG5cdFx0XHR9KSxcblx0XHRcdGRpc3BsYXk6IGVsZW1lbnQucXVlcnlTZWxlY3RvcihcImRpc3BsYXkgY2FudmFzXCIpLmdldENvbnRleHQoXCIyZFwiKSxcblx0XHRcdGZpZ3VyZTogZWxlbWVudC5xdWVyeVNlbGVjdG9yKFwiZGlzcGxheSBmaWd1cmVcIilcblx0XHR9O1xuXG5cdFx0ZG9jdW1lbnQucXVlcnlTZWxlY3RvcihcInNlbGVjdFthY3Rpb249ZmlndXJlXVwiKS5hZGRFdmVudExpc3RlbmVyKFwiY2hhbmdlXCIsIGZ1bmN0aW9uKGUpIHtcblx0XHRcdHRoYXQuc3lzdGVtLmluc2VydGVkX2ZpZ3VyZSA9IE51bWJlcihlLnRhcmdldC52YWx1ZSk7XG5cdFx0fSk7XG5cblx0XHR0aGlzLl9kZWJ1Z19wb3J0ID0gMHgzMDAwO1xuXHRcdHRoaXMudXBkYXRlX2NvbnRyb2woKTtcblxuXHRcdHRoaXMucmVmcmVzaCA9IHRoaXMucmVmcmVzaF9kZWJ1Z2dlcjtcblx0fSBlbHNlIHtcblx0XHR0aGlzLmJvZHkgPSB7IFxuXHRcdFx0Z2x5cGhzOiBlbGVtZW50LnF1ZXJ5U2VsZWN0b3JBbGwoXCJpLmdseXBoXCIpLFxuXHRcdFx0ZGlzcGxheTogZWxlbWVudC5xdWVyeVNlbGVjdG9yKFwiZGlzcGxheSBjYW52YXNcIikuZ2V0Q29udGV4dChcIjJkXCIpLFxuXHRcdFx0ZmlndXJlOiBlbGVtZW50LnF1ZXJ5U2VsZWN0b3IoXCJkaXNwbGF5IGZpZ3VyZVwiKVxuXHRcdH07XG5cblx0XHR0aGlzLnJlZnJlc2ggPSB0aGlzLnJlZnJlc2hfc2ltcGxlO1xuXHRcdC8vIFN0YXJ0IHJ1bm5pbmcgc29vblxuXHRcdHNldFRpbWVvdXQoZnVuY3Rpb24oKSB7IHRoYXQucnVuKCk7IH0sIDEwKTtcblx0fVxufTtcblxubW9kdWxlLmV4cG9ydHMgPSB7XG5cdHN0YXJ0OiBzdGFydFxufTtcbiIsIm1vZHVsZS5leHBvcnRzPShmdW5jdGlvbigpIHt2YXIgdCA9IGZ1bmN0aW9uIGFub255bW91cyhsb2NhbHMsIGZpbHRlcnMsIGVzY2FwZSwgcmV0aHJvdykge1xuZXNjYXBlID0gZXNjYXBlIHx8IGZ1bmN0aW9uIChodG1sKXtcbiAgcmV0dXJuIFN0cmluZyhodG1sKVxuICAgIC5yZXBsYWNlKC8mKD8hIz9bYS16QS1aMC05XSs7KS9nLCAnJmFtcDsnKVxuICAgIC5yZXBsYWNlKC88L2csICcmbHQ7JylcbiAgICAucmVwbGFjZSgvPi9nLCAnJmd0OycpXG4gICAgLnJlcGxhY2UoLycvZywgJyYjMzk7JylcbiAgICAucmVwbGFjZSgvXCIvZywgJyZxdW90OycpO1xufTtcbnZhciBidWYgPSBbXTtcbndpdGggKGxvY2FscyB8fCB7fSkgeyAoZnVuY3Rpb24oKXsgXG4gYnVmLnB1c2goJzxjb2x1bW4+XFxuXHQ8ZGlzcGxheT5cXG5cdFx0PGZpZ3VyZT48L2ZpZ3VyZT5cXG5cdFx0PGRpdj5cXG5cdFx0XHQ8aSBjbGFzcz1cImljb24gaWNvbi1kYXNoYm9hcmQgZ2x5cGhcIj48L2k+XFxuXHRcdFx0PGkgY2xhc3M9XCJpY29uIGljb24tZm9vZCBnbHlwaFwiPjwvaT5cXG5cdFx0XHQ8aSBjbGFzcz1cImljb24gaWNvbi10cmFzaCBnbHlwaFwiPjwvaT5cXG5cdFx0XHQ8aSBjbGFzcz1cImljb24gaWNvbi1nbG9iZSBnbHlwaFwiPjwvaT5cXG5cdFx0XHQ8aSBjbGFzcz1cImljb24gaWNvbi11c2VyIGdseXBoXCI+PC9pPlxcblx0XHQ8L2Rpdj5cXG5cdFx0PGNhbnZhcyB3aWR0aD1cIjQ4XCIgaGVpZ2h0PVwiMzFcIj48L2NhbnZhcz5cXG5cdFx0PGRpdj5cXG5cdFx0XHQ8aSBjbGFzcz1cImljb24gaWNvbi1jb21tZW50cyBnbHlwaFwiPjwvaT5cXG5cdFx0XHQ8aSBjbGFzcz1cImljb24gaWNvbi1tZWRraXQgZ2x5cGhcIj48L2k+XFxuXHRcdFx0PGkgY2xhc3M9XCJpY29uIGljb24taGVhcnQgZ2x5cGhcIj48L2k+XFxuXHRcdFx0PGkgY2xhc3M9XCJpY29uIGljb24tYm9vayBnbHlwaFwiPjwvaT5cXG5cdFx0XHQ8aSBjbGFzcz1cImljb24gaWNvbi1iZWxsIGdseXBoXCI+PC9pPlxcblx0XHQ8L2Rpdj5cXG5cdDwvZGlzcGxheT5cXG5cdFxcblx0Jyk7MjE7IGlmIChkZWJ1ZykgeyA7IGJ1Zi5wdXNoKCdcXG5cdFx0PGJ1dHRvbnM+XFxuXHRcdFx0PGlucHV0IHR5cGU9XCJidXR0b25cIiB2YWx1ZT1cInN0ZXBcIiBhY3Rpb249XCJzdGVwXCI+PC9pbnB1dD5cXG5cdFx0XHQ8aW5wdXQgdHlwZT1cImJ1dHRvblwiIHZhbHVlPVwicnVuXCIgYWN0aW9uPVwicnVuXCI+PC9pbnB1dD5cXG5cdFx0XHQ8aW5wdXQgdHlwZT1cImJ1dHRvblwiIHZhbHVlPVwicmVzZXRcIiBhY3Rpb249XCJyZXNldFwiPjwvaW5wdXQ+XFxuXHRcdFx0PGlucHV0IHR5cGU9XCJidXR0b25cIiB2YWx1ZT1cIm5taVwiIGFjdGlvbj1cIm5taVwiPjwvaW5wdXQ+XFxuXHRcdDwvYnV0dG9ucz5cXG5cdFx0PGJ1dHRvbnM+XFxuXHRcdFx0PHNlbGVjdCBhY3Rpb249XCJmaWd1cmVcIj5cXG5cdFx0XHRcdDxvcHRpb24gdmFsdWU9XCIwXCI+Tm8gRmlndXJlPC9vcHRpb24+XFxuXHRcdFx0XHQ8b3B0aW9uIHZhbHVlPVwiMVwiPkZpZzE8L29wdGlvbj5cXG5cdFx0XHRcdDxvcHRpb24gdmFsdWU9XCIyXCI+RmlnMjwvb3B0aW9uPlxcblx0XHRcdFx0PG9wdGlvbiB2YWx1ZT1cIjNcIj5GaWczPC9vcHRpb24+XFxuXHRcdFx0PC9zZWxlY3Q+XFxuXFxuXHRcdFx0PHNlbGVjdCBhY3Rpb249XCJpcnFcIj5cXG5cdFx0XHRcdDxvcHRpb24gdmFsdWU9XCIwXCI+MDogVElNMDwvb3B0aW9uPlxcblx0XHRcdFx0PG9wdGlvbiB2YWx1ZT1cIjNcIj4zOiAyMDQ4PC9vcHRpb24+XFxuXHRcdFx0XHQ8b3B0aW9uIHZhbHVlPVwiNFwiPjQ6IDgxOTI8L29wdGlvbj5cXG5cdFx0XHRcdDxvcHRpb24gdmFsdWU9XCI1XCI+NTogU1BVPC9vcHRpb24+XFxuXHRcdFx0XHQ8b3B0aW9uIHZhbHVlPVwiNlwiPjY6IFNQSTwvb3B0aW9uPlxcblx0XHRcdFx0PG9wdGlvbiB2YWx1ZT1cIjdcIj43OiBGUDwvb3B0aW9uPlxcblx0XHRcdFx0PG9wdGlvbiB2YWx1ZT1cIjEwXCI+MTA6IFRJTTE8L29wdGlvbj5cXG5cdFx0XHRcdDxvcHRpb24gdmFsdWU9XCIxMlwiPjEyOiBUQkg8L29wdGlvbj5cXG5cdFx0XHRcdDxvcHRpb24gdmFsdWU9XCIxM1wiPjEzOiBUQkw8L29wdGlvbj5cXG5cdFx0XHQ8L3NlbGVjdD5cXG5cdFx0XHQ8aW5wdXQgYWN0aW9uPVwiaXJxXCIgdHlwZT1cImJ1dHRvblwiIHZhbHVlPVwiaXJxXCI+PC9pbnB1dD5cXG5cdFx0PC9idXR0b25zPlxcblx0XHQ8Y3B1Plxcblx0XHRcdDxmbGFncz5cXG5cdFx0XHRcdDxmbGFnIG5hbWU9XCJDXCI+PC9mbGFnPlxcblx0XHRcdFx0PGZsYWcgbmFtZT1cIlpcIj48L2ZsYWc+XFxuXHRcdFx0XHQ8ZmxhZyBuYW1lPVwiSVwiPjwvZmxhZz5cXG5cdFx0XHRcdDxmbGFnIG5hbWU9XCJEXCI+PC9mbGFnPlxcblx0XHRcdFx0PGZsYWcgbmFtZT1cIlZcIj48L2ZsYWc+XFxuXHRcdFx0XHQ8ZmxhZyBuYW1lPVwiTlwiPjwvZmxhZz5cXG5cdFx0XHQ8L2ZsYWdzPlxcblxcblx0XHRcdDxyZWdpc3RlcnM+XFxuXHRcdFx0XHQ8cmVnaXN0ZXIgbmFtZT1cIkFcIj48L3JlZ2lzdGVyPlxcblx0XHRcdFx0PHJlZ2lzdGVyIG5hbWU9XCJYXCI+PC9yZWdpc3Rlcj5cXG5cdFx0XHRcdDxyZWdpc3RlciBuYW1lPVwiWVwiPjwvcmVnaXN0ZXI+XFxuXHRcdFx0XHQ8cmVnaXN0ZXIgbmFtZT1cIlNcIj48L3JlZ2lzdGVyPlxcblx0XHRcdFx0PHJlZ2lzdGVyIG5hbWU9XCJQQ1wiPjwvcmVnaXN0ZXI+XFxuXHRcdFx0PC9yZWdpc3RlcnM+XFxuXHRcdDwvY3B1Plxcblx0XHQ8Y29udHJvbD5cXG5cdFx0XHQnKTs2ODsgZm9yICh2YXIgaSA9IDA7IGkgPCByZWdpc3RlckJ5dGVzOyBpICs9IHJlZ2lzdGVyQnl0ZXNQZXJMaW5lICkgeyA7IGJ1Zi5wdXNoKCdcXG5cdFx0XHRcdDxyb3c+XFxuXHRcdFx0XHRcdDxhZGRyZXNzPicsIGVzY2FwZSgoNzAsICB0b0hleCg0LCBpKzB4MzAwMCkgKSksICc8L2FkZHJlc3M+XFxuXHRcdFx0XHRcdCcpOzcxOyBmb3IgKHZhciBiID0gMDsgYiA8IHJlZ2lzdGVyQnl0ZXNQZXJMaW5lOyBiICsrICkgeyA7IGJ1Zi5wdXNoKCdcXG5cdFx0XHRcdFx0XHQ8Ynl0ZSBkYXRhLWFkZHJlc3M9XCInLCBlc2NhcGUoKDcyLCAgaStiKzB4MzAwMCApKSwgJ1wiPjwvYnl0ZT5cXG5cdFx0XHRcdFx0Jyk7NzM7IH0gOyBidWYucHVzaCgnXFxuXHRcdFx0XHQ8L3Jvdz5cXG5cdFx0XHQnKTs3NTsgfSA7IGJ1Zi5wdXNoKCdcXG5cdFx0PC9jb250cm9sPlxcblx0PC9jb2x1bW4+XFxuXHQ8Y29sdW1uPlxcblx0XHQ8ZGlzYXNzZW1ibHk+XFxuXHRcdFx0Jyk7ODA7IGZvciAodmFyIGkgPSAwOyBpIDwgaW5zdHJ1Y3Rpb25Db3VudDsgaSsrICkgeyA7IGJ1Zi5wdXNoKCdcXG5cdFx0XHQ8aW5zdHJ1Y3Rpb24gcG9ydD5cXG5cdFx0XHRcdDxsb2NhdGlvbj48L2xvY2F0aW9uPlxcblx0XHRcdFx0PG9wY29kZT48L29wY29kZT5cXG5cdFx0XHRcdDxhZGRyZXNzaW5nIG1vZGUgYWRkcmVzcz48L2FkZHJlc3Npbmc+XFxuXHRcdFx0XHQ8ZGF0YT48L2RhdGE+XFxuXHRcdFx0PC9pbnN0cnVjdGlvbj5cXG5cdFx0XHQnKTs4NzsgfSA7IGJ1Zi5wdXNoKCdcXG5cdFx0PC9kaXNhc3NlbWJseT5cXG5cdDwvY29sdW1uPlxcblx0PGNvbHVtbj5cXG5cdFx0PHBvcnQ+PC9wb3J0Plxcblx0XHQ8bWVtb3J5Plxcblx0XHRcdCcpOzkzOyBmb3IgKHZhciBpID0gMDsgaSA8IHJhbUJ5dGVzOyBpICs9IG1lbW9yeUJ5dGVzUGVyTGluZSApIHsgOyBidWYucHVzaCgnXFxuXHRcdFx0XHQ8cm93Plxcblx0XHRcdFx0XHQ8YWRkcmVzcz4nLCBlc2NhcGUoKDk1LCAgdG9IZXgoNCwgaSkgKSksICc8L2FkZHJlc3M+XFxuXHRcdFx0XHRcdCcpOzk2OyBmb3IgKHZhciBiID0gMDsgYiA8IG1lbW9yeUJ5dGVzUGVyTGluZTsgYiArKyApIHsgOyBidWYucHVzaCgnXFxuXHRcdFx0XHRcdFx0PGJ5dGUgZGF0YS1hZGRyZXNzPVwiJywgZXNjYXBlKCg5NywgIGkrYiApKSwgJ1wiPjwvYnl0ZT5cXG5cdFx0XHRcdFx0Jyk7OTg7IH0gOyBidWYucHVzaCgnXFxuXHRcdFx0XHQ8L3Jvdz5cXG5cdFx0XHQnKTsxMDA7IH0gOyBidWYucHVzaCgnXFxuXHRcdDwvbWVtb3J5Plxcblx0Jyk7MTAyOyB9IDsgYnVmLnB1c2goJ1xcbjwvY29sdW1uPicpOyB9KSgpO1xufSBcbnJldHVybiBidWYuam9pbignJyk7XG59OyByZXR1cm4gZnVuY3Rpb24obCkgeyByZXR1cm4gdChsKSB9fSgpKSIsIm1vZHVsZS5leHBvcnRzPShmdW5jdGlvbigpIHt2YXIgdCA9IGZ1bmN0aW9uIGFub255bW91cyhsb2NhbHMsIGZpbHRlcnMsIGVzY2FwZSwgcmV0aHJvdykge1xuZXNjYXBlID0gZXNjYXBlIHx8IGZ1bmN0aW9uIChodG1sKXtcbiAgcmV0dXJuIFN0cmluZyhodG1sKVxuICAgIC5yZXBsYWNlKC8mKD8hIz9bYS16QS1aMC05XSs7KS9nLCAnJmFtcDsnKVxuICAgIC5yZXBsYWNlKC88L2csICcmbHQ7JylcbiAgICAucmVwbGFjZSgvPi9nLCAnJmd0OycpXG4gICAgLnJlcGxhY2UoLycvZywgJyYjMzk7JylcbiAgICAucmVwbGFjZSgvXCIvZywgJyZxdW90OycpO1xufTtcbnZhciBidWYgPSBbXTtcbndpdGggKGxvY2FscyB8fCB7fSkgeyAoZnVuY3Rpb24oKXsgXG4gYnVmLnB1c2goJzxoMT4nLCBlc2NhcGUoKDEsICBuYW1lICkpLCAnICgweCcsIGVzY2FwZSgoMSwgIGFkZHJlc3MgKSksICcpPC9oMT5cXG5cXG4nKTszOyBmb3IgKHZhciBpID0gMDsgaSA8IGZpZWxkcy5sZW5ndGg7IGkrKyApIHsgdmFyIGZpZWxkID0gZmllbGRzW2ldIDsgYnVmLnB1c2goJ1xcblx0PGZpZWxkIG5hbWU9XCInLCBlc2NhcGUoKDQsICBmaWVsZC5uYW1lICkpLCAnXCIgZGF0YS1zdGFydD1cIicsIGVzY2FwZSgoNCwgIGZpZWxkLnN0YXJ0ICkpLCAnXCIgZGF0YS1sZW5ndGg9XCInLCBlc2NhcGUoKDQsICBmaWVsZC5sZW5ndGggKSksICdcIj5cXG5cdFx0PHJhbmdlPiBcXG5cdFx0XHRbJywgZXNjYXBlKCg2LCAgZmllbGQuc3RhcnQgKSksICcnKTs2OyBpZihmaWVsZC5sZW5ndGggPiAxKSB7IDsgYnVmLnB1c2goJzonLCBlc2NhcGUoKDYsICBmaWVsZC5sZW5ndGggKyBmaWVsZC5zdGFydCAtIDEgKSksICcnKTs2OyB9IDsgYnVmLnB1c2goJ11cXG5cdFx0PC9yYW5nZT5cXG5cdFx0PGhleD48L2hleD5cXG5cdFx0PGJpbj48L2Jpbj5cXG5cdDwvZmllbGQ+XFxuJyk7MTE7IH0gOyBidWYucHVzaCgnICAgJyk7IH0pKCk7XG59IFxucmV0dXJuIGJ1Zi5qb2luKCcnKTtcbn07IHJldHVybiBmdW5jdGlvbihsKSB7IHJldHVybiB0KGwpIH19KCkpIiwiZnVuY3Rpb24gZWFjaChvLCBjYikge1xuXHRpZiAoQXJyYXkuaXNBcnJheShvKSkgeyBvLmZvckVhY2goY2IpOyB9XG5cdE9iamVjdC5nZXRPd25Qcm9wZXJ0eU5hbWVzKG8pLmZvckVhY2goZnVuY3Rpb24gKG4pIHsgY2Iob1tuXSwgbiwgbyk7IH0pXG59XG5cbmZ1bmN0aW9uIGV4dGVuZChhLCBiKSB7XG5cdGVhY2goYiwgZnVuY3Rpb24gKHYsIGspIHsgYVtrXSA9IHY7IH0pXG59XG5cbmZ1bmN0aW9uIGZpbGwoYywgdikge1xuXHR2YXIgYSA9IFtdOyBcblx0aWYgKHYgPT09IHVuZGVmaW5lZCkgeyB2ID0gMDsgfVxuXHR3aGlsZShjLS0gPiAwKSB7IGEucHVzaCgwKTsgfVxuXHRyZXR1cm4gYTtcbn1cblxubW9kdWxlLmV4cG9ydHMgPSAge1xuXHRlYWNoOiBlYWNoLFxuXHRleHRlbmQ6IGV4dGVuZCxcblx0ZmlsbDogZmlsbFxufTtcbiJdfQ==
;
