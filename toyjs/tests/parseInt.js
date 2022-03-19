assert(parseInt("1") === 1);
assert(parseInt("0x1") === 1);
assert(parseInt("0xc") === 12);
assert(parseInt("-0xc") === -12);
assert(parseInt("-11",2) === -3);
assert(parseInt("+66",8) === 54);
assert(isNaN());
assert(isNaN(parseInt("+ff",37)));
assert(isNaN(parseInt("+ff",1)));
