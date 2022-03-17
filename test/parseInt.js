console.log(parseInt("1") === 1);
console.log(parseInt("0x1") === 1);
console.log(parseInt("0xc") === 12);
console.log(parseInt("-0xc") === -12);
console.log(parseInt("-11",2) === -3);
console.log(parseInt("+66",8) === 54);
console.log(isNaN());
console.log(isNaN(parseInt("+ff",37)));
console.log(isNaN(parseInt("+ff",1)));
