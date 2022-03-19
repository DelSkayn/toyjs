
function Foo(){
    this.bar = "baz";
    this.faz = "far";
}

let cla = new Foo();
console.log(Foo.prototype);
console.log(Foo.prototype.constructor);
console.log(cla.bar);
console.log(cla.faz);
