function Foo(){
    this.bar = 1;
    this.faz = 2;
}
Foo.prototype.baz = 3;
let cla = new Foo();
assert(Foo.prototype !== undefined);
assert(Foo.prototype.constructor !== undefined);
assert(cla.bar === 1);
assert(cla.faz === 2);
assert(cla.baz === 3);
