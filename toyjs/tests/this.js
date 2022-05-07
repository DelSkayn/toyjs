function Foo(bar){
    this.faz = bar
}
let f = new Foo("baz");
assert(f.faz === "baz")
Foo.prototype.toString = function(){
    return this.faz + "z";
}
assert(f.toString() === "bazz");
