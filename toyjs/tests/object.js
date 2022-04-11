let a = Object.create(Object.prototype,{a: { 
    get(){ 
        return 1;
    }
    set(a){ 
        this.b = a 
    }
}});
assert(a.a === 1,"a.a not equal to 1")
assert(a.b === undefined,"a.b is not undefined")
a.a = 3
assert(a.b === 3,"a.b not equal to 3")

let b = {a: 1};
assert(!Object.isFrozen(b),"object is frozen before being frozen");
assert(!Object.isSealed(b),"object is sealed before being selad");
Object.seal(b);
assert(!Object.isFrozen(b),"object is frozen after sealed");
assert(Object.isSealed(b),"object is not sealed after sealed");
Object.freeze(b);
assert(Object.isFrozen(b),"object is not frozen after freeze");
assert(Object.isSealed(b),"object is not sealed after freeze");
