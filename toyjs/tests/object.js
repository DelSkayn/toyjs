
let a = Object.create(Object.prototype,{a: { 
    get(){ 
        return 1;
    }
    set(a){ 
        this.b = a 
    }
}});
assert(a.a === 1)
assert(a.b === undefined)
a.a = 3
assert(a.b === 3)

