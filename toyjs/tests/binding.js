
let obj = {
    a: 1,
    b: 2,
    c: 3,
    d: [4,5,6]
};
let { b, c: e, d:[,f]} = obj;

function g({a: h}) {
    assert(h === 1,"h != 1");
}
g(obj);

assert(b === 2,"b != 2");
assert(e === 3,"c != 3");
assert(f === 5,"f != 5");