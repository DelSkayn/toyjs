
function a(){
    b+=1;
}
var b = 2;
a();
assert(b === 3);
function c(){
    function d(){
        e += 1;
        f += 1;
    }
    var e = 2;
    d();
    assert(e === 3);
}
let f = 2;
c();
assert(f === 3);
