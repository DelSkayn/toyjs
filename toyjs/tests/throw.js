try{
    (function(){
        throw "b";
        assert(false);
    })()
}catch(e){
    assert(e === "b");
}

function nested(){
    try{
        (function(){
            (function(){
                let a = {};
                (function(){
                    assert(a);
                    throw "d";
                    assert(false);
                })()
            })()
        })()
    }catch(e){
        assert(e === "d");
    }finally{
        return
    }
    assert(false);
}
nested();

let acc = "";

try{
    acc += "a"
}catch(e){
    acc += "b"
}finally{
    acc += "c"
}
assert(acc === "ac")
true
