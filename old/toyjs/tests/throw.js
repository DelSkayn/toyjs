try{
    (function(){
        throw "b";
        assert(false, "throw did not throw");
    })()
}catch(e){
    assert(e === "b","retrieved wrong value in catch");
}

function nested(){
    try{
        (function(){
            (function(){
                let a = {};
                (function(){
                    assert(a);
                    throw "d";
                    assert(false, "throw did not throw");
                })()
            })()
        })()
    }catch(e){
        assert(e === "d");
    }finally{
        return
    }
    assert(false, "finally did not execute");
}
nested();

let acc = "";

try{
    assert(acc === "",1)
    acc += "a"
}catch(e){
    acc += "b"
}finally{
    assert(acc === "a",2)
    acc += "c"
}
assert(acc === "ac", "wrong flow, is: `" + acc + "` should be `ac`")
true
