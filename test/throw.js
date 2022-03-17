try{
    (function(){
        throw "b";
    })()
}catch(e){
    console.log("a");
    console.log(e)
}

function nested(){
    try{
        (function(){
            (function(){
                let a = {};
                (function(){
                    throw "d";
                })()
            })()
        })()
    }catch(e){
        console.log("c");
        console.log(e);
    }finally{
        console.log("e");
    }
}
nested();

try{
    console.log("f");
}catch(e){
    console.log("invalid");
}finally{
    console.log("g");
}
console.log("h");


