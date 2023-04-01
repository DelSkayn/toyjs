(function(){
    let b = 1;
    let x = 1;

    let outer = function (){
        let inner = function (){
            assert(x === 1,"1");
            x += 1;
        }
        inner();
        assert(x === 2,"2");
    }
    let another = function(){
        x += 1;
    }
    outer();
    assert(x === 2,"3");
    assert(b === 1,"4");
    another();
    assert(x === 3,"5");

    let iterator = function (){
        let x = 0;
        return function (){
            return x++;
        }
    }

    let iter = iterator();
    assert(iter() === 0, "6");
    assert(iter() === 1, "7");
    assert(iter() === 2, "8");
    assert(iter() === 3, "9");



    let deep = function (){
        let a = "Too deep";
        return function (){
            let b = "Less deep";
            return function (){
                return function (){
                    return {a: a, b: b};
                }
            }
        }
    }
    let obj = deep()()()();
    assert(obj,"10");
    assert(obj.a === "Too deep","11");
    assert(obj.b === "Less deep","12");

    let wrap = function(){
        let x = 1;
        let inner1 = function(){
            x += 1;
            return x;
        }
        let inner2 = function(){
            x += 1;
            return x;
        }
        return{
            inner1: inner1,
            inner2: inner2,
        }
    }
    let unwrap = wrap();
    assert(unwrap.inner1() === 2,"13");
    assert(unwrap.inner2() === 3,"14");
    assert(unwrap.inner1() === 4,"15");
})()
