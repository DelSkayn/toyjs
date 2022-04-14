(function(){
    let b = 1;
    let x = 1;

    let outer = function (){
        let inner = function (){
            assert(x === 1);
            x += 1;
        }
        inner();
        assert(x === 2);
    }
    let another = function(){
        x += 1;
    }
    outer();
    assert(x === 2);
    assert(b === 1);
    another();
    assert(x === 3);

    let iterator = function (){
        let x = 0;
        return function (){
            return x++;
        }
    }

    let iter = iterator();
    assert(iter() === 0);
    assert(iter() === 1);
    assert(iter() === 2);
    assert(iter() === 3);



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
    assert(obj);
    assert(obj.a === "Too deep");
    assert(obj.b === "Less deep");

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
    assert(unwrap.inner1() === 2);
    assert(unwrap.inner2() === 3);
    assert(unwrap.inner1() === 4);
})()
