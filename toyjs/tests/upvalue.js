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
    outer();
    //assert(x === 2);
    //assert(b === 1);

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
})()
