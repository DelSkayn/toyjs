let b = 1;
let x = 1;

function outer(){
    function inner(){
        console.log(x);
        x += 1;
    }
    inner();
    console.log(x);
}
outer();

function iterator(){
    let x = 0;
    return function (){
        return x++;
    }
}

let iter = iterator();
console.log(iter());
console.log(iter());
console.log(iter());
console.log(iter());



function deep(){
    let a = "Too deep";
    return function (){
        let b = "Less deep";
        return function (){
            return function (){
                console.log(b);
                return a;
            }
        }
    }
}

console.log(deep()()())
console.log(deep()()()())
