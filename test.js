function fibbo(a){
    if (a === 0){
        return 1
    }
    if (a === 1){
        return 1
    }
    return fibbo(a -1) + fibbo(a - 2);
}
console.log(fibbo(40));
