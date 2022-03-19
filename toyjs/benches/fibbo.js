let fibbo = function(a){
    if(!a){
        return 1
    }
    if(!(a-1)){
        return 1
    }
    return fibbo(a -1) + fibbo(a - 2)
}
fibbo(20)
