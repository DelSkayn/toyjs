let obj = {
    a: 1,
    b: 1,
    c: 1,
    dddddd: 1,
    eeeeee: 1,
    ffffff: 1,
    gggggg: 1,
    hhhhhh: 1,
    iiiiii: 1,
}

let sum = 0;
for(let i = 0;i < 10000;i++){
    sum + obj.a
        + obj.b
        + obj.c
        + obj.dddddd
        + obj.eeeeee
        + obj.ffffff
        + obj.gggggg
        + obj.hhhhhh
        + obj.iiiiii;
}
sum
