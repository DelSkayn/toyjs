
for(let j = 10;j;j--){
    for(let i = 100000;i;i--){
        let a = {
            b: {
                c: {
                    d: {
                        e:{
                            f:{
                            }
                        }
                    }
                }
            }
        };
        a.b.c.d.e.f.g = a;
    }
}
