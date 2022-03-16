let running = true;
while (running){
    let input = console.input();
    if(input){
        let v = eval(input);
        console.log(v);
    }else{
        running = false
    }
}
