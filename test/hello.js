
let running = true;
while (running){
    console.log("Who are you?");
    let val = console.input();
    if(val){
        console.log("Hello " + val);
    }else{
        running = false;
    }
}
