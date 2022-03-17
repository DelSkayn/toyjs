while (true){
    console.log("Who are you?");
    let val = console.input();
    if(val){
        if(val === "nobody\n"){
            console.log("Okay, goodbye then.");
            break
        }else{
            console.log("Hello " + val);
        }
    }else{
        break
    }
}
