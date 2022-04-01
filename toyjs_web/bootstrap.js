import './style.scss';
import 'xterm/css/xterm.css';
import { Terminal } from "xterm";
import { FitAddon } from "xterm-addon-fit";

import("./pkg").then(({ToyJs}) => {
    const toyjs = ToyJs.new();
    const term = new Terminal();
    const fit = new FitAddon();
    term.loadAddon(fit);
    let input = "";
    const handleInput = x => {
        const out = toyjs.eval(x);
        term.writeln(out.replaceAll("\n","\r\n"))
    }
    term.onKey((key,_) => {
            const code = key.key.charCodeAt(0);
            if (code == 13) { // CR
                term.writeln("");
                let pending_delim = 0;
                for (const key of input){
                    if (key === "{" || key === "(" || key === "[") {
                        pending_delim += 1;
                    }
                    if (key === "]" || key === ")" || key === "}") {
                        pending_delim -= 1;
                    }
                }
                if (pending_delim == 0){
                    handleInput(input);
                    term.write("> ");
                    input = "";
                }else{
                    term.write("... ");
                }
            } else if (code < 32 || code == 127) { // Control
                if (code == 127){
                    term.write("\x08 \x08");
                    input = input.slice(0,input.length - 1)
                }
                return;
            } else { // Visible
                term.write(key.key);
                input += key.key;
            }
        });
    term.open(document.getElementById("term"));
    fit.fit();
    term.write("> ");
});
