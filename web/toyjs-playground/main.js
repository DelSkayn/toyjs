import * as monaco from "monaco-editor";
import editorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
import jsWorker from 'monaco-editor/esm/vs/language/typescript/ts.worker?worker';
import init, { lex, parse, resolve, compile } from './toyjs-playground-bundle/pkg/toyjs_playground_bundle';

await init("./toyjs-playground-bundle/pkg/toyjs_playground_bundle_bg.wasm");


const defaultValue = "function fibbo(a){\r\n    if(a === 0 || a === 1){\r\n        return 1\r\n    }\r\n    return fibbo(a-1)+fibbo(a-2);\r\n}\r\n\r\nfibbo(20);";
const fgCodeMap = Object.freeze({
  30: "#c0caf5",
  31: "#f7768e",
  32: "#9ece6a",
  33: "#e0af68",
  34: "#7aa2f7",
  35: "#bb9af7",
  36: "#7dcfff",
  37: "#24283b",
});

self.MonacoEnvironment = {
  getWorker(_, label) {
    switch (label) {
      case "javascript":
      case "typescript":
        return new jsWorker()
      default:
        return new editorWorker()
    }
  }
}

monaco.editor.createWebWorker

const editorDiv = document.getElementById("editor");
const result = document.getElementById("result");
const runButton = document.getElementById("run-button");
const runOption = document.getElementById("run-option");

function splitFirst(x, at) {
  const i = x.indexOf(at);
  return [x.slice(0, i), x.slice(i + 1)]
}

function processText(x) {
  let fg = null;
  //let bg = null;
  /*
  const bgCodeMap = {
    40: "black",
    41: "red",
    42: "green",
    43: "yellow",
    44: "blue",
    45: "magenta",
    46: "cyan",
    47: "white",
  };
  */

  const escapes = x.split("\u001b").map(x => {
    if (x.startsWith("[")) {
      let [code, rem] = splitFirst(x.slice(1), "m");
      const codes = code.split(";");
      let newFg = codes.length > 1 ? fgCodeMap[parseInt(codes[1])] ?? null : null;
      //let newBg = codes.length > 1 ? bgCodeMap[parseInt(codes[1])] ?? null : null;

      if (newFg !== null) {
        if (fg !== null) {
          rem = `</i><i style="color: ${newFg};">` + rem;
        } else {
          rem = `<i style="color: ${newFg};">` + rem;
        }
        fg = newFg;
      } else {
        if (fg !== null) {
          rem = `</i>` + rem;
        }
        fg = newFg;
      }
      return rem
    }
    return x
  });

  return escapes.join("");
}

const value = localStorage["v"] ?? defaultValue;


/*
monaco.editor.addCommand(monaco.KeyMod.Alt | monaco.KeyCode.KeyL, function() {
  run("lex")
});
*/


const editor = monaco.editor.create(editorDiv, {
  value,
  language: "javascript",
  automaticLayout: true,
  theme: "vs-dark",
})


editor.addAction({
  id: "runLex",
  label: "Run the lexer",
  keybindings: [monaco.KeyMod.Alt | monaco.KeyCode.KeyL],
  run: function() {
    run("lex")
  }
});

editor.addAction({
  id: "runParse",
  label: "Run the parser",
  keybindings: [monaco.KeyMod.Alt | monaco.KeyCode.KeyP],
  run: function() {
    run("parse")
  }
});

editor.addAction({
  id: "runResolve",
  label: "Run the variable resolver",
  keybindings: [monaco.KeyMod.Alt | monaco.KeyCode.KeyR],
  run: function() {
    run("resolve")
  }
});

editor.addAction({
  id: "runCompile",
  label: "Run the compiler",
  keybindings: [monaco.KeyMod.Alt | monaco.KeyCode.KeyC],
  run: function() {
    run("compile")
  }
});

editor.addAction({
  id: "runEvaluate",
  label: "Evaluate the script",
  keybindings: [monaco.KeyMod.Alt | monaco.KeyCode.KeyE],
  run: function() {
    run("evaluate")
  }
});

let saveTimeout = null;
editor.getModel().onDidChangeContent(() => {
  if (saveTimeout) {
    clearTimeout(saveTimeout);
  }
  saveTimeout = setTimeout(() => {
    localStorage["v"] = editor.getValue();
    saveTimeout = null;
  }, 100);
})

function run(which) {
  switch (which) {
    case "lex": {
      const res = lex(editor.getValue());
      result.innerHTML = res;
      break;
    }
    case "parse":
      try {
        const res = parse(editor.getValue());
        result.innerHTML = processText(res);
        result.style = ""
      } catch (e) {
        result.innerHTML = e;
        result.style = "background-color: #621717;"
      }
      break;
    case "resolve":
      try {
        const res = resolve(editor.getValue());
        result.innerHTML = processText(res);
        result.style = ""
      } catch (e) {
        result.innerHTML = e;
        result.style = "background-color: #621717;"
      }
      break;
    case "compile":
      try {
        const res = compile(editor.getValue());
        result.innerHTML = processText(res);
        result.style = ""
      } catch (e) {
        result.innerHTML = e;
        result.style = "background-color: #621717;"
      }
      break;
    case "evaluate":
      result.innerHTML = "Evaluate is not yet implemented in the playground";
      result.style = ""
      break;
  }
}

runButton.onclick = function() {
  run(runOption.value);
}


