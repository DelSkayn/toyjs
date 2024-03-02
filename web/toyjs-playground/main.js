import * as monaco from "monaco-editor";
import editorWorker from 'monaco-editor/esm/vs/editor/editor.worker?worker';
import jsWorker from 'monaco-editor/esm/vs/language/typescript/ts.worker?worker';
import init, { lex, parse } from './toyjs-playground-bundle/pkg/toyjs_playground_bundle';

await init("./toyjs-playground-bundle/pkg/toyjs_playground_bundle_bg.wasm");

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


const editor = monaco.editor.create(editorDiv, {
  value: "hello",
  language: "javascript",
  automaticLayout: true,
  theme: "vs-dark",
})

runButton.onclick = function() {
  switch (runOption.value) {
    case "lex": {
      const res = lex(editor.getValue());
      result.innerHTML = res;
      break;
    }
    case "parse":
      try {
        const res = parse(editor.getValue());
        result.innerHTML = res;
        result.style = ""
      } catch (e) {
        result.innerHTML = e;
        result.style = "background-color: #621717;"
      }
      break;
    case "compile":
      result.innerHTML = "Compile is not yet implemented in the playground";
      result.style = ""
      break;
    case "evaluate":
      result.innerHTML = "Evaluate is not yet implemented in the playground";
      result.style = ""
      break;
  }
}


