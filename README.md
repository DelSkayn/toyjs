# Toyjs javascript interpreter.

This repository contains the code of a toy javascript interpreter. 
Built for experimentation with building dynamic languages.

This interpreter is very much under development and lacks support for a large part of the
javascript language.

You can play around with an previous version of the interpreter in your browser [here](https://delskayn.github.io/toyjs/)

## Features
 - Register based bytecode VM
 - Handwritten parser.
 - Traditional Lexer to Parser to ast to compiler to bytecode design.
 - Almost safe sweep and trace GC 
 - Minimal dependencies.

## Current State

The engine is currently undergoing a rewrite and is in a non-functional state.

But you can play around with the currently functional parts of the interpreter.


## Running the interpreter

The VM is currently very bare-bones, with only a very small number of instructions implemented.
A simple example for the VM can ran with 

```sh
cargo run --example vm
```

All examples take either a file name or assume a script will be piped into stdin.

The interpreter has varies tools for printing data-structures and generated source information.

For printing a AST you can use
```sh
cargo run --example parse
```
The lexer can similarly be run on code with
```sh
cargo run --example lex
```

A overview of symbol information produced by the compiler can be printed with
```sh
cargo run --example resolve
```

A script can be compiled and the compiled instructions can be dumped with
```sh
cargo run --example compile
```

