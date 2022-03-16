# Toyjs javascript interpreter.

This repository contains the code of a toy javascript interpreter. 
Built for experimentation with building dynamic languages.

This interpreter is very much under development and lacks support for a large part of the
javascript language.

## Features
 - Register based bytecode VM
 - Handwritten parser.
 - Traditional Lexer to Parser to ast to compiler to bytecode design.
 - Minimal dependencies.

## Running

In order to run the vm compile the `cli` binary with the following command:
```
cargo build --release --bin cli
```
This will produce a binary in the target directory which can execute javascript scripts as following:
```
cargo run --release --bin cli ./script.js 
```

## Crates
The toyjs interpreter consists of various crates.
The primary crate is `toyjs` crate in the repository root.
This crate implements a 'safe', high level interface over the various functions of the interpreter, as well as the javascript runtime.

The implementation of the interpreter itself can be found in the various crates under the `crates` directory.
