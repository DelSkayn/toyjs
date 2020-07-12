# toyjs
A toy javascript interperter

TODO implementation parts
=============================

Parsing
-------
Formatting the code into a AST

Missing:
- [x] For loops
- [ ] Better Delim handling
- [x] Semi Keyword handling
- [x] Operator precedence
- [ ] Spans in AST
  - [ ] Find a good way for spans to exist in the AST
- [ ] String interner
- [ ] Bigint
- [ ] Unicode escape codes
- [ ] Regex's
- [ ] Templates
- [ ] HTML comment's
- [ ] Arrow functions
- [ ] Futures
- [ ] Generators
- [ ] Labels
- [ ] Private members?

Compiling
---------
Compiling the AST into bytecode

Missing:
- [x] Basic aritmetic expressions
- [ ] Error infrastructure.
- [ ] Variable handling
  - [x] Basic globals
  - [ ] Let binding
  - [ ] Const binding
- [ ] Spans with bytecode for error handling
- [ ] Logical operations
- [ ] If Else
- [ ] Basic while loop
- [ ] Basic for loop
- [ ] Basic map like Objects
- [ ] Functions
  - [ ] Inlining small functions?
  - [ ] Tail call?
- [ ] Strings
- [ ] Bindings
- [ ] Classes
- [ ] Optimization pases:
    - [ ] Dead code elimination
    - [ ] Pre calculation
    - [ ] Const propegation
- [ ] Everything Else

VM
--
Running the bytecode as well as builtin functions.
- [x] Value implementation with NaN-tagging
- [ ] Instructions
  - [x] Basic format
  - [x] Decoding and encoding
  - [x] Disassembeling
- [x] Basic aritmetic expressions
- [ ] Error infrastructure.
- [ ] Const data
  - [x] Basic implementation
  - [ ] Allow serialization
  - [ ] Fast instantion of objects.
  - [ ] Copy on write like functionality?
- [ ] Objects
  - [x] Basic object functionality
  - [ ] Handle object constants
  - [ ] Better object index, something like the atoms from quickjs
- [ ] Variables
  - [x] Basic globals as object properties
  - [ ] Scoped variables
- [ ] Logical operations
- [ ] Jumps
- [ ] Functions
  - [ ] Implement basic call convention
  - [ ] Tail call?
- [ ] Everything Else
