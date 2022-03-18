A simple imperative language implementation.

The language is designed to be simple while being exposed to several interesting topics such as:

- Multiple level expression derivation rules as well as possibility to define custom operators
- A simple type system and type inference and potentially some genericity
- Some advanced topics on functions such as closures and HOF
- Ranges, tuples, arrays, ...

Current status:

- [x] Lexer with enhanced features.
- [x] Parser to generate AST
    - [x] Simple expressions and expression trees of 11 levels.
    - [x] Custom operators in AST generation.
    - [x] Type notations and patterns.
    - [x] Statements, declarations and functions.
- [x] Handle generic clauses in function declarations as well as function calls.
- [x] Handle type annotations such as `tuple<int, tuple<int, int>>` to distinguish with `>>` symbol, however, this feature is not used in parser.
- [ ] Type system
    - [ ] Simple type checking,
    - [ ] Type inference, 
    - [ ] Template genericity.
- [ ] Code generation (planned for LLVM)

Known bugs:

- The error message throw out arbitrary messages that don't help to understand the error.
