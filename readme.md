**(Archived) This project is suspended due to my lack of advanced FP techniques, a newer repository based on an imperative base language such as C# will be available sooner or later.**

In fact, in order to implement the typing system (type inference included) in my language as per techniques and algorithm decribed in TAPL, it's required to use local variables and make mutations on them. This implies that, in Haskell, some advanced FP skills (State Monad, Algebraic Effects, ...) are required.

Currently I prefer a priority on compiling and typing system than monad and cats hence a choice to implement my language on an imperative base language has been made. Still, this repo show the interest of achieving a parser in a FP way.

---

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
- [ ] Fixed error messages and refactor code.
- [ ] Type system
    - [ ] Simple type checking,
    - [ ] Type inference, 
    - [ ] Template genericity.
- [ ] Code generation (planned for LLVM)

Known bugs and remarks:

- The error message throw out arbitrary messages that don't help to understand the error. I have tried to use non backtracking parser to resolve this but failed. As some combinators such as `leftAssociate` need this feature, the idea of non back tracking parser has been abandonned. Still, even if with the backtracking parser, one has to use `try` combinator in many places and it will make the non backtracking parser useless as one is dependent on the backtracking feature.
- Expressions such as `1 + 2 + 3 +` could be handled by chainl1.
- In `exprLevel4` the first op cannot be parsed, walkaround with `nop` to occupy this first position.
- Usage of `try` in `statement` since cannot distinguish case with `a += 1` and `a + b` (or `a.foo()` as function call is also an expression). This should be eliminated but I don't have a good idea.
- ~~Cannot really handle type annotations well, currently the colon symbol `:` is only used inside type annotations and there is a walkaround for switch statement and label argument with `=>` instead of `:`.~~ (fixed)
- ~~The current implementation of handle `>>` forbid the symbol `>` from being used as a head of custom operator. The previous implementation of looking for `:` requires that `:` should only be used in one position i.e. the type annotation, which is impossible as it's also used in parameter labels and it's possible to be used in other future places.~~ (fixed)

Credits:

- Programming in Haskell for overall structure of parser
- [???Haskell??????Parser Combinator?????????](https://gaufoo.com/parser2/) for the idea of non backtracking parser
