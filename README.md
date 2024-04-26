# ICL-Project

In this repository you will find an haskell's implementation of the portion of the micro-ml programming language [proposed](assigment.pdf) for the first part of the pratical assigment of **ICL**. You will find a **typechecker**, **parser** and an **interpreter** for the language. There is also a **compiler** (only work for logical and arithmetic operations) but to use it you would have to change the code.

<!-- This is surely not complete but it works at some extent. Here you have an interpreter for a mini-caculator as well as a compiler for this mini calculator language to JVM. -->
<!--  -->
# Build & Run
To build you need to have `ghc` (haskell compiler) and `gnu make` installed. After making sure you have both installed you can just run `make`, at the root of the repo. This will out of the box drop an executable named `bin/main`.

The program works as required by the professor. When provided with an argument it tries to find a file with such name and it parses, typecheck and interpret it. There are two other way to give input to our program, to know more about them just run `bin/main --help`.

<!-- For the first time you might want to run `./bin/main --help` go get a feel of how it works. That's it c; -->

# Syntax & Ast

Here below we list the `context-free grammar` of our parser:

```
<program>    ::=  <decl> EOF
<decl>       ::= "let" ( Id (":"<type>)? "=" <expr> )+ "in" <decl> "end" | <sequence> 
<sequence>   ::= <assigment> (";" <sequence>)*
<assigment>  ::= <expr> (":=" <assigment>)*
<expr>       ::= <logicalOr>  ( "&&" <logicalOr> )*
<logicalOr>  ::= <comparison> ( "||" <comparison>)*
<comparison> ::= <term>  (( ">" | "<" | "==" | "!=" | ">=" | "<=" ) <term> )*
<term>       ::= <factor> (( "+" | "-" ) <term>  )*
<factor>     ::= <primary> (( "*" | "/" ) <factor> )*
<unary>      ::= ("-"|"~"|"!"|"new") <unary> | <primary>
<primary>    ::= "true" | "false" | Num | "(" ")" | "(" <expr> ")" | ID 
                  | <ifExpr> | <printExpr> | <whileExpr>

<printExpr>  ::= ("print" | "println") <expr>
<ifExpr>     ::= "if" <expr> "then" <decl> ("else" <decl>)? "end"
<whileExpr>  ::= "while" <expr> "do" <decl> "end"

<type>       ::=  "int" | "bool" | "unit" | "ref" <type>
```

About the AST, we tried to be clever and spare as much as possible, not to endup with tons of nodes so the full extend of it might not be easy to understand. But in the beggining of of [src/Parser.hs](src/Parser.hs) you will find a list of all the official nodes. I don't know if you will even peek at the code, but I left this message just in case.

# Extra features

About the extra features to implement you are thinking on:
- The extensive set of tests
- Typechecker (parser and scanner as well) reporting as much error as it can
- Type sums and case expressions
- Type inference

These are some that we are thinking on. We will probably take two or three of those and implement.

---
# Disclaimer
PLEASE IGNORE WHAT IS BELOW IT IS HERE FOR THE SOLE PURPOSE OF GUIDING US THROUGH THE DEVELOPMENT CYCLE.

# TODOs:
- [x] Figure a way to handle and report errors
- [ ] Write a set of tests
- [x] Add support for types on variable declarations
- [ ] Rename some tokens to improve code readability
- [ ] Look for better ways to design your CLI interface c:
- [x] Think about storing information about places on the tokens (probaby coming up with a token type c:)
- [ ] Think about the possibility of adding types `BinaryOp` and `UnaryOp` (to simplifly the code c:).
- [ ] Add multi-error report

# Features to implement
- [x] If stamemt
- [x] Assigment
- [x] While loop
- [x] Functions print and println
- [x] Semi-colon expressions c:
- [x] References things (`new`, `!` and `ref` type)
- [ ] Add strings c:
