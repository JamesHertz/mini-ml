# ICL-Project

This is surely not complete but it works at some extent. Here you have an interpreter for a mini-caculator as well as a compiler for this mini calculator language to JVM.

# Build & Run
To build you will need to have `ghc` (haskell compiler) and `gnu make` installed. After making sure you have both installed you can just run `make`, at the root of the repo, which will out of nothing drop off and executable named `bin/main` for you. This is the program so just run it. 

For the first time you might want to run `./bin/main --help` go get a feel of how it works. That's it c;

# TODOs:
- [x] Figure a way to handle and report errors
- [ ] Write a set of tests
- [x] Add support for types on variable declarations
- [ ] Rename some tokens to improve code readability
- [ ] Look for better ways to design your CLI interface c:
- [x] Think about storing information about places on the tokens (probaby coming up with a token type c:)

# Features to implement
- [x] If stamemt
- [ ] Assigment
- [ ] While loop
- [x] Functions print and println
- [x] Semi-colon expressions c:
- [ ] References things (`new`, `!` and `ref` type)
