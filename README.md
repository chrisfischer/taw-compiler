# Taw Compiler
Chris Fischer (cdf) and Logan May (loma)

## Components
Components should be read in the following order.

### Ast
The Taw AST is wholly contained in the `Ast` module.

### Parser
The parser is wholly contained in the `Parser` module.

### Pretty Printer
The pretty printer is wholly contained in the `PrettyAst` module.

### Interpreter
The interpreter is wholly contained in the `Interpreter` module.

### Compiler
The compiler consists of two modules, `Frontend` and `LLVMGen`. Start reading with `LLVMGen`, it contains helper functions that `Frontend` uses.

### REPL
The REPL shell is implemented in `JITMain.hs`.

## Files

### Ast.hs
Contains the type definitions for the Taw abstract syntax tree.

### AstGen.hs
Contains the `Gen` definition for the Taw systax tree. Allows for the generation of syntactically correct programs.

### Compiler.hs
Main module for the compiler executable. Contains code needed to read and parse a file, compile it, and pretty print the resulting LLVM out to a new file.

### Frontend.hs
Contains all functions that compile Taw to LLVM. The code in this file interacts with the functions in `LLVMGen.hs` to build up the compiled code. Mainly used in the REPL and the compiler executable.

### InterpMain.hs
Main module for the interpreter executable. Contains code needed to read and parse a file, interpret the resulting AST, and print the value.

### Interpreter.hs
Contains all functions used to interpret taw programs. Also contains all the type definitions for the contexts used in its state monad. Used in the interpreter executable and the QuickCheck properties.

### JIT.hs
Contains code that interfaces with LLVM and its JIT compiler. Provides a way to run LLVM code within Haskell and marshall the resulting value into Haskell types. Used by the REPL shell and by QuickCheck properties.

### JITMain.hs
Main module for the Taw REPL executable. Contains all code that implements the shell functionality. Also contains the type definitions for the context used in its state monad.

### LLVMGen.hs
Contains helper functions that make it easier to build up LLVM code. This is mainly used by `Frontend.hs` to build up the compiled statements.  Also contains the type definitions for the contexts used in its state monads.

### Parser.hs
Contains functions to parse a Taw program from a file and a string.

### PrettyAst.hs
Contains functions to pretty print a Taw program.

### PrettyAstMain.hs
Main module for the formatter. Contains code needed to read and parse a file, and pretty print the parsed AST.

## Dependencies
* [haskeline](https://hackage.haskell.org/package/haskeline) >=  0.7.5.0
* [llvm-hs ](https://hackage.haskell.org/package/llvm-hs) >=  9.0.1
* [llvm-hs-pure](https://hackage.haskell.org/package/llvm-hs-pure) >=  9.0.0
* [parsec](https://hackage.haskell.org/package/parsec) >=  3.1
* [QuickCheck-GenT](https://hackage.haskell.org/package/QuickCheck-GenT) >= 0.2.2
* [string-random](https://hackage.haskell.org/package/string-random) >= 0.1.2.0
* [quickcheck-string-random](https://hackage.haskell.org/package/quickcheck-string-random) >= 0.1.3.0