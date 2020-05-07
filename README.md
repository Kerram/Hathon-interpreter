Simple (but first in the world) interpreter of Hathon language.

Hathon is a pure functional language with syntax being a mix of Haskell (big part)
and Python (smaller parts), hence the name.

Hathon and its interpreter support many features with a detailed list below:

- strong type system
- comments, both multi and single line
- 2 basic types: Int and Bool
- standard arithmetic, boolean and comparision operations
- if construct
- let construct
- functions as first class citizens: multiple arguments, partial application, higher order functions, lambdas (anonymous functions), closures
- recurrence both for functions and non functional values (yes, you can declare true infinity in code, but you won't be able to do anything with it unfortunately)
- error handling with stack trace for runtime errors and positions in the source code for all types of errors
- predefined function to deal with lists: head, empty and tail
- syntactic sugar to declare constant lists, e.g.: [], [42, 5, 2]
- homogeneous list of arbitrary types: lists of lists, functions or combination of both
- static binding of identifiers
- static type checking before code execution

Source code is firstly run through the lexer (BNFC is used here) and after
obtaining AST (abstract syntax tree) is checked statically using type checking mechanism and
only then run.

One of the biggest downsides is the requirement for the user to explicitly write
types of all values and functions.

Language is eager, but some parts could be evaluated lazily, interpreter and the language
do not state which parts, if any.

In the src directory you will find source codes of the interpreter and type checking mechanism.

In the examples directory you will find example programs written in Hathon.
Good subdirectory contains programs that exit correctly (safe for one example with infinite recurrence).
Bad subdirectory contains programs that exit with an error.

To compile the interpreter you should have ghc and bnfc installed on your computer.
In order to provide positions of errors in the source code, non mainstream version of
BNFC was used (in the mainstream there is a bug): https://github.com/BNFC/bnfc/tree/176-source-position.
This version also contains a bug, but it is easily fixable.

To compile interpreter simply type make (remember to write in the Makefile localization of your bnfc).
To run interpreter type ./interpreter {filePath}, where {filePath} is a path to file containing source code in Hathon.

Program is divided into 'instructions' separated by semicolons.
Each 'instruction' is either a definition or an expression.
Definition will add new name to the global environment, but expression will cause
the interpreter to print its value to stdout.

Any error will interrupt the interpretation of the program and will end the interpreter execution,
with error message written to stderr.
