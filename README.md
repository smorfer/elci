# elci
## A lambda calculus interpreter

# Building
ghc elci.hs

# Usage
elci filepath

# Syntax
"|" denotes a lambda binder

All letters lowercase can be used as variables, e.g. "|x"

All WORDS uppercase can be used as constants, e.g. "TRUE"

"." separates the variable declaration from the function body, e.g. "|x.x"

Parentheses used for inner terms obviously, e.g "|wyx.y(wyx)"

# Statements & Assignments
Assignments are written

    CONSTANT = lambda

and can itself have constants in the the lambda term


Statements are just lambda terms and get printed on the console

# Bugs
The dot (".") just gets skipped after the first one per term (it's not a bug, it's a feature!)

The parsing at whole is a little bit rough and will have bugs, that I couldn't find until now

# WIP
Comments

Better Parsing

Syntax errors are not handled very clean (the throwing is all over the place) and will be more structured in the future

The REPL is missing

Libraries consisting of commonly used functions and church-encoded data types

A module loader

Detection of already defined terms and displaying them as their constant names (should be easy, as DeBruijn terms can be instantiated as Eq and are easy to compare, will be implemented soon)

# Testing
You can find example programs written in actual lambda calculus in the example directory

bool.lc -> This tests a few boolean functions

arithmetic.lc -> This defines the numbers up to ten and tests arithmetical operations as addition (multiplication & subtraction coming soon)

(coming soon) flow.lc -> First touch with recursion and conditionals, still have to read in to those
