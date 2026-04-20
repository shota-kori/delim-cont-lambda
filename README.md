# delim-cont-lambda

([日本語版](./README_ja.md))

**delim-cont-lambda** is a lambda calculus interpreter featuring let-polymorphism and delimited continuation operators.
It was made by Haskell and an interactive REPL is available.

## Description

This project is an implementation of a functional programming language based on the lambda calculus. It features a Hindley-Milner style type system, implemented according to the formal type rules described in the referenced literature, enabling the description of complex control flows using delimited continuations.

## Example

### Basic Types and Operations
The language supports `Int` and `Bool` types, basic arithmetic operators (`+`, `-`, `*`), and `if` expressions.

```
> 1
 : Int => 1
> + 2 3
 : Int => 5
> True
 : Bool => True
> if False then 1 else 0
 : Int => 0
```

### Function Types
In this system, function types are represented with four type components to account for delimited continuations.
`\` denotes λ and `\x.` starts lambda abstraction. (`x` is any variable name)

```
> \x. x
 : (t1 / t2 -> t1 / t2) => (\x. x)
> \x. + x 2
 : (Int / t10 -> Int / t10) => (\x. + x 2)
```

### Let-polymorphism
`let` expressions support polymorphic types, allowing the same function to be used with different types.

```
> let id = \x. x in if (id True) then (id 1) else (id 0)
 : Int => 1
```

### Delimited Continuations
The `shift` operator captures the continuation, and `reset` delimits it.

```
> reset (+ 1 (shift k. k (k 10)))
 : Int => 12
> reset (- 100 (shift k. False))
 : Bool => False
```

## Web REPL
Experience the language directly in your browser: [Web REPL](https://shota-kori.github.io/delim-cont-lambda/html/)

## References
- [Introduction to Programming with Shift and Reset](http://pllab.is.ocha.ac.jp/~asai/cw2011tutorial/main-e.pdf)

