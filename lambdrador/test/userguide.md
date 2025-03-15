# Lambdrador User's Guide

This document aims to teach the reader how to program in the Lambdrador programming language, it assumes you have some level of experience with programming, however will explain concepts when they are encountered.

## Intro

Lambdrador is a functional programming language influenced by the ML family of languages and features a flexible, easy to read syntax, dynamic type system and type inference. 

What does this mean?
* functional programming: a type of programming focused on functions, immutability and pattern matching (this will be explained in more detail)
* dynamic type system: types are checked at run-time, meaning any value can be any type and any value passed into any input parameter
* type inference: the types of variables are deducted from their definition, rather than an being given by the user explicitly (such as in a language like C)

## Basics

### Using the interpreter

you can type `lambdrador` into the console to open up the REPL, which will allow you to type in command individualy, by inputting `lambdrador X` into the console the interpreter will interpret file X

### Basic Expressions

Arithmatic expression can be completed through the +, -, * and / operators, representing addition, subtraction, multiplication and division respectively. 

### Variables and Functions

variables can be assigned using the `let` keyword, an example is shown below

`let x = 5`

this assigns the value of 5 to the name 'x'. This variable can then be used in expressions

`x + 5`

The result of the expression above is 10

Functions can be defined using the same `let` keyword as used in variable assignment, but with parameter names given after the name, an example is shown below

`let add_one n = n + 1`

in this example a function called 'add_one' has been defined with 1 input parameter, named 'n'. This function's output is n+1 for any given input n. 

Both variable and function definition allow for statements
