# Lambdrador User's Guide

This document aims to teach the reader how to program in the Lambdrador programming language, it assumes you have some level of experience with programming, however will explain concepts when they are encountered.

## Intro

Lambdrador is a functional programming language influenced by the ML family of languages and features a flexible, easy to read syntax, dynamic type system and type inference. 

What does this mean?
* functional programming: a type of programming focused on functions, immutability and pattern matching (this will be explained in more detail)
* dynamic type system: types are checked at run-time, meaning any value can be any type and any value passed into any input parameter
* type inference: the types of variables are deducted from their definition, rather than an being given by the user explicitly (like that in C and other similar languages)

## Basics

### Using the interpreter

you can type `lambdrador` into the console to open up the REPL, which will allow you to type in command individualy, by inputting `lambdrador X` into the console the interpreter will interpret file X

lambdrador files do not need to have any file extension to be interpreted (i've been using .lrd)

### Basic Expressions

Arithmetic expression can be completed through the +, -, * and / operators, representing addition, subtraction, multiplication and division respectively. 

If in the REPL mode, you can type an expression like '2 + 2', and you can see the evaluated result below.

### Statements

The following are all statements in lambdrador
- 'print' (prints an expression without a newline)
- 'println' (prints an expression with a newline)
- 'let' (assigns a variable or function to a name)
- 'import' (imports functions and variables from a file)

### Variables and Functions

variables can be assigned using the `let` keyword, an example is shown below

`let x = 5`

this assigns the value of 5 to the name 'x'. This variable can then be used in expressions

`x + 5`

The result of the expression above is 10

Functions can be defined using the same `let` keyword as used in variable assignment, but with parameter names given after the name, an example is shown below

`let add_one n = n + 1`

in this example a function called 'add_one' has been defined with 1 input parameter, named 'n'. This function's output is n+1 for any given input n. 

Both variable and function definition allow for a list of statements before a final expression, this means you can define variables and functions, print expression and import variables and functions from different files, as shown below:

```
let foo a b =
    import "bar.lrd";
    let baz = n * func_imported_from_bar a b;
    print "baz is: "; println baz;
    baz
```

in the example above a function named foo is defined with 2 input parameters, a and b. When the function is invoked it imports functions from "bar.ldr" into its scope, defines a variable named baz equal to a function defined from bar, prints "baz is: ", prints the value of baz with a newline and then returns the value of bar from the function.

when a variable is defined its value is evaluated immediately, this includes interpreting all the statements within.

### Lists

Expressions can be collected into lists, using the syntax below

```
[1; 2; 3]
```

These lists are singly linked, meaning that the list of '[1; 2; 3]' is a list of 1 -> 2 -> 3 -> [].

You can add a new element to the start of a list using the cons operator, ::

```
1 :: [2; 3]
```

This operation evaluates to [1; 2; 3]

### Advanced Functions

#### Functions as input

Functions are 'first-class' citizens within lambdrador, meaning they can be treated like any other value

Functions like 'filter' and 'map' make use of functions as inputs, as shown below

```
import "stdlib.ldr"

let is_greater_than_three n = n > 3

filter is_greater_than_three [1; 2; 3; 4; 5]
```

The example above shows a list of [1; 2; 3; 4; 5], filtered to only have values greater than 3 (returns [4; 5]).

NOTE: to use functions like 'filter' you must import a copy of stdlib.ldr

#### Functions as outputs (Closures)

Functions can also be returned as outputs from other functions, and can 'remember' values of variables from the scope they're returned from, as shown in the example below.

```
let factor f =
    let mult n = 
        f * n;
    mult

let double = mult 2
let triple = mult 3

println double 10
println triple 10
```

The result of double 10 is 20, as double 'remembers' the value of f is 2, similarly, triple 10 returns 30.

#### Anonymous functions

instead of defining functions, such as is_greater_than_three, anonymous functions can be used to definie a functions without assignig it to a varaible

The example using filter can be rewritten as such:

```
filter (ld n = n > 3) [1; 2; 3; 4; 5]
```


### Conditional Expressions

#### If expressions

'if' expressions are a type of conditional expression which will return one value or another depending on the value of a condition

```
if x = 1 then "x is one" else "x is not one"
```

the expression will return the string "x is one" if x is one, and "x is not one" if not one. These if statements can be used to influence the output of functions, and as such can allow for recursive functions

```
let factorial n = if n <= 1 then 1 else n * factorial (n-1)
```

This function recursively calls itself to calculate the factorial of a number 'n'

#### Pattern Matching

More advanced conditionals can be implemented using pattern matching, which provides a clean and expressive way to control flow based on its shape or value. Below is an example of a reverse function implemented using pattern matching:

```
let my_rev list =
  let my_rev_impl acc l =
    match l with
    case [] then acc
    case x :: xs then my_rev_impl (x :: acc) xs
  in
  my_rev_impl [] list
```

The code in the example above defines a function named 'my_rev' that takes a list as its input and returns a new list with the elements in reverse order. Inside 'my_rev', a helper function called 'my_rev_impl' is defined to handle the actual reversal logic using tail recursion.

- 'my_rev_impl' takes two parameters:
  - 'acc': short for accumulator, which gradually builds up the reversed list.
  - 'l': the remaining list to process.

It uses pattern matching to deconstruct the list 'l':
- Base case: If 'l' is an empty list '[]', the function returns 'acc`. This means we’ve reached the end of the original list, and the reversed version has been fully accumulated.
- Recursive case: If 'l' is of the form 'x :: xs', meaning it has a head element 'x' and a tail 'xs', the function adds 'x' to the accumulator and recurses on the tail. This builds the reversed list by moving elements one by one from the original list to the front of the accumulator.

Finally, 'my_rev_impl' is called with an empty accumulator '[]' and the original list 'list` to start the reversal process.

Pattern matching will match on exact values if one is provided, but if a variable is used to match against it will assign that value to that variable.

if [1;2;3] is matched against [a; 2; c] the match would be true, and a and c would be assigned to 1 and 3 respectively
if a :: 2 :: c :: [] is matched agaisnt, the result would be the same.

is [1;2;3] is matched against a the entire list would be assigned to variable a.

### Conclusion

That concludes the user guide. Below is a copy of the stdlib for reference.

stdlib.ldr
```
let not b = if b then false else true

let mod a b = a - (b * (int (a/b)))

let rev l =
  let aux acc list =
    match list with
      case [] then acc
      case x :: xs then
        aux (x :: acc) xs;
  aux [] l

let concat l1 l2 =
  let aux acc l =
    match l with
      case [] then rev acc
      case x :: xs then
        aux (x :: acc) xs;
  aux (rev l1) l2

let flatten_tree tree =
  match tree with
    case `Branch [l; r] then
      let ll = flatten_tree l;
      let rl = flatten_tree r;
      concat ll rl
    case `Leaf n then [n]

let filter func list =
  let aux acc l =
    match l with
      case [] then rev acc
      case x :: xs when func x = true then
        aux (x :: acc) xs
      case _ :: xs then
        aux acc xs;
  aux [] list

let map func list =
  let aux acc l =
    match l with
      case [] then rev acc
      case x :: xs then aux ((func x) :: acc) xs;
  aux [] list

let foldl func init list =
  let aux acc l =
    match l with
      case [] then acc
      case x :: xs then aux (func acc x) xs;
  aux init list

let foldr func init list =
  let aux acc l =
    match l with
      case [] then acc
      case x :: xs then aux (func x acc) xs;
  aux init list

let nth list n =
  let aux counter l =
    match l with
      case x :: [] then x
      case x :: _ when counter <= 0 then x
      case x :: xs then aux (counter-1) xs
      case [] then print "empty list"; [];
  aux n list

let mem list val =
  match list with
    case [] then false
    case x :: _ when x = val then true
    case _ :: xs then mem xs val

let head list =
  match list with
    case x :: _ then
      x
    case [] then
      []

let tail list =
  match list with
    case _ :: xs then
      xs
    case [] then
      []

let church n f v = 
  match n with
    case _ when n > 0 then
      church (n-1) f (f v)
    case _ then v

@
let flatten list = 
  let aux acc l = 
    match l with
      case (x :: xs) :: r then
        print (x :: xs);
        [];
  aux [] list
@

let slice list start_idx end_idx =
  let aux acc counter =
    if counter = (end_idx + 1) then rev acc
    else aux ((nth list counter) :: acc) (counter+1);
  aux [] start_idx

let flatten list = 
  let aux acc l =
    match l with
      case [] then rev acc
      case (x :: xs) :: rem then
        let new_acc = aux (x :: acc) xs;
        aux new_acc rem
      case x :: xs then aux (x :: acc) xs;
  aux [] list

let zip left right =
  let aux acc l r =
    match [l; r] with
      case [[]; _] then
        rev acc
      case [_; []] then
        rev acc
      case [x :: xs; y :: ys] then
        aux ([x; y] :: acc) xs ys;
  aux [] left right

let range n = 
  let aux acc c =
    if c = n then rev acc
    else aux (c :: acc) (c+1);
  aux [] 0

let length list = 
  let aux l c =
    match l with
      case [] then c
      case _ :: xs then aux xs (c+1);
  aux list 0

let enumerate list = 
  zip (range (length list)) list

let dedup list = 
  let aux acc l =
    match l with
      case [] then rev acc
      case v :: vs when not (mem acc v) then aux (v :: acc) vs
      case _ :: vs then aux acc vs;
  aux [] list

let alter list idx value =
  let aux acc l =
    match l with
      case [] then rev acc
      case [i; _] :: rem when i = idx then
        aux (value :: acc) rem
      case [_; v] :: rem then aux (v :: acc) rem;
  aux [] (enumerate list)

let alter_2d list x y value = 
  let aux acc l =
    match l with
      case [] then rev acc
      case [i; inner_list] :: rem when i = x then
        let altered = (alter inner_list y value);
        aux (altered :: acc) rem
      case [i; inner_list] :: rem then aux (inner_list :: acc) rem;
  let el = (enumerate list);
  aux [] el

let new_alter list idx value =
  let aux acc l i =
    match [i; l] with
      case [_; []] then rev acc
      case [0; _ :: rem] then aux (value :: acc) rem (i-1)
      case [n; cur :: rem] then aux (cur :: acc) rem (i-1);
  aux [] list idx

let new_alter_2d list x y value =
  let aux acc l i =
    match [i; l] with
      case [_; []] then rev acc
      case [0; inner_list :: rem] then aux ((new_alter inner_list y value) :: acc) rem (i-1)
      case [_; inner_list :: rem] then aux (inner_list :: acc) rem (i-1);
  aux [] list x

let all list =
  let aux l =
    match l with
      case false :: _ then false
      case true :: bs then aux bs
      case [] then true
      case _ then println "non-bool value in all"; false;
  aux list

let any list =
  let aux l =
    match l with
      case false :: bs then aux bs
      case true :: _ then true
      case [] then false
      case thing then println "non-bool list value in any"; false;
  aux list
```
