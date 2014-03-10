Authors
=======

Dorota Filipczuk    (df1e12@soton.ac.uk)
Vlad Velici         (vsv1g12@soton.ac.uk)

Description of the language
===========================

The language has been designed to work with unbounded streams of data.
The data can be represented as integer or floating-point values. Appropriate functions
have been provided to work with reading and printing these streams. There are also
functions enabling to reverse a bounded stream, and perform calculations on the
stream elements - either on a single element or all of them at once.

A stream in the language is a primitive constructed either from the input file, using the
`input` function or by a user-defined function that takes an integer as its only argument.

The language enables to use if and if-else statements, as well as for, while and do-while
loops. It also has a feature allowing to comment the code.

Names of variables and functions
--------------------------------

Names can be formed by any alphanumeric character including _ and must not start
with a number.

Regex: `[A-Za-z_][A-Za-z0-9_]*`

Types of variables and syntax for it
------------------------------------

<type> can be:

- int                         INTEGER (OCaml int)
- float                       FLOAT (OCaml float)
- bool                        Boolean (true/false literals)
- func(<typelist>)<type>      Function that takes arguments of types in <typelist> and returns <type>
- stream:<type>               A stream of type <type>

<typelist> is a list of types, separated by commas.

Examples:

Float a: `float a`

Function f that takes an int and a float and returns a string: `func(int, float)int f`

Create a new stream
-------------------

A stream is create from:

- input file, using `input(n)` where `n` is an integer from 0 to the number of input streams minus one
- or a function f, using ~f
- or other streams and/or primitives using stream arithmetics: `s+2`

Functions are primitives as well
--------------------------------

You can declare that a variable is of type Function(<typelist>)<type> by either
declaring it as above or implementing it.

    func f (int a, int b) int {
        return a + b
    }

is the same as:

    func(int, int) int f = func(int a, int b) int {
        return a + b
    }

or:

    func(int, int) int f
    f = func (int a, int b) int {
        return a + b
    }

Applying functions
------------------

Considering the function f from above, we can just `f(3, 4)`.

But we can define functions and call them in-place as well:

    func (int a) int {
        return a+1
    }(5)

Which will return `6`.


Primitive operations
--------------------

`+` addition

`-` substraction

`/` division

`*` multiplication

`%` modulo

`^` exponent

`~` create a stream from function

`<<` read from a stream

`!` not

`&&` and

`||` or


If statements
--------------

   if (<condition>) {
        <body>
    }

or

    if (<condition>) {
        <body>
    } else {
        <body2>
    }

Loops
-----

For:

    for (<init>, <cond>, <afterthought>) {
        <body>
    }

while:

    while (<cond>) {
        <body>
    }

do-while

   do {
        <body>
    } while (<cond>) 

Scoping
-------

Scheme-like Scoping for functions.

Inside loops and if statements, a new scope is created.

