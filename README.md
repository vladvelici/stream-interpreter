stream-interpreter
==================

Compiling:

    make depend
    make

Make depend is required only once or after changes
that might influence the dependency of files.

Names of variables and functions
--------------------------------

Names can be formed by any alphanumeric character including _ and must not start
with a number.

Regex: `[A-Za-z_][A-Za-z0-9_]*`

Types of variables and syntax for it
------------------------------------

<type> can be:

int                         INTEGER (32 or 64, depending or arch - basically an OCaml int)
float                       FLOAT 64bit (OCaml float)
string                      String
bool                        Boolean (true/false literals)
func(<typelist>)<type>      Function that takes arguments of types in <typelist> and returns <type>

<typelist> is a list of types, separated by commas.

Example of declarations
-----------------------

Float a: `float a`

Function f that takes an int and a float and returns a string: `func(int, float)string f`

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


Operations
----------

+, -, /, %, ^...

conditionals, loops
-------------------

if (else/elseif), switch, for, while



new stream(func (arguments) int  {} )

new stream( int (arguments) int {} )

scoping
-------

scheme-like scoping

Lists used in the Parser
========================

All list items are separated by commas.

<arglist> - a list of names of variables and their types, mainly used for
function implementation. Might be used for declaring more variables in one go as well.

<vallist> - a list of values. Primitives or variable names. If a variable name
is given in a vallist and it is not bound in the scope, there will be an
exception at runtime.

<typelist> - a list of types. Used in function declaration (not implementation).
