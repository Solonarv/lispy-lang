# lispy-lang

A pure, functional toy language with a vaguely lisp-like syntax.

It has the following value types:

 - `int`: an integer (of unbounded size)
 - `float`: a double-precision floating point number
 - `string`: a string
 - `symbol`: a symbol, consisting of a valid identifier prefixed by `#`
 - `list`: a list of values (of any length).
 - `function`: a function, which can be applied to one or more values to return another value.

The syntax is as follows:

 - `123` is a literal integer
 - `12.3` is a literal float
 - `"foo bar"` is a literal string
 - `#foo` is the symbol foo
 - `bar` is a variable
 - `'(a b c)` is the list consisting of a, b and c (in that order).
 - `(f a b)` is the function `f` applied to the arguments a and b.
 - `let var = val in body` replaces every occurence of `var` in `body` by `val` and then evaluates `body`
 - `lambda x y z -> body` is an anonymous function that takes three arguments `x`, `y`, `z` and returns `body`

The interpreter comes with the following built-in functions:

 - `+` (variadic) adds all its arguments together. `(+ 2 3 4)` -> `9`
 - `-` subtracts its second argument from the first: `(- 4 2)` -> `2`
 - `*` (variadic) multiplies all its arguments. `(+ 2 3 4)` -> `24`
 - `/` divides its first argument by the second: `(/ 12 3)` -> `4`
 - `show` converts its argument into a string representation.
 - `==` tests its two arguments for equality. Returns `#true` if equal, `#false` if not,
and `#nil` if not applicable (one of the arguments is a function, or they're lists of different length)
 - `typeof` returns the type(s) of its argument(s): `#int`, `#float`, `#string`, `#symbol`, `function<args>` or `#builtin<n>`
(where `n` is the number of arguments the builtin takes, or 'variadic')
 - `if` takes three arguments. `(if cond foo bar)` is `foo` if `cond` is `#false`, `bar` if `cond` is `#true`, and `#nil` otherwise.
 - `match` takes one or more arguments. The first argument must be a symbol. `(match #foo ...)` will search its extra arguments and return the first match.
   - symbols match themselves, so `(match #foo 12 #bar #foo)` will return `#foo`.
   - lists consisting only of a symbol (i.e. `'(#foo)`) are treated the same as the symbol itself
   - an argument of the form `'(#foo val)` will match `#foo` and return `val`
   - an argument of the form `'(#foo x y z ...)` will match `#foo` and return `'(x y z ...)`