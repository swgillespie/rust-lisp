# rust-lisp [![Build Status](https://travis-ci.org/swgillespie/rust-lisp.svg?branch=master)](https://travis-ci.org/swgillespie/rust-lisp/)

Rust is my new favorite language and I've really wanted to use it in a project.
I like languages so I wrote a minimal Lisp-like interpreter. It draws heavy inspiration
from both Common Lisp and Scheme.

## Building and running ##
This project builds using Cargo. To build, clone this repository and run
```
cargo build
./target/rust-lisp
```
or, simply,
```
cargo run
```

```cargo test``` will run the tests, and ```cargo bench``` will run the benchmarks

## Features ##
This lisp interpreter draws its heaviest inspiration from Scheme and most of its feature
come directly from it.

### Fundamental Forms ###
This interpreter has ten fundamental forms as of right now:
```
if    defun    defmacro  define  lambda
quote unquote  and       or      quasiquote
```
`defun` will be a macro in the future. Each one of these represents a fundamental
feature of this lisp:

* `(if condition true_branch false_branch?)` - Evaluates condition and executes the true branch
if condition is truthy, otherwise it executes false branch.
* `(defun <symbol> parameters body)` - Define a function named <symbol> with the given
parameter list and body. Places the function into the global namespace.
* `(defmacro <symbol> parameter_form body_form)` - Defines a macro, not yet implemented.
* `(define symbol form)` - Evaluates form and binds it to symbol in the global namespace.
* `(lambda parameters body)` - Creates an anonymous function with parameter list and body.
* `(quote form)` - Returns the form unevaluated.
* `(unquote form)` - If currently evaluating a `quasiquote` form, escapes the quote and evaluates
`form` as it would normally. Only that form is evaluated. This is not usually called directly -
it is almost always invoked using the `,` reader macro.
* `(and form*)` - Evaluates every form unless one of them is `#f`, after which all other
forms will not be evaluated (short circuit evaluation).
* `(or form*)` - Evaluates every form unless one of them is truthy, after which all other forms
will not be evaluated.
* `(quasiquote form)` - Increments the "quasiquotation" level of the form. This behaves exactly
as the `quote` form, except that `unquote` and `unquote-splicing` allow for the select evaluation
of forms. This is not usually called directly - it is almost always invoked using the \` reader macro.
* `(unquote-splicing form)` - Behaves the same as `unquote`, except that if form evaluates to a list,
it will flatten the list, and otherwise it will produce an improper list. This is not usually called directly -
it is almost always invoked using the `,@` reader macro.

### Multiple evaluation ###
This interpreter will evaluate every form that it is given. As an example:
```
lisp> (+ 1 2) (+ 3 4) (+ 5 6)
$0 = 3
$1 = 7
$2 = 11
```

In the future these `$x` variables may be bound to the current environment, but
they aren't right now.

### Nil vs Empty List ###
This interpreter takes the side of Scheme on the nil vs empty list debate. `nil`
is an ordinary symbol and carries no extra meaning. The empty list is written as
`'()`, as in Scheme, and means the same thing:

```
lisp> (cdr '(1))
$0 = ()
```
Attempting to evaluate an unquoted `()` will result in an error.

### Improper parameter lists ###

Improper lists can be used as the parameter form in a lambda, defun, or defmacro. An
improper list binds all remaining parameters to the final parameter. As an example,

```
lisp> (defun test (first second third . rest) rest)
$0 = <function>
```

This defines a function that takes three or more parameters. The first
parameter is always bound to `first`, the second to `second`, and the third
to `third`. If there are more than three parameters, the remaining parameters
are bound as a list to `rest`. For example,

```
lisp> (test 1 2 3 4 5)
$0 = (4 5)
```

When there are exactly as many parameters as there are required parameters,
the rest parameter is the null list. For example,

```
lisp> (test 1 2 3)
$0 = ()
```

## TODO list

- [x] `lambda` form and closures
- [ ] `defmacro`, `unquote`, `quasiquote`, and macros
- [ ] `eval`, `read`, and `macro-expand`
- [ ] don't trap on stack overflows
- [ ] do something about integer overflows
- [ ] a standard library
- [ ] bytecode interpreter?
- [ ] mark and sweep GC?
- [ ] WRITE MORE TESTS

Rust is awesome!
