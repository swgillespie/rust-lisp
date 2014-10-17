# rust-lisp #

Rust is my new favorite language and I've really wanted to use it in a project.
I like languages so I wrote a minimal Lisp-like interpreter. It doesn't do a whole lot now
but there'll be more stuff as I find the time.

## Currently Supported ##
* Basic arithmetic (+, -, *)
* `car` and `cdr`
* Quoting and unquoting (but not quasiquoting... yet)
* Defining functions with `defun`
* Defining and calling functions written in Rust
* `if`, `define`, `quote`, `defun` fundamental forms

## TODO list ##

- [ ] `lambda` form and closures
- [ ] `quasiquote` form
- [ ] `defmacro`, `unquote`, and macros
- [ ] `eval` and `read`
- [ ] a standard library
- [ ] bytecode interpreter?
- [ ] mark and sweep GC?

Rust is awesome!