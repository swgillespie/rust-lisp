#![feature(box_syntax, box_patterns)]

use std::fmt::{self, Error, Formatter};

use std::rc::Rc;
use std::ops::Deref;
use std::collections::HashSet;

use reader;
use intrinsics;
use environment;

use reader::Sexp;



// The LispValue enum is the type of all Lisp values at runtime. These are
// the same as the S-expression representation, except that functions can also
// be values. LispValues are always used as a reference counted pointer.
#[derive(Debug, PartialEq)]
pub enum LispValue {
    Int(i64),
    Float(f64),
    Str(String),
    Bool(bool),
    Symbol(String),
    Func(Function),
    Cons(Rc<LispValue>, Rc<LispValue>),
    Nil
}

impl fmt::Display for LispValue {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), Error> {
        write!(fmt, "{}", self.pretty_print())
    }
}

impl LispValue {
    // Simple algorithm for pretty-printing an S-expression.
    // The algorithm goes like this:
    // 1) If self isn't a list, print it and return.
    // 2) If self is a list, print an open paren.
    //   2a) Print the car of the list.
    //   2b) If cdr is a list, recurse to step 2a with cdr as the new list
    //   2c) If cdr is nil, print nothing,
    //   2d) If cdr is anything else, print a "." followed by a space
    //       and recursively print the cdr.
    // This function returns a string so "printing" isn't done, but it's basically
    // the same thing.
    pub fn pretty_print(&self) -> String {
        match *self {
            LispValue::Int(v) => v.to_string(),
            LispValue::Float(v) => v.to_string(),
            LispValue::Str(ref v) => format!("\"{}\"", v),
            LispValue::Symbol(ref v) => format!("{}", v),
            LispValue::Cons(ref car, ref cdr) => {
                let (s_car, s_cdr) = self.print_cons(&**car, &**cdr);
                format!("({} {})", s_car, s_cdr)
            },
            LispValue::Nil => "()".to_string(),
            LispValue::Bool(v) => if v {
                "#t".to_string()
            } else {
                "#f".to_string()
            },
            LispValue::Func(ref c) => format!("{}", c)
        }
    }

    fn print_cons(&self, car: &LispValue, cdr: &LispValue) -> (String, String) {
        let car_str = car.pretty_print();
        let cdr_str = match *cdr {
            LispValue::Cons(ref c_car, ref c_cdr) => {
                let (s_car, s_cdr) = self.print_cons(&**c_car, &**c_cdr);
                if s_cdr.len() == 0 {
                    format!("{}", s_car)
                } else {
                    format!("{} {}", s_car, s_cdr)
                }
            }
            LispValue::Nil => "".to_string(),
            _ => format!(". {}", cdr.pretty_print())
        };
        (car_str, cdr_str)
    }
}

#[derive(Debug)]
pub enum FunctionParameters {
    Fixed(Vec<String>),
    Variadic(Vec<String>, String)
}

impl FunctionParameters {
    pub fn get_variables(&self) -> Vec<String> {
        match *self {
            FunctionParameters::Fixed(ref vec) => vec.clone(),
            FunctionParameters::Variadic(ref vec, ref rest) => {
                let mut temp = vec.clone();
                temp.push(rest.clone());
                temp
            }
        }
    }
}

type Closure = Vec<(String, Option<Rc<LispValue>>)>;

// Functions can be one of three things - an internal function (defined by defun or
// lambda), an external Rust function exposed to the interpreter, or a macro.
// Macros aren't supported yet.
#[allow(dead_code)] // macros aren't used yet
#[derive(Debug)]
pub enum Function {
    InternalFunction(FunctionParameters, Rc<reader::Sexp>, Closure),
    ExternalFunction(String, fn(Vec<Rc<LispValue>>) -> EvalResult),
    Macro(FunctionParameters, Rc<reader::Sexp>),
}

// Impl of PartialEq for the Function type indicating that functions can never
// be equal to one another.
impl PartialEq for Function {
    fn eq(&self, _: &Function) -> bool {
        false
    }

    fn ne(&self, _: &Function) -> bool {
        true
    }
}

impl fmt::Display for Function {
    fn fmt (&self, fmt: &mut Formatter) -> Result<(), Error> {
        match *self {
            Function::InternalFunction(_, _, _) => write!(fmt, "<function>"),
            Function::ExternalFunction(_, _) => write!(fmt, "<external function>"),
            Function::Macro(_, _) => write!(fmt, "<macro>")
        }
    }
}

pub type EvalResult = Result<Rc<LispValue>, String>;

pub struct Interpreter {
    environment: environment::Environment,
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut interpreter = Interpreter {
            environment: environment::Environment::new(),
        };
        interpreter.load_intrinsics();
        interpreter
    }

    fn load_intrinsics(&mut self) {
        self.expose_external_function("+".to_string(), intrinsics::add);
        self.expose_external_function("-".to_string(), intrinsics::sub);
        self.expose_external_function("*".to_string(), intrinsics::mul);
        self.expose_external_function("/".to_string(), intrinsics::div);
        self.expose_external_function("car".to_string(), intrinsics::car);
        self.expose_external_function("cdr".to_string(), intrinsics::cdr);
        self.expose_external_function("cons".to_string(), intrinsics::cons);
        self.expose_external_function("=".to_string(), intrinsics::eq);
        self.expose_external_function("display".to_string(), intrinsics::display);
        self.expose_external_function("pair?".to_string(), intrinsics::pair);
    }

    pub fn eval(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Int(i) => Ok(Rc::new(LispValue::Int(i))),
            Sexp::Float(f) => Ok(Rc::new(LispValue::Float(f))),
            Sexp::Str(ref s) => Ok(Rc::new(LispValue::Str(s.clone()))),
            Sexp::Symbol(ref s) => self.eval_symbol(s.clone()),
            Sexp::Boolean(b) => Ok(Rc::new(LispValue::Bool(b))),
            Sexp::Cons(box Sexp::Symbol(ref s), ref right) if self.is_intrinsic(s) => self.eval_intrinsic(s, &**right),
            Sexp::Cons(ref left, ref right) => self.eval_function(&**left, &**right),
            Sexp::Nil => Err("Unknown form ()".to_string()),
            _ => unreachable!()
        }
    }
    
    pub fn expose_external_function(&mut self, name: String, func: fn(Vec<Rc<LispValue>>) -> EvalResult) {
        let wrapped_func = LispValue::Func(Function::ExternalFunction(name.clone(), func));
        self.environment.put(name.clone(), Rc::new(wrapped_func));
    }

    fn is_intrinsic(&self, name: &String) -> bool {
        match name.as_str() {
            "if"
                | "defun"
                | "defmacro"
                | "lambda"
                | "define"
                | "quote"
                | "unquote"
                | "and"
                | "or"
                | "progn"
                | "quasiquote" => true,
            _ => false
        }
    }

    fn eval_intrinsic(&mut self, name: &String, sexp: &reader::Sexp) -> EvalResult {
        match name.as_str() {
            "quote" => self.eval_quote(sexp),
            "unquote" => Err("unquote not valid outside of quasiquote form".to_string()),
            "lambda" => self.eval_lambda(sexp),
            "if" => self.eval_if(sexp),
            "define" => self.eval_define(sexp),
            "quasiquote" => self.eval_quasiquote(sexp),
            "defun" => self.eval_defun(sexp),
            "and" => self.eval_and(sexp),
            "or" => self.eval_or(sexp),
            "defmacro" => self.eval_defmacro(sexp),
            "progn" => self.eval_progn(sexp),
            _ => unreachable!()
        }
    }

    fn eval_progn(&mut self, sexp: &reader::Sexp) -> EvalResult {
        let mut last : Option<Rc<LispValue>> = None;
        let mut cursor = sexp;
        loop {
            match *cursor {
                Sexp::Nil => return Ok(last.unwrap_or(Rc::new(LispValue::Nil))),
                Sexp::Cons(ref car, ref cdr) => {
                    let result = try!(self.eval(&**car));
                    last = Some(result.clone());
                    cursor = &**cdr;
                }
                _ => return Err("Improper lists disallowed in progn form".to_string())
            }
        }
    } 

    fn eval_and(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(ref car, ref cdr) => match self.eval(&**car) {
                Ok(val) => match val.deref() {
                    &LispValue::Bool(false) => Ok(val.clone()),
                    _ => self.eval_and(&**cdr)
                },
                Err(e) => Err(e)
            },
            Sexp::Nil => Ok(Rc::new(LispValue::Bool(true))),
            ref e => match self.eval(e) {
                Ok(val) => match val.deref() {
                    &LispValue::Bool(false) => Ok(val.clone()),
                    _ => Ok(Rc::new(LispValue::Bool(true)))
                },
                Err(e) => Err(e)
            }
        }
    }

    fn eval_or(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(ref car, ref cdr) => match self.eval(&**car) {
                Ok(val) => match val.deref() {
                    &LispValue::Bool(true) => Ok(val.clone()),
                    _ => self.eval_or(&**cdr)
                },
                Err(e) => Err(e)
            },
            Sexp::Nil => Ok(Rc::new(LispValue::Bool(false))),
            ref e => match self.eval(e) {
                Ok(val) => match val.deref() {
                    &LispValue::Bool(false) => Ok(val.clone()),
                    &LispValue::Nil => Ok(Rc::new(LispValue::Bool(false))),
                    _ => Ok(Rc::new(LispValue::Bool(true)))
                },
                Err(e) => Err(e)
            }
        }
    }

    fn eval_quote(&mut self, sexp: &reader::Sexp) -> EvalResult {
        fn sexp_to_lvalue(s: &reader::Sexp) -> Rc<LispValue> {
            match *s {
                Sexp::Int(i) => Rc::new(LispValue::Int(i)),
                Sexp::Float(i) => Rc::new(LispValue::Float(i)),
                Sexp::Str(ref s) => Rc::new(LispValue::Str(s.clone())),
                Sexp::Symbol(ref s) => Rc::new(LispValue::Symbol(s.clone())),
                Sexp::Boolean(b) => Rc::new(LispValue::Bool(b)),
                Sexp::Cons(ref car, ref cdr) => Rc::new(LispValue::Cons(sexp_to_lvalue(&**car), sexp_to_lvalue(&**cdr))),
                Sexp::Nil => Rc::new(LispValue::Nil),
                _ => unreachable!()
            }
        }
        Ok(sexp_to_lvalue(sexp))
    }

    fn eval_if(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(ref condition,
                         box Sexp::Cons(ref true_branch,
                                          box Sexp::Cons(ref false_branch,
                                                           box Sexp::Nil))) => {
                let cond = try!(self.eval(&**condition));
                if let &LispValue::Bool(false) = cond.deref() {
                    self.eval(&**false_branch)
                } else {
                    self.eval(&**true_branch)
                }
            }
            Sexp::Cons(ref condition,
                         box Sexp::Cons(ref true_branch,
                                          box Sexp::Nil)) => {
                let cond = try!(self.eval(&**condition));
                if let &LispValue::Bool(false) = cond.deref() {
                    Ok(Rc::new(LispValue::Nil))
                } else {
                    self.eval(&**true_branch)
                }
            }
            _ => Err("Invalid pattern for if form".to_string())
        }
    }

    fn eval_define(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(box Sexp::Symbol(ref sym), box Sexp::Cons(ref exp, box Sexp::Nil)) => {
                let value = try!(self.eval(&**exp));
                self.environment.put_global(sym.clone(), value.clone());
                Ok(value)
            }
            Sexp::Cons(box Sexp::Symbol(ref sym), ref exp) => {
                let value = try!(self.eval(&**exp));
                self.environment.put_global(sym.clone(), value.clone());
                Ok(value)
            }
            Sexp::Cons(ref other, _) => Err(format!("Not a symbol: {:?}", other)),
            _ => Err(format!("No arguments to define form"))
        }
    }

    // algorithm for evaluating a quasiquoted S-expression:
    // 1) incremenent the quasiquote counter
    // 2) if the atom isn't a list, evaluate it as a quoted atom and decrement the quasiquote counter.
    // 3) if the atom is a list...
    //  a) If the car of the list is unquoted, decrement the quasiquote counter. If the quasiquote
    //     counter is zero, evaluate the car of the list. If the counter is less than zero, error.
    //     If the counter is greater than zero, evaluate the car of the list as a quoted atom.
    //  b) If the car of the list is not quoted, evaluate the car of the list as a quoted atom.
    //  c) Evaluate the cdr of the list (goto step 2)
    fn eval_quasiquote(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(box Sexp::Cons(box Sexp::Symbol(ref s), ref quoted), ref cdr) if *s == "unquote".to_string() => {
                let result = try!(self.eval(&**quoted));
                let rest = try!(self.eval_quasiquote(&**cdr));
                Ok(Rc::new(LispValue::Cons(result, rest)))
            },
            Sexp::Cons(box Sexp::Symbol(ref s), ref quoted) if *s == "unquote".to_string() => {
                let result = try!(self.eval(&**quoted));
                Ok(result)
            },
            Sexp::Cons(box Sexp::Cons(box Sexp::Symbol(ref s), ref quoted), box Sexp::Nil) if *s == "unquote-splicing".to_string() => {
                let result = try!(self.eval(&**quoted));
                Ok(result)
            }
            Sexp::Cons(box Sexp::Cons(box Sexp::Symbol(ref s), _), _) if *s == "unquote-splicing".to_string() => {
                Err("Invalid unquote-splicing form".to_string())
            }
            Sexp::Cons(box Sexp::Symbol(ref s), _) if *s == "unquote-splicing".to_string() => {
                Err("Invalid unquote-splicing form".to_string())
            },
            Sexp::Cons(ref car, ref cdr) => {
                let result = try!(self.eval_quote(&**car));
                let rest = try!(self.eval_quasiquote(&**cdr));
                Ok(Rc::new(LispValue::Cons(result, rest)))
            }
            ref val => self.eval_quote(val)
        }
    }

    fn eval_defun(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(box Sexp::Symbol(ref sym),
                         box Sexp::Cons(ref parameters,
                                          box Sexp::Cons(ref body,
                                                           box Sexp::Nil))) => {
                let params = self.eval_list_as_parameter_list(&**parameters);
                let free_vars = self.get_free_variables(params.get_variables(), &**body);
                let closure = free_vars.into_iter().map(|x| (x.clone(), self.environment.get(x.clone()).clone())).collect();
                let func = Rc::new(LispValue::Func(Function::InternalFunction(params, Rc::new(*body.clone()), closure)));
                self.environment.put_global(sym.clone(), func.clone());
                Ok(func)
            }
            Sexp::Cons(ref other, _) => Err(format!("Not a symbol: {:?}", other)),
            _ => Err("No arguments to defun form".to_string())
        }
    }

    fn eval_lambda(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(ref parameters,
                         box Sexp::Cons(ref body,
                                          box Sexp::Nil)) => {
                let params = self.eval_list_as_parameter_list(&**parameters);
                let free_vars = self.get_free_variables(params.get_variables(), &**body);
                let closure = free_vars.into_iter().map(|x| (x.clone(), self.environment.get(x.clone()).clone())).collect();
                let func = LispValue::Func(Function::InternalFunction(params, Rc::new(*body.clone()), closure));
                Ok(Rc::new(func))
            }
            _ => Err("Incorrect pattern for lambda form".to_string())
        }
    }

    fn eval_symbol(&mut self, sym: String) -> EvalResult {
        match self.environment.get(sym.clone()) {
            Some(value) => Ok(value),
            None => Err(format!("Unbound symbol: {}", sym))
        }
    }

    fn eval_function(&mut self, car: &reader::Sexp, cdr: &reader::Sexp) -> EvalResult {
        let sym_val = try!(self.eval(car));
        match *sym_val {
            LispValue::Func(ref f) => match *f {
                Function::InternalFunction(ref parameters, ref body, ref closure) => self.eval_internal_function(cdr, parameters, body.clone(), closure),
                Function::ExternalFunction(_, func) => self.eval_external_function(cdr, func),
                Function::Macro(ref parameters, ref body) => self.eval_macro(cdr, parameters, body.clone())
            },
            _ => Err(format!("Value is not callable: {}", sym_val))
        }
    }

    fn eval_internal_function(&mut self,
                              actual_params: &reader::Sexp,
                              formal_params: &FunctionParameters,
                              body: Rc<reader::Sexp>,
                              closure: &Closure) -> EvalResult {
        let params = try!(self.sexp_map(actual_params, |s, t| s.eval(t)));
        let actual : &Vec<String> = match *formal_params {
            FunctionParameters::Fixed(ref vec) => {
                if params.len() != vec.len() {
                    return Err("Incorrect number of parameters".to_string());
                }
                vec
            },
            FunctionParameters::Variadic(ref vec, _) => {
                if params.len() < vec.len() {
                    return Err("Incorrect number of parameters".to_string());
                }
                vec
            }
        };
        self.environment.enter_scope();
        for (value, binding) in params.iter().zip(actual.iter()) {
            self.environment.put(binding.clone(), value.clone());
        }
        if let FunctionParameters::Variadic(_, ref rest) = *formal_params {
            let rest_as_vector = params.iter().skip(actual.len());
            let list = self.iter_to_list(rest_as_vector);
            self.environment.put(rest.clone(), list);
        }
        for &(ref binding, ref value) in closure.iter() {
            match *value {
                Some(ref v) => self.environment.put(binding.clone(), v.clone()),
                None => match self.environment.get(binding.clone()) {
                    Some(ref v) => self.environment.put(binding.clone(), v.clone()),
                    None => return Err(format!("unbound variable: {}", binding.clone()))
                }
            }
        }
        let result = self.eval(body.deref());
        self.environment.exit_scope();
        result
    }

    fn iter_to_list<'a,
                    T: Iterator<Item = &'a Rc<LispValue>>
                    >(&mut self, mut iter: T) -> Rc<LispValue>
    {
        match iter.next() {
            Some(v) => Rc::new(LispValue::Cons(v.clone(), self.iter_to_list(iter))),
            None => Rc::new(LispValue::Nil)
        }
    }

    fn eval_external_function(&mut self,
                              actual_params: &reader::Sexp,
                              func: fn(Vec<Rc<LispValue>>) -> EvalResult) -> EvalResult {
        match self.sexp_map(actual_params, |s, t| s.eval(t)) {
            Ok(v) => func(v),
            Err(e) => Err(e)
        }
    }

    fn eval_macro(&mut self,
                  actual_params: &reader::Sexp,
                  formal_params: &FunctionParameters,
                  body: Rc<reader::Sexp>) -> EvalResult {
        let params = try!(self.sexp_map(actual_params, |s, t| s.eval_quote(t)));
        let actual : &Vec<String> = match *formal_params {
            FunctionParameters::Fixed(ref vec) => {
                if params.len() != vec.len() {
                    return Err("Incorrect number of parameters".to_string());
                }
                vec
            },
            FunctionParameters::Variadic(ref vec, _) => {
                if params.len() < vec.len() {
                    return Err("Incorrect number of parameters".to_string());
                }
                vec
            }
        };
        self.environment.enter_scope();
        for (value, binding) in params.iter().zip(actual.iter()) {
            self.environment.put(binding.clone(), value.clone());
        }
        if let FunctionParameters::Variadic(_, ref rest) = *formal_params {
            let rest_as_vector = params.iter().skip(actual.len());
            let list = self.iter_to_list(rest_as_vector);
            self.environment.put(rest.clone(), list);
        }
        let result = self.eval(body.deref());
        self.environment.exit_scope();
        if let Ok(value) = result {
            let sexp = self.value_to_sexp(value);
            self.eval(&sexp)
        } else {
            result
        }
    }

    fn eval_defmacro(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            Sexp::Cons(box Sexp::Symbol(ref sym),
                         box Sexp::Cons(ref parameters,
                                          box Sexp::Cons(ref body,
                                                           box Sexp::Nil))) => {
                let params = self.eval_list_as_parameter_list(&**parameters);
                let macro_thing = Rc::new(LispValue::Func(Function::Macro(params, Rc::new(*body.clone()))));
                self.environment.put_global(sym.clone(), macro_thing.clone());
                Ok(macro_thing)
            }
            _ => Err("Not a legal defmacro pattern".to_string())
        }
    }

    fn sexp_map<F>(&mut self, params: &reader::Sexp,
                func: F
    ) -> Result<Vec<Rc<LispValue>>, String>
        where F: for<'a> Fn(&'a mut Interpreter, &'a Sexp) -> EvalResult
    {
        match *params {
            Sexp::Cons(ref car, ref cdr) => {
                let mut out_vec = vec![];
                match func(self, &**car) {
                    Ok(v) => out_vec.push(v),
                    Err(e) => return Err(e)
                };
                match self.sexp_map(&**cdr, func) {
                    Ok(vec) => {
                        out_vec.extend(vec.into_iter());
                        Ok(out_vec)
                    }
                    Err(e) => Err(e)
                }
            }
            Sexp::Nil => Ok(vec![]),
            _ => Err("Cannot use an improper list as parameters to a function".to_string())
        }
    }

    fn value_to_sexp(&self, value: Rc<LispValue>) -> reader::Sexp {
        match *value {
            LispValue::Int(i) => Sexp::Int(i),
            LispValue::Float(i) => Sexp::Float(i),
            LispValue::Str(ref s) => Sexp::Str(s.clone()),
            LispValue::Bool(b) => Sexp::Boolean(b),
            LispValue::Symbol(ref s) => Sexp::Symbol(s.clone()),
            LispValue::Cons(ref car, ref cdr) => Sexp::Cons(box self.value_to_sexp(car.clone()),
                                                   box self.value_to_sexp(cdr.clone())),
            LispValue::Nil => Sexp::Nil,
            _ => unreachable!()
        }
    }
    
    // The function is similar to eval_list_as_parameters, but it just gets the names
    // of all of the symbols in the linked list instead of evaluating them. This
    // is also used for evaluating parameters for a function call.
    fn eval_list_as_parameter_list(&self, params: &reader::Sexp) -> FunctionParameters {
        let mut cursor = params;
        let mut out = vec![];
        let mut out_rest = None;
        loop {
            match *cursor {
                Sexp::Cons(box Sexp::Symbol(ref s), box Sexp::Symbol(ref rest)) => {
                    out.push(s.clone());
                    out_rest = Some(rest.clone());
                    break;
                },
                Sexp::Cons(box Sexp::Symbol(ref s), ref cdr) => {
                    out.push(s.clone());
                    cursor = &**cdr;
                },
                Sexp::Nil => break,
                _ => unreachable!()
            };
        }
        if out_rest.is_some() {
            FunctionParameters::Variadic(out, out_rest.unwrap())
        } else {
            FunctionParameters::Fixed(out)
        }
    }

    fn get_free_variables(&self, variables: Vec<String>, body: &Sexp) -> Vec<String> {
        let parameter_set : HashSet<String> = variables.iter().map(|&ref x| x.clone()).collect();
        let variable_set : HashSet<String> = self.get_variables(body);
        variable_set.difference(&parameter_set).map(|&ref x| x.clone()).collect()
    }

    fn get_variables(&self, body: &Sexp) -> HashSet<String> {
        match *body {
            Sexp::Symbol(ref s) if !self.is_intrinsic(s) => {
                let mut set = HashSet::new();
                set.insert(s.clone());
                set
            },
            Sexp::Cons(ref car, ref cdr) => {
                let free_car = self.get_variables(&**car);
                let free_cdr = self.get_variables(&**cdr);
                free_car.union(&free_cdr).map(|&ref x| x.clone()).collect()
            },
            _ => HashSet::new()
        }
    }

}

#[cfg(test)]
mod tests {
    extern crate test;

    use super::*;
    use self::test::Bencher;

    use super::super::reader;
    use std::process::Command;

    fn evaluate(input: &'static str) -> EvalResult {
        let mut interpreter = Interpreter::new();
        evaluate_with_context(input, &mut interpreter)
    }

    fn evaluate_with_context(input: &'static str, interpreter: &mut Interpreter) -> EvalResult {
        let mut reader = reader::SexpReader::new();
        match reader.parse_str(input) {
            Ok(e) => match interpreter.eval(&e) {
                Ok(val) => Ok(val),
                Err(e) => Err(e)
            },
            Err(e) => Err(e)
        }
    }

    #[test]
    fn test_addition() {
        if let Ok(val) = evaluate("(+ 1 2)") {
            match val.deref() {
                &LispValue::Int(x) => assert_eq!(x, 3),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_varargs_addition() {
        if let Ok(val) = evaluate("(+ 5 5 5 5 5)") {
            match val.deref() {
                &LispValue::Int(x) => assert_eq!(x, 25),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }        
    }

    #[test]
    fn test_subtraction() {
        if let Ok(val) = evaluate("(- 1 2)") {
            match val.deref() {
                &LispValue::Int(x) => assert_eq!(x, -1),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_division() {
        if let Ok(val) = evaluate("(/ 8 2 2.5)") {
            match val.deref() {
                &LispValue::Float(x) => assert_eq!(x, 1.6),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_varargs_subtraction() {
        if let Ok(val) = evaluate("(- 5 1 1 1)") {
            match val.deref() {
                &LispValue::Int(x) => assert_eq!(x, 2),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_car() {
        if let Ok(val) = evaluate("(car '(1 2))") {
            match val.deref() {
                &LispValue::Int(x) => assert_eq!(x, 1),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_cdr() {
        if let Ok(val) = evaluate("(cdr '(1 2))") {
            match val.deref() {
                &LispValue::Cons(ref car, _) => match car.deref() {
                    &LispValue::Int(a) => assert_eq!(a, 2),
                    _ => panic!("Unexpected: {}", car)
                },
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_if_true() {
        if let Ok(val) = evaluate("(if #t 1 2)") {
            match val.deref() {
                &LispValue::Int(a) => assert_eq!(a, 1),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
        
    }

    #[test]
    fn test_if_false() {
        if let Ok(val) = evaluate("(if #f 1 2)") {
            match val.deref() {
                &LispValue::Int(a) => assert_eq!(a, 2),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_if_true_no_false_branch() {
        if let Ok(val) = evaluate("(if #t 1)") {
            match val.deref() {
                &LispValue::Int(a) => assert_eq!(a, 1),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_if_false_no_false_branch() {
        if let Ok(val) = evaluate("(if #f 1)") {
            match val.deref() {
                &LispValue::Nil => (),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_basic_defun() {
        let mut interpreter = Interpreter::new();
        if let Ok(_) = evaluate_with_context("(defun square (x) (* x x))", &mut interpreter) {
            if let Ok(val) = evaluate_with_context("(square 5)", &mut interpreter) {
                match val.deref() {
                    &LispValue::Int(v) => assert_eq!(v, 25),
                    e => panic!("Unexpected: {}", e)
                }
            } else {
                panic!("Unexpected error");
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_recursive_defun() {
        let mut interpreter = Interpreter::new();
        if let Ok(_) = evaluate_with_context("(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))", &mut interpreter) {
            if let Ok(val) = evaluate_with_context("(fact 5)", &mut interpreter) {
                match val.deref() {
                    &LispValue::Int(v) => assert_eq!(v, 120),
                    e => panic!("Unexpected: {}", e)
                }
            } else {
                panic!("Unexpected error");
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_and_short_circuit() {
        if let Ok(val) = evaluate("(and #f asdfjsldlf)") {
            match val.deref() {
                &LispValue::Bool(b) => assert!(!b),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_or_short_circuit() {
        if let Ok(val) = evaluate("(or #t asdfjsldlf)") {
            match val.deref() {
                &LispValue::Bool(b) => assert!(b),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_or() {
        if let Ok(val) = evaluate("(or #f #f)") {
            match val.deref() {
                &LispValue::Bool(b) => assert!(!b),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_and() {
        if let Ok(val) = evaluate("(and #t #t)") {
            match val.deref() {
                &LispValue::Bool(b) => assert!(b),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_lambda() {
        if let Ok(val) = evaluate("((lambda (x) (* x x)) 5)") {
            match val.deref() {
                &LispValue::Int(b) => assert_eq!(b, 25),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }        
    }

    #[test]
    fn test_atom_quasiquote_unquote() {
        if let Ok(val) = evaluate("`,10") {
            match val.deref() {
                &LispValue::Int(b) => assert_eq!(b, 10),
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }        
    }

    #[test]
    fn test_quasiquote_with_no_unquote() {
        if let Ok(val) = evaluate("`(1 2 3)") {
            match val.deref() {
                &LispValue::Cons(ref car, ref cdr) => {
                    assert_eq!(car.deref(), &LispValue::Int(1));
                    match cdr.deref() {
                        &LispValue::Cons(ref car_2, ref cdr_2) => {
                            assert_eq!(car_2.deref(), &LispValue::Int(2));
                            match cdr_2.deref() {
                                &LispValue::Cons(ref car_3, ref cdr_3) => {
                                    assert_eq!(car_3.deref(), &LispValue::Int(3));
                                    assert_eq!(cdr_3.deref(), &LispValue::Nil);
                                },
                                e => panic!("Unexpected: {}", e)
                            }
                        },
                        e => panic!("Unexpected: {}", e)
                    }
                }
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }        
    }

    #[test]
    fn test_quasiquote_with_unquote() {
        if let Ok(val) = evaluate("`(1 2 ,(* 2 2))") {
            match val.deref() {
                &LispValue::Cons(ref car, ref cdr) => {
                    assert_eq!(car.deref(), &LispValue::Int(1));
                    match cdr.deref() {
                        &LispValue::Cons(ref car_2, ref cdr_2) => {
                            assert_eq!(car_2.deref(), &LispValue::Int(2));
                            match cdr_2.deref() {
                                &LispValue::Cons(ref car_3, ref cdr_3) => {
                                    assert_eq!(car_3.deref(), &LispValue::Int(4));
                                    assert_eq!(cdr_3.deref(), &LispValue::Nil);
                                },
                                e => panic!("Unexpected: {}", e)
                            }
                        },
                        e => panic!("Unexpected: {}", e)
                    }
                }
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }        
    }

    #[test]
    fn test_quasiquote_with_unquote_splicing() {
        if let Ok(val) = evaluate("`(1 ,@'(2 3))") {
            match val.deref() {
                &LispValue::Cons(ref car, ref cdr) => {
                    assert_eq!(car.deref(), &LispValue::Int(1));
                    match cdr.deref() {
                        &LispValue::Cons(ref car_2, ref cdr_2) => {
                            assert_eq!(car_2.deref(), &LispValue::Int(2));
                            match cdr_2.deref() {
                                &LispValue::Cons(ref car_3, ref cdr_3) => {
                                    assert_eq!(car_3.deref(), &LispValue::Int(3));
                                    assert_eq!(cdr_3.deref(), &LispValue::Nil);
                                },
                                e => panic!("Unexpected: {}", e)
                            }
                        },
                        e => panic!("Unexpected: {}", e)
                    }
                }
                e => panic!("Unexpected: {}", e)
            }
        } else {
            panic!("Unexpected error")
        }        
    }



    #[test]
    fn test_higher_order_function() {
        let mut interpreter = Interpreter::new();
        if let Ok(_) = evaluate_with_context("(defun apply (f x) (f x))", &mut interpreter) {
            if let Ok(val) = evaluate_with_context("(apply (lambda (x) (* x x)) 5)", &mut interpreter) {
                match val.deref() {
                    &LispValue::Int(v) => assert_eq!(v, 25),
                    e => panic!("Unexpected: {}", e)
                }
            } else {
                panic!("Unexpected error");
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[test]
    fn test_improper_parameter_list() {
        let mut interpreter = Interpreter::new();
        if let Ok(_) = evaluate_with_context("(defun test-thing (first . rest) rest)", &mut interpreter) {
            if let Ok(val) = evaluate_with_context("(test-thing 1 2)", &mut interpreter) {
                match val.deref() {
                    &LispValue::Cons(ref left, ref right) => match (left.deref(), right.deref()) {
                        (&LispValue::Int(a), &LispValue::Nil) => assert_eq!(a, 2),
                        e => panic!("Unexpected: {:?}", e)
                    },
                    e => panic!("Unexpected: {}", e)
                }
            } else {
                panic!("Unexpected error");
            }
        } else {
            panic!("Unexpected error")
        }
    }



    #[bench]
    fn bench_fibonacci(b: &mut Bencher) {
        let mut interpreter = Interpreter::new();
        if let Ok(_) = evaluate_with_context("(defun fib (n) (if (or (= n 0) (= n 1)) 1 (+ (fib (- n 1)) (fib (- n 2)))))", &mut interpreter) {
            b.iter(|| {
                if let Ok(_) = evaluate_with_context("(fib 4)", &mut interpreter) {
                    ()
                } else {
                    panic!("Unexpected error");
                }
            })
        } else {
            panic!("Unexpected error")
        }
    }

    #[bench]
    fn bench_addition(b: &mut Bencher) {
        b.iter(|| {
            evaluate("(+ 1 2)")
        })
    }

    #[bench]
    fn bench_factorial(b: &mut Bencher) {
        let mut interpreter = Interpreter::new();
        if let Ok(_) = evaluate_with_context("(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))", &mut interpreter) {
            b.iter(|| {
                if let Ok(_) = evaluate_with_context("(fact 5)", &mut interpreter) {
                    ()
                } else {
                    panic!("Unexpected error");
                }
            })
        } else {
            panic!("Unexpected error")
        }
    }

    #[bench]
    fn lisp_first_25_squares(b: &mut Bencher) {
        let mut interpreter = Interpreter::new();
        if let Ok(_) = evaluate_with_context("(defun map (f list) (if (= list '()) '() (cons (f (car list)) (map f (cdr list)))))", &mut interpreter) {
            if let Ok(_) = evaluate_with_context("(defun reduce (f z l) (if (= l '()) z (f (car l) (reduce f z (cdr l)))))", &mut interpreter) {
                b.iter(|| {
                    if let Ok(_) = evaluate_with_context("(reduce + 0 (map (lambda (x) (* x x)) '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25)))", &mut interpreter) {
                        ()
                    } else {
                        panic!("Unexpected error");
                    }
                })
            }
        } else {
            panic!("Unexpected error")
        }
    }

    #[bench]
    fn rust_first_25_squares(b: &mut Bencher) {
        b.iter(|| {
            let vec = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25isize];
            test::black_box(vec.iter().map(|x| *x * *x).fold(0, |a, b| a + b));
        })
    }

    #[bench]
    fn python_first_25_squares(b: &mut Bencher) {
        b.iter(|| {
            let mut c = Command::new("python");
            c.arg("-c").arg("print reduce(lambda x, y: x + y, map(lambda x: x * x, range(1, 25)))");
            c
        })
    }

    #[bench]
    fn ruby_first_25_squares(b: &mut Bencher) {
        b.iter(|| {
            let mut c = Command::new("ruby");
            c.arg("'e").arg("puts (1..25).to_a.map { |x| x * x } .reduce(:+)");
            c
        })
    }
}
