use std::fmt::{Formatter, FormatError, Show};
use std::rc::Rc;
use std::collections::HashMap;
use std::collections::hashmap::{Vacant, Occupied};

use reader;
use intrinsics;

// The LispValue enum is the type of all Lisp values at runtime. These are
// the same as the S-expression representation, except that functions can also
// be values. LispValues are always used as a reference counted pointer.
#[deriving(PartialEq)]
pub enum LispValue {
    Int(i32),
    Float(f32),
    Str(String),
    Bool(bool),
    Symbol(String),
    Func(Function),
    Cons(Rc<LispValue>, Rc<LispValue>),
    Nil
}

impl Show for LispValue {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FormatError> {
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
            Int(v) => v.to_string(),
            Float(v) => v.to_string(),
            Str(ref v) => format!("\"{}\"", v),
            Symbol(ref v) => format!("{}", v),
            Cons(ref car, ref cdr) => {
                let (s_car, s_cdr) = self.print_cons(&**car, &**cdr);
                format!("({} {})", s_car, s_cdr)
            },
            Nil => "nil".to_string(),
            Bool(v) => if v {
                "#t".to_string()
            } else {
                "#f".to_string()
            },
            Func(ref c) => format!("{}", c)
        }
    }

    fn print_cons(&self, car: &LispValue, cdr: &LispValue) -> (String, String) {
        let car_str = car.pretty_print();
        let cdr_str = match *cdr {
            Cons(ref c_car, ref c_cdr) => {
                let (s_car, s_cdr) = self.print_cons(&**c_car, &**c_cdr);
                if s_cdr.len() == 0 {
                    format!("{}", s_car)
                } else {
                    format!("{} {}", s_car, s_cdr)
                }
            }
            Nil => "".to_string(),
            _ => format!(". {}", cdr.pretty_print())
        };
        (car_str, cdr_str)
    }
}

// Functions can be one of three things - an internal function (defined by defun or
// lambda), an external Rust function exposed to the interpreter, or a macro.
// Macros aren't supported yet.
#[allow(dead_code)] // macros aren't used yet
pub enum Function {
    InternalFunction(Rc<reader::Sexp>, Rc<reader::Sexp>),
    ExternalFunction(String, fn(Vec<Rc<LispValue>>) -> EvalResult),
    Macro(Rc<reader::Sexp>, Rc<reader::Sexp>),
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

impl Show for Function {
    fn fmt (&self, fmt: &mut Formatter) -> Result<(), FormatError> {
        match *self {
            InternalFunction(_, _) => write!(fmt, "<internal function>"),
            ExternalFunction(_, _) => write!(fmt, "<external function>"),
            Macro(_, _) => write!(fmt, "<macro>")
        }
    }
}

pub type EvalResult = Result<Rc<LispValue>, String>;

pub struct Interpreter {
    environment: HashMap<String, Rc<LispValue>>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        let mut interpreter = Interpreter {
            environment: HashMap::new()
        };
        interpreter.load_intrinsics();
        interpreter
    }

    fn load_intrinsics(&mut self) {
        self.expose_external_function("+".to_string(), intrinsics::add);
        self.expose_external_function("-".to_string(), intrinsics::sub);
        self.expose_external_function("*".to_string(), intrinsics::mul);
        self.expose_external_function("car".to_string(), intrinsics::car);
        self.expose_external_function("cdr".to_string(), intrinsics::cdr);
        self.expose_external_function("=".to_string(), intrinsics::eq);
    }

    pub fn eval(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            reader::Int(i) => Ok(Rc::new(Int(i))),
            reader::Float(f) => Ok(Rc::new(Float(f))),
            reader::Str(ref s) => Ok(Rc::new(Str(s.clone()))),
            reader::Symbol(ref s) => self.eval_symbol(s.clone()),
            reader::Boolean(b) => Ok(Rc::new(Bool(b))),
            reader::Cons(box reader::Symbol(ref s), ref right) if self.is_intrinsic(s) => self.eval_intrinsic(s, &**right),
            reader::Cons(ref left, ref right) => self.eval_function(&**left, &**right),
            reader::Nil => Ok(Rc::new(Nil))
        }
    }

    pub fn expose_external_function(&mut self, name: String, func: fn(Vec<Rc<LispValue>>) -> EvalResult) {
        let wrapped_func = Func(ExternalFunction(name.clone(), func));
        self.environment.insert(name.clone(), Rc::new(wrapped_func));
    }

    fn is_intrinsic(&self, name: &String) -> bool {
        match name.as_slice() {
            "if"
                | "defun"
                | "defmacro"
                | "lambda"
                | "define"
                | "quote"
                | "unquote"
                | "quasiquote" => true,
            _ => false
        }
    }

    fn eval_intrinsic(&mut self, name: &String, sexp: &reader::Sexp) -> EvalResult {
        match name.as_slice() {
            "quote" => self.eval_quote(sexp),
            "unquote" => Err("unquote not valid outside of quasiquote form".to_string()),
            "lambda" => self.eval_lambda(sexp),
            "if" => self.eval_if(sexp),
            "define" => self.eval_define(sexp),
            "quasiquote" => self.eval_quasiquote(sexp),
            "defun" => self.eval_defun(sexp),
            "defmacro" => Err("not supported yet".to_string()),
            _ => unreachable!()
        }
    }

    fn eval_quote(&mut self, sexp: &reader::Sexp) -> EvalResult {
        fn sexp_to_lvalue(s: &reader::Sexp) -> Rc<LispValue> {
            match *s {
                reader::Int(i) => Rc::new(Int(i)),
                reader::Float(i) => Rc::new(Float(i)),
                reader::Str(ref s) => Rc::new(Str(s.clone())),
                reader::Symbol(ref s) => Rc::new(Symbol(s.clone())),
                reader::Boolean(b) => Rc::new(Bool(b)),
                reader::Cons(ref car, ref cdr) => Rc::new(Cons(sexp_to_lvalue(&**car), sexp_to_lvalue(&**cdr))),
                reader::Nil => Rc::new(Nil)
            }
        }
        Ok(sexp_to_lvalue(sexp))
    }

    #[allow(unused_variable)]
    fn eval_lambda(&mut self, sexp: &reader::Sexp) -> EvalResult {
        Err("not implemented".to_string())
    }

    fn eval_if(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            reader::Cons(ref condition,
                         box reader::Cons(ref true_branch,
                                          box reader::Cons(ref false_branch,
                                                           box reader::Nil))) => {
                let cond = try!(self.eval(&**condition));
                if let &Bool(false) = cond.deref() {
                    self.eval(&**false_branch)
                } else {
                    self.eval(&**true_branch)
                }
            }
            reader::Cons(ref condition,
                         box reader::Cons(ref true_branch,
                                          box reader::Nil)) => {
                let cond = try!(self.eval(&**condition));
                if let &Bool(false) = cond.deref() {
                    Ok(Rc::new(Nil))
                } else {
                    self.eval(&**true_branch)
                }
            }
            _ => Err("Invalid pattern for if form".to_string())
        }
    }

    fn eval_define(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            reader::Cons(box reader::Symbol(ref sym), box reader::Cons(ref exp, box reader::Nil)) => {
                let value = try!(self.eval(&**exp));
                self.env_put(sym.clone(), value);
                Ok(Rc::new(Nil))
            }
            reader::Cons(box reader::Symbol(ref sym), ref exp) => {
                let value = try!(self.eval(&**exp));
                self.env_put(sym.clone(), value);
                Ok(Rc::new(Nil))
            }
            reader::Cons(ref other, _) => Err(format!("Not a symbol: {}", other)),
            _ => Err(format!("No arguments to define form"))
        }
    }

    #[allow(unused_variable)]
    fn eval_quasiquote(&mut self, sexp: &reader::Sexp) -> EvalResult {
        Err("not implemented".to_string())
    }

    fn eval_defun(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            reader::Cons(box reader::Symbol(ref sym), box reader::Cons(ref parameters, box reader::Cons(ref body, box reader::Nil))) => {
                let func = Func(InternalFunction(Rc::new(*parameters.clone()), Rc::new(*body.clone())));
                self.env_put(sym.clone(), Rc::new(func));
                Ok(Rc::new(Nil))
            }
            reader::Cons(ref other, _) => Err(format!("Not a symbol: {}", other)),
            _ => Err("No arguments to defun form".to_string())
        }
    }

    fn eval_symbol(&mut self, sym: String) -> EvalResult {
        match self.env_get(sym.clone()) {
            Some(value) => Ok(value),
            None => Err(format!("Unbound symbol: {}", sym))
        }
    }

    fn eval_function(&mut self, car: &reader::Sexp, cdr: &reader::Sexp) -> EvalResult {
        let sym_val = try!(self.eval(car));
        match *sym_val {
            Func(ref f) => match *f {
                InternalFunction(ref parameters, ref body) => self.eval_internal_function(cdr, parameters.clone(), body.clone()),
                ExternalFunction(_, func) => self.eval_external_function(cdr, func),
                Macro(ref parameters, ref body) => self.eval_macro(cdr, parameters.clone(), body.clone())
            },
            _ => Err(format!("Value is not callable: {}", sym_val))
        }
    }

    fn eval_internal_function(&mut self,
                              actual_params: &reader::Sexp,
                              formal_params: Rc<reader::Sexp>,
                              body: Rc<reader::Sexp>) -> EvalResult {
        let params = try!(self.eval_list_as_parameters(actual_params));
        let list = self.eval_list_as_parameter_list(formal_params.deref());
        if params.len() != list.len() {
            return Err("Incorrect number of parameters".to_string());
        }
        for (value, binding) in params.iter().zip(list.iter()) {
            self.env_put(binding.clone(), value.clone());
        }
        let result = self.eval(body.deref());
        for binding in list.iter() {
            self.env_delete(binding);
        }
        result
    }

    fn eval_external_function(&mut self,
                              actual_params: &reader::Sexp,
                              func: fn(Vec<Rc<LispValue>>) -> EvalResult) -> EvalResult {
        match self.eval_list_as_parameters(actual_params) {
            Ok(v) => func(v),
            Err(e) => Err(e)
        }
    }

    #[allow(unused_variable)]
    fn eval_macro(&mut self,
                  actual_params: &reader::Sexp,
                  formal_params: Rc<reader::Sexp>,
                  body: Rc<reader::Sexp>) -> EvalResult {
        Err("not implemented".to_string())
    }
                  

    // This function traverses the Cons linked list and collapses it into a vector by
    // evaluating everything in the list. Any errors are propegated to the caller.
    // This is used when evaluating parameters for a function call.
    fn eval_list_as_parameters(&mut self, params: &reader::Sexp) -> Result<Vec<Rc<LispValue>>, String> {
        match *params {
            reader::Cons(ref car, ref cdr) => {
                let mut out_vec = vec![];
                match self.eval(&**car) {
                    Ok(v) => out_vec.push(v),
                    Err(e) => return Err(e)
                };
                match self.eval_list_as_parameters(&**cdr) {
                    Ok(vec) => {
                        out_vec.extend(vec.into_iter());
                        Ok(out_vec)
                    }
                    Err(e) => Err(e)
                }
            }
            reader::Nil => Ok(vec![]),
            _ => Err("Cannot use an improper list as parameters to a function".to_string())
        }
    }

    // The function is similar to eval_list_as_parameters, but it just gets the names
    // of all of the symbols in the linked list instead of evaluating them. This
    // is also used for evaluating parameters for a function call.
    fn eval_list_as_parameter_list(&mut self, params: &reader::Sexp) -> Vec<String> {
        match *params {
            reader::Cons(box reader::Symbol(ref s), ref cdr) => {
                let mut result = vec![s.clone()];
                result.extend(self.eval_list_as_parameter_list(&**cdr).into_iter());
                result
            },
            reader::Nil => vec![],
            _ => unreachable!()
        }
    }

    pub fn env_get(&mut self, key: String) -> Option<Rc<LispValue>> {
        match self.environment.entry(key.clone()) {
            Vacant(_) => None,
            Occupied(entry) => Some(entry.get().clone())
        }
    }

    pub fn env_put(&mut self, key: String, value: Rc<LispValue>) {
        self.environment.insert(key, value);
    }

    pub fn env_delete(&mut self, key: &String) {
        self.environment.remove(key);
    }
}

#[cfg(test)]
mod test {
    use std::rc::Rc;
    use super::*;
    use super::super::reader;

    fn evaluate(input: &'static str) -> EvalResult {
        let mut interpreter = Interpreter::new();
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
                &Int(x) => assert_eq!(x, 3),
                e => fail!("Unexpected: {}", e)
            }
        } else {
            fail!("Unexpected error")
        }
    }

    #[test]
    fn test_varargs_addition() {
        if let Ok(val) = evaluate("(+ 5 5 5 5 5)") {
            match val.deref() {
                &Int(x) => assert_eq!(x, 25),
                e => fail!("Unexpected: {}", e)
            }
        } else {
            fail!("Unexpected error")
        }        
    }

    #[test]
    fn test_subtraction() {
        if let Ok(val) = evaluate("(- 1 2)") {
            match val.deref() {
                &Int(x) => assert_eq!(x, -1),
                e => fail!("Unexpected: {}", e)
            }
        } else {
            fail!("Unexpected error")
        }
    }

    #[test]
    fn test_varargs_subtraction() {
        if let Ok(val) = evaluate("(- 5 1 1 1)") {
            match val.deref() {
                &Int(x) => assert_eq!(x, 2),
                e => fail!("Unexpected: {}", e)
            }
        } else {
            fail!("Unexpected error")
        }
    }

    #[test]
    fn test_car() {
        if let Ok(val) = evaluate("(car '(1 2))") {
            match val.deref() {
                &Int(x) => assert_eq!(x, 1),
                e => fail!("Unexpected: {}", e)
            }
        } else {
            fail!("Unexpected error")
        }
    }

    #[test]
    fn test_cdr() {
        if let Ok(val) = evaluate("(cdr '(1 2))") {
            match val.deref() {
                &Cons(ref car, _) => match car.deref() {
                    &Int(a) => assert_eq!(a, 2),
                    _ => fail!("Unexpected: {}", car)
                },
                e => fail!("Unexpected: {}", e)
            }
        } else {
            fail!("Unexpected error")
        }
    }

    /*
    #[bench]
    fn factorial_bench(b: &mut Bencher) {
        let mut interpreter = Interpreter::new();
        let mut reader = reader::SexpReader::new();
        match reader.parse_str("(defun fact (n) (if (= n 0) 1 (* n (fact (- n 1)))))") {
            Ok(e) => match interpreter.eval(&e) {
                Ok(val) => (),
                Err(e) => fail!("{}", e)
            },
            Err(e) => fail!("{}", e)
        };
        b.iter(|| {
            match reader.parse_str("(fact 15)") {
                Ok(e) => match interpreter.eval(&e) {
                    Ok(val) => assert_eq!(val, Int(130767436800)),
                    Err(e) => fail!("{}", e)
                },
                Err(e) => fail("{}", e)
            }
        })
    }*/
    
}
