use std::fmt::{Formatter, FormatError, Show};
use std::rc::Rc;
use std::collections::HashMap;
use std::collections::hashmap::{Vacant, Occupied};

use reader;


#[deriving(Clone)]
pub enum LispValue {
    Int(i32),
    Float(f32),
    Str(Rc<String>),
    Bool(bool),
    Symbol(Rc<String>),
    Func(Rc<Function>),
    Cons(Rc<LispValue>, Rc<LispValue>),
    Nil
}

impl Show for LispValue {
    fn fmt(&self, fmt: &mut Formatter) -> Result<(), FormatError> {
        write!(fmt, "{}", self.pretty_print())
    }
}

impl LispValue {
    #[allow(dead_code)]
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

    #[allow(dead_code)]
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

pub enum Function {
    InternalFunction(uint, Rc<reader::Sexp>, Rc<reader::Sexp>),
    ExternalFunction(String, fn(Vec<LispValue>) -> EvalResult),
    Macro(uint, Rc<reader::Sexp>, Rc<reader::Sexp>),
}

impl Show for Function {
    fn fmt (&self, fmt: &mut Formatter) -> Result<(), FormatError> {
        match *self {
            InternalFunction(_, _, _) => write!(fmt, "<internal function>"),
            ExternalFunction(_, _) => write!(fmt, "<external function>"),
            Macro(_, _, _) => write!(fmt, "<macro>")
        }
    }
}

pub type EvalResult = Result<Rc<LispValue>, String>;

pub struct Interpreter {
    environment: HashMap<String, LispValue>
}

impl Interpreter {
    pub fn new() -> Interpreter {
        Interpreter {
            environment: HashMap::new()
        }
    }

    pub fn eval(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            reader::Int(i) => Ok(Int(i)),
            reader::Float(f) => Ok(Float(f)),
            reader::Str(ref s) => Ok(Str(Rc::new(s.clone()))),
            reader::Symbol(ref s) => self.eval_symbol(Rc::new(s.clone())),
            reader::Boolean(b) => Ok(Bool(b)),
            reader::Cons(box reader::Symbol(ref s), ref right) if self.is_intrinsic(s) => self.eval_intrinsic(s, &**right),
            reader::Cons(ref left, ref right) => self.eval_function(&**left, &**right),
            reader::Nil => Ok(Nil)
        }
    }

    pub fn expose_external_function(&mut self, name: String, func: fn(Vec<LispValue>) -> EvalResult) {
        let wrapped_func = Func(Rc::new(ExternalFunction(name.clone(), func)));
        self.environment.insert(name.clone(), wrapped_func);
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
        fn sexp_to_lvalue(s: &reader::Sexp) -> LispValue {
            match *s {
                reader::Int(i) => Int(i),
                reader::Float(i) => Float(i),
                reader::Str(ref s) => Str(Rc::new(s.clone())),
                reader::Symbol(ref s) => Symbol(Rc::new(s.clone())),
                reader::Boolean(b) => Bool(b),
                reader::Cons(ref car, ref cdr) => Cons(Rc::new(sexp_to_lvalue(&**car)), Rc::new(sexp_to_lvalue(&**cdr))),
                reader::Nil => Nil
            }
        }
        Ok(sexp_to_lvalue(sexp))
    }

    fn eval_lambda(&mut self, sexp: &reader::Sexp) -> EvalResult {
        Ok(Int(42))
    }

    fn eval_if(&mut self, sexp: &reader::Sexp) -> EvalResult {
        Ok(Int(42))
    }

    fn eval_define(&mut self, sexp: &reader::Sexp) -> EvalResult {
        match *sexp {
            reader::Cons(box reader::Symbol(ref sym), box reader::Cons(ref exp, box reader::Nil)) => {
                let value = try!(self.eval(&**exp));
                self.env_put(sym.clone(), value);
                Ok(Nil)
            }
            reader::Cons(box reader::Symbol(ref sym), ref exp) => {
                let value = try!(self.eval(&**exp));
                self.env_put(sym.clone(), value);
                Ok(Nil)
            }
            reader::Cons(ref other, _) => Err(format!("Not a symbol: {}", other)),
            _ => Err(format!("No arguments to define form"))
        }
    }

    fn eval_quasiquote(&mut self, sexp: &reader::Sexp) -> EvalResult {
        Ok(Int(42))
    }

    fn eval_defun(&mut self, sexp: &reader::Sexp) -> EvalResult {
        Ok(Int(42))
    }

    fn eval_symbol(&mut self, sym: Rc<String>) -> EvalResult {
        match self.env_get(sym.clone()) {
            Some(value) => Ok(value),
            None => Err(format!("Unbound symbol: {}", sym))
        }
    }

    fn eval_function(&mut self, car: &reader::Sexp, cdr: &reader::Sexp) -> EvalResult {
        let sym_val = try!(self.eval(car));
        match sym_val {
            Func(f) => match *f {
                InternalFunction(arity, ref parameters, ref body) => self.eval_internal_function(cdr, arity, parameters.clone(), body.clone()),
                ExternalFunction(ref name, func) => self.eval_external_function(cdr, name.clone(), func),
                Macro(arity, ref parameters, ref body) => self.eval_macro(cdr, arity, parameters.clone(), body.clone())
            },
            _ => Err(format!("Value is not callable: {}", sym_val))
        }
    }

    fn eval_internal_function(&mut self,
                              actual_params: &reader::Sexp,
                              arity: uint,
                              formal_params: Rc<reader::Sexp>,
                              body: Rc<reader::Sexp>) -> EvalResult {
        Ok(Int(42))
    }

    fn eval_external_function(&mut self,
                              actual_params: &reader::Sexp,
                              name: String,
                              func: fn(Vec<LispValue>) -> EvalResult) -> EvalResult {
        match self.eval_list_as_parameters(actual_params) {
            Ok(v) => func(v),
            Err(e) => Err(e)
        }
    }

    fn eval_macro(&mut self,
                  actual_params: &reader::Sexp,
                  arity: uint,
                  formal_params: Rc<reader::Sexp>,
                  body: Rc<reader::Sexp>) -> EvalResult {
        Ok(Int(42))
    }
                  

    fn eval_list_as_parameters(&mut self, params: &reader::Sexp) -> Result<Vec<LispValue>, String> {
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

    pub fn env_get(&mut self, key: Rc<String>) -> Option<LispValue> {
        let ref value = key;
        match self.environment.entry((**value).clone()) {
            Vacant(_) => None,
            Occupied(entry) => Some(entry.get().clone())
        }
    }

    pub fn env_put(&mut self, key: String, value: LispValue) {
        self.environment.insert(key, value);
    }
}
