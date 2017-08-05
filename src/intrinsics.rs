use std::rc::Rc;
use std::ops::Deref;

use interpreter::{LispValue, EvalResult};

// add function - exposed as (+) to Lisp
pub fn add(params: Vec<Rc<LispValue>>) -> EvalResult {
    params.into_iter()
        .fold(Ok(LispValue::Int(0)), |a, b| {
            match a {
                Ok(acc) => match (acc, b.deref()) {
                    (LispValue::Int(r), &LispValue::Int(a)) => Ok(LispValue::Int(r + a)),
                    (LispValue::Int(r), &LispValue::Float(a)) => Ok(LispValue::Float((r as f64) + a)),
                    (LispValue::Float(r), &LispValue::Int(a)) => Ok(LispValue::Float(r + (a as f64))),
                    (LispValue::Float(r), &LispValue::Float(a)) => Ok(LispValue::Float(r + a)),
                    (_, ref rb) => Err(format!("Wrong type: {}", rb))
                },
                Err(e) => Err(e)
            }
        })
        .map(|e| Rc::new(e))
}

// subtraction function - exposed as (-) to Lisp
pub fn sub(params: Vec<Rc<LispValue>>) -> EvalResult {
    if params.len() == 0 {
        return Err("Incorrect number of parameters".to_string());
    }
    let initial = match params[0].deref() {
        &LispValue::Int(r) => LispValue::Int(r),
        &LispValue::Float(r) => LispValue::Float(r),
        e => return Err(format!("Wrong type: {}", e))
    };
    params.into_iter()
        .skip(1)
        .fold(Ok(initial), |a, b| {
            match a {
                Ok(acc) => match (acc, b.deref()) {
                    (LispValue::Int(r), &LispValue::Int(a)) => Ok(LispValue::Int(r - a)),
                    (LispValue::Int(r), &LispValue::Float(a)) => Ok(LispValue::Float((r as f64) - a)),
                    (LispValue::Float(r), &LispValue::Int(a)) => Ok(LispValue::Float(r - (a as f64))),
                    (LispValue::Float(r), &LispValue::Float(a)) => Ok(LispValue::Float(r - a)),
                    (_, ref rb) => Err(format!("Wrong type: {}", rb))
                },
                Err(e) => Err(e)
            }
        })
        .map(|e| Rc::new(e))
}

// multiplication function - exposed as (*) to Lisp
pub fn mul(params: Vec<Rc<LispValue>>) -> EvalResult {
    params.into_iter()
        .fold(Ok(LispValue::Int(1)), |a, b| {
            match a {
                Ok(acc) => match (acc, b.deref()) {
                    (LispValue::Int(r), &LispValue::Int(a)) => Ok(LispValue::Int(r * a)),
                    (LispValue::Int(r), &LispValue::Float(a)) => Ok(LispValue::Float((r as f64) * a)),
                    (LispValue::Float(r), &LispValue::Int(a)) => Ok(LispValue::Float(r * (a as f64))),
                    (LispValue::Float(r), &LispValue::Float(a)) => Ok(LispValue::Float(r * a)),
                    (_, ref rb) => Err(format!("Wrong type: {}", rb))
                },
                Err(e) => Err(e)
            }
        })
        .map(|e| Rc::new(e))
}

// division function - exposed as (/) to Lisp
pub fn div(params: Vec<Rc<LispValue>>) -> EvalResult {
    if params.len() == 0 {
        return Err("Incorrect number of parameters".to_string());
    }
    let initial = match params[0].deref() {
        &LispValue::Int(r) => LispValue::Int(r),
        &LispValue::Float(r) => LispValue::Float(r),
        e => return Err(format!("Wrong type: {}", e))
    };
    params.into_iter()
        .skip(1)
        .fold(Ok(initial), |a, b| {
            match a {
                Ok(acc) => match (acc, b.deref()) {
                    (LispValue::Int(r), &LispValue::Int(a)) => if a == 0 { Err(format!("Division by zero")) } else { Ok(LispValue::Float(r as f64 / a as f64)) },
                    (LispValue::Int(r), &LispValue::Float(a)) => if a == 0.0 { Err(format!("Division by zero")) } else { Ok(LispValue::Float((r as f64) / a)) },
                    (LispValue::Float(r), &LispValue::Int(a)) => if a == 0 { Err(format!("Division by zero")) } else { Ok(LispValue::Float(r / (a as f64))) },
                    (LispValue::Float(r), &LispValue::Float(a)) => if a == 0.0 { Err(format!("Division by zero")) } else { Ok(LispValue::Float(r / a)) },
                    (_, ref rb) => Err(format!("Wrong type: {}", rb))
                },
                Err(e) => Err(e)
            }
        })
        .map(|e| Rc::new(e))
}

// car function - exposed as (car) to Lisp
pub fn car(params: Vec<Rc<LispValue>>) -> EvalResult {
    if params.len() != 1 {
        return Err("Incorrect number of parameters".to_string())
    }
    match params[0].deref() {
        &LispValue::Cons(ref car, _) => Ok(car.clone()),
        &LispValue::Nil => Err("Cannot take the car of an empty list".to_string()),
        _ => Err("Cannot take the car of a non-list".to_string())
    }
}

// cdr function - exposed as (cdr) to Lisp
pub fn cdr(params: Vec<Rc<LispValue>>) -> EvalResult {
    if params.len() != 1 {
        return Err("Incorrect number of parameters".to_string())
    }
    match params[0].deref() {
        &LispValue::Cons(_, ref cdr) => Ok(cdr.clone()),
        &LispValue::Nil => Err("Cannot take the cdr of an empty list".to_string()),
        _ => Err("Cannot take the cdr of a non-list".to_string())
    }
}

pub fn cons(params: Vec<Rc<LispValue>>) -> EvalResult {
    if params.len() != 2 {
        return Err("Incorrect number of parameters".to_string())
    }
    Ok(Rc::new(LispValue::Cons(params[0].clone(), params[1].clone())))
}

// eq function - exposed as (=) to Lisp.
pub fn eq(params: Vec<Rc<LispValue>>) -> EvalResult {
    if params.len() == 0 {
        return Ok(Rc::new(LispValue::Bool(true)));
    }
    let first = params[0].clone();
    let res = params.into_iter()
        .fold(true, |a, b| {
            a && b == first
        });
    Ok(Rc::new(LispValue::Bool(res)))
}

pub fn display(params: Vec<Rc<LispValue>>) -> EvalResult {
    for ref value in params.iter() {
        println!("{}", value);
    }
    Ok(Rc::new(LispValue::Nil))
}

// pair function - exposed as (pair?) to Lisp.
pub fn pair(params: Vec<Rc<LispValue>>) -> EvalResult {
    if params.len() != 1 {
        return Err("Incorrect number of parameters".to_string())
    }
    if let &LispValue::Cons(_, _) = params[0].deref() {
        Ok(Rc::new(LispValue::Bool(true)))
    } else {
        Ok(Rc::new(LispValue::Bool(false)))
    }
}
