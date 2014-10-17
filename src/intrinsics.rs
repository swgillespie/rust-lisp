use interpreter::{LispValue, EvalResult, Int, Float, Cons};


/*
pub fn add(params: Vec<LispValue>) -> EvalResult {
    match params.len() {
        0 => return Ok(Int(0)),
        1 => return Ok(params[0].clone()),
        _ => ()
    };
    let rest = try!(add(params
                        .iter()
                        .skip(1)
                        .map(|x| x.clone())
                        .collect()));
    match (rest, params[0].clone()) {
        (Int(r), Int(a)) => Ok(Int(r + a)),
        (Int(r), Float(a)) => Ok(Float((r as f32) + a)),
        (Float(r), Int(a)) => Ok(Float(r + (a as f32))),
        (Float(r), Float(a)) => Ok(Float(r + a)),
        _ => Err(format!("Wrong type: {}", params[0]))
    }
}*/

pub fn add(params: Vec<LispValue>) -> EvalResult {
    params.into_iter()
        .fold(Ok(Int(0)), |a, b| {
            match a {
                Ok(acc) => match (acc, b) {
                    (Int(r), Int(a)) => Ok(Int(r + a)),
                    (Int(r), Float(a)) => Ok(Float((r as f32) + a)),
                    (Float(r), Int(a)) => Ok(Float(r + (a as f32))),
                    (Float(r), Float(a)) => Ok(Float(r + a)),
                    (_, ref rb) => Err(format!("Wrong type: {}", rb))
                },
                Err(e) => Err(e)
            }
        })
}

pub fn sub(params: Vec<LispValue>) -> EvalResult {
    if params.len() == 0 {
        return Err("Incorrect number of parameters".to_string());
    }
    params.into_iter()
        .fold(Ok(Int(0)), |a, b| {
            match a {
                Ok(acc) => match (acc, b) {
                    (Int(r), Int(a)) => Ok(Int(r - a)),
                    (Int(r), Float(a)) => Ok(Float((r as f32) - a)),
                    (Float(r), Int(a)) => Ok(Float(r - (a as f32))),
                    (Float(r), Float(a)) => Ok(Float(r - a)),
                    (_, ref rb) => Err(format!("Wrong type: {}", rb))
                },
                Err(e) => Err(e)
            }
        })
}

pub fn mul(params: Vec<LispValue>) -> EvalResult {
    params.into_iter()
        .fold(Ok(Int(1)), |a, b| {
            match a {
                Ok(acc) => match (acc, b) {
                    (Int(r), Int(a)) => Ok(Int(r * a)),
                    (Int(r), Float(a)) => Ok(Float((r as f32) * a)),
                    (Float(r), Int(a)) => Ok(Float(r * (a as f32))),
                    (Float(r), Float(a)) => Ok(Float(r * a)),
                    (_, ref rb) => Err(format!("Wrong type: {}", rb))
                },
                Err(e) => Err(e)
            }
        })
}

