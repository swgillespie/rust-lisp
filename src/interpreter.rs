use reader::Sexp;

pub enum LispValue {
    VInt(i32),
    VFloat(f32),
    VStr(String),
    VSymbol(String),
    VFunction(Function),
    VList(Box<LispValue>, Box<LispValue>),
}

pub enum Function {
    InternalFunction(u32, Vec<String>, Sexp),
    ExternalFunction(u32, fn(Vec<LispValue>, LispValue)),
    Macro(u32, Sexp, Sexp)
}
