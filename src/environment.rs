use std::rc::Rc;
use std::collections::HashMap;

use interpreter;

pub struct Environment {
    stack: Vec<HashMap<String, Rc<interpreter::LispValue>>>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            stack: vec![HashMap::new()]
        }
    }

    pub fn enter_scope(&mut self) {
        self.stack.push(HashMap::new())
    }

    pub fn exit_scope(&mut self) {
        self.stack.pop().expect("Popping from empty stack");
    }

    pub fn get(&mut self, key: String) -> Option<Rc<interpreter::LispValue>> {
        self.stack[self.stack.len() - 1].find_copy(&key)
            .or(self.stack[0].find_copy(&key))
    }

    pub fn put(&mut self, key: String, value: Rc<interpreter::LispValue>) {
        let len = self.stack.len();
        self.stack.get_mut(len - 1).insert(key, value);
    }

    pub fn put_global(&mut self, key: String, value: Rc<interpreter::LispValue>) {
        self.stack.get_mut(0).insert(key, value);
    }
}
