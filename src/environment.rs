use std::rc::Rc;
use std::collections::HashMap;

use interpreter;

pub struct Environment {
    stack: Vec<StackFrame>
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            stack: vec![StackFrame::new()]
        }
    }

    pub fn enter_scope(&mut self) {
        self.stack.push(StackFrame::new());
    }

    pub fn exit_scope(&mut self) {
        self.stack.pop().expect("Popped from an empty stack");
    }

    pub fn get(&mut self, key: String) -> Option<Rc<interpreter::LispValue>> {
        for ref stack_frame in self.stack.iter() {
            let lookup = stack_frame.get(&key);
            if lookup.is_some() {
                return lookup;
            }
        }
        None
    }

    pub fn put(&mut self, key: String, value: Rc<interpreter::LispValue>) {
        let top_of_stack = self.stack.get_mut(0);
        top_of_stack.put(key, value);
    }
}

struct StackFrame {
    frame: HashMap<String, Rc<interpreter::LispValue>>,
}

impl StackFrame {
    pub fn new() -> StackFrame {
        StackFrame {
            frame: HashMap::new(),
        }
    }

    pub fn get(&self, key: &String) -> Option<Rc<interpreter::LispValue>> {
        self.frame.find_copy(key)
    }

    pub fn put(&mut self, key: String, value: Rc<interpreter::LispValue>) {
        self.frame.insert(key, value);
    }
}
