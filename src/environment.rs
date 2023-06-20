use std::collections::HashMap;

use crate::evaluator::Object;

#[derive(Clone, PartialEq, Debug)]
pub struct Environment {
    variables: HashMap<String, Object>,
}

impl Environment {
    pub fn new() -> Self {
        Environment {
            variables: HashMap::new(),
        }
    }

    pub fn set(&mut self, ident: String, value: Object) {
        self.variables.insert(ident, value);
    }

    pub fn get(&mut self, ident: &str) -> Option<Object> {
        self.variables.get(ident).map(|v| v.to_owned())
    }
}
