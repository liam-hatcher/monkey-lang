use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Object;

pub struct Environment {
    store: RefCell<HashMap<String, Box<dyn Object>>>,
}

impl Environment {
    pub fn new() -> Self {
        Self {
            store: RefCell::new(HashMap::new()),
        }
    }

    pub fn get(&self, key: &str) -> Option<Box<dyn Object>> {
        self.store.borrow().get(key).map(|obj| obj.clone_box())
    }

    pub fn set(&self, key: String, value: Box<dyn Object>) {
        self.store.borrow_mut().insert(key, value);
    }

    pub fn size(&self) -> usize {
        self.store.borrow().len()
    }
}

pub type SharedEnvironment = Rc<RefCell<Environment>>;
