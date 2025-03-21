use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Object;

pub type SharedEnvironment = Rc<RefCell<Environment>>;

pub struct Environment {
    store: RefCell<HashMap<String, Box<dyn Object>>>,
    outer: Option<SharedEnvironment>,
}

impl Environment {
    pub fn new() -> SharedEnvironment {
        Rc::new(RefCell::new(Self {
            store: RefCell::new(HashMap::new()),
            outer: None,
        }))
    }

    pub fn new_enclosed(outer: SharedEnvironment) -> SharedEnvironment {
        Rc::new(RefCell::new(Self {
            store: RefCell::new(HashMap::new()),
            outer: Some(outer),
        }))
    }

    pub fn get(&self, key: &str) -> Option<Box<dyn Object>> {
        let mut object = self.store.borrow().get(key).map(|obj| obj.clone_box());

        if object.is_none() && self.outer.is_some() {
            object = self
                .outer
                .as_ref()
                .unwrap()
                .borrow()
                .get(key)
                .map(|obj| obj.clone_box());
        }

        object
    }

    pub fn set(&self, key: String, value: Box<dyn Object>) {
        self.store.borrow_mut().insert(key, value);
    }

    pub fn size(&self) -> usize {
        self.store.borrow().len()
    }
}
