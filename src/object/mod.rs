#[derive(PartialEq, Debug)]
pub enum ObjectType {
    Integer,
    Boolean,
    Return,
    Error,
    Null,
}

#[derive(Debug, PartialEq)]
pub enum ObjectValue {
    Int(i64),
    Bool(bool),
    None,
    Error(String),
    Null,
}

trait CloneBox {
    fn clone_box(&self) -> Box<dyn Object>;
}

impl<T: 'static + Object + Clone> CloneBox for T {
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(self.clone())
    }
}

impl Clone for Box<dyn Object> {
    fn clone(&self) -> Self {
        self.clone_box()
    }
}

pub trait Object: CloneBox {
    fn inspect(&self) -> String;
    fn kind(&self) -> ObjectType;
    fn get_value(&self) -> ObjectValue {
        ObjectValue::None
    }
}

pub struct Error {
    pub message: String,
}

impl Object for Error {
    fn inspect(&self) -> String {
        format!("ERROR: {}", self.message)
    }

    fn kind(&self) -> ObjectType {
        ObjectType::Error
    }
    fn get_value(&self) -> ObjectValue {
        ObjectValue::Error(self.message.clone())
    }
}

impl CloneBox for Error {
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Self {
            message: self.message.clone(),
        })
    }
}

pub struct Return {
    pub value: Box<dyn Object>,
}

impl Object for Return {
    fn inspect(&self) -> String {
        format!("{:?}", self.value.get_value())
    }
    fn kind(&self) -> ObjectType {
        ObjectType::Return
    }
    fn get_value(&self) -> ObjectValue {
        let value = self.value.get_value();

        match value {
            ObjectValue::Int(i) => ObjectValue::Int(i),
            ObjectValue::Bool(b) => ObjectValue::Bool(b),
            ObjectValue::Null => ObjectValue::Null,
            _ => ObjectValue::None,
        }
    }
}

impl CloneBox for Return {
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Self {
            value: self.value.clone_box(),
        })
    }
}

pub struct Integer {
    pub value: i64,
}

impl Object for Integer {
    fn inspect(&self) -> String {
        format!("{}", self.value)
    }
    fn kind(&self) -> ObjectType {
        ObjectType::Integer
    }
    fn get_value(&self) -> ObjectValue {
        ObjectValue::Int(self.value)
    }
}

impl CloneBox for Integer {
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Self {
            value: self.value.clone(),
        })
    }
}

pub struct Boolean {
    pub value: bool,
}

impl Object for Boolean {
    fn inspect(&self) -> String {
        match self.value {
            true => "true".into(),
            false => "false".into(),
        }
    }
    fn kind(&self) -> ObjectType {
        ObjectType::Boolean
    }
    fn get_value(&self) -> ObjectValue {
        ObjectValue::Bool(self.value)
    }
}

impl CloneBox for Boolean {
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Self {
            value: self.value.clone(),
        })
    }
}

pub struct Null;

impl Object for Null {
    fn inspect(&self) -> String {
        "null".into()
    }
    fn kind(&self) -> ObjectType {
        ObjectType::Null
    }
    fn get_value(&self) -> ObjectValue {
        ObjectValue::Null
    }
}

impl CloneBox for Null {
    fn clone_box(&self) -> Box<dyn Object> {
        Box::new(Self)
    }
}

pub mod environment;
