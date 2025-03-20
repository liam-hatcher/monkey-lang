#[derive(PartialEq, Debug)]
pub enum ObjectType {
    Integer,
    Boolean,
    Return,
    Null,
}

#[derive(Debug, PartialEq)]
pub enum ObjectValue {
    Int(i64),
    Bool(bool),
    None,
    Null,
}

pub trait Object {
    fn inspect(&self) -> String;
    fn kind(&self) -> ObjectType;
    fn get_value(&self) -> ObjectValue {
        ObjectValue::None
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
            _ => ObjectValue::None
        }
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
