use crate::object::{Array, Error, Integer, NativeFunction, Null, Object, ObjectType, ObjectValue};

fn len(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 1 {
        return Box::new(Error {
            message: format!("len expects 1 argument, but {} were supplied", args.len()),
        });
    }

    match args[0].kind() {
        ObjectType::Str => {
            let ObjectValue::Str(value) = args[0].get_value() else {
                unreachable!();
            };

            Box::new(Integer {
                value: value.len() as i64,
            })
        }
        ObjectType::Array => {
            let ObjectValue::Array(value) = args[0].get_value() else {
                unreachable!();
            };

            Box::new(Integer {
                value: value.len() as i64,
            })
        }
        _ => Box::new(Error {
            message: format!("argument to 'len' not supported, got {:?}", args[0].kind()),
        }),
    }
}

fn first(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 1 {
        return Box::new(Error {
            message: format!("first expects 1 argument, but {} were supplied", args.len()),
        });
    }

    if args[0].kind() != ObjectType::Array {
        return Box::new(Error {
            message: format!(
                "argument to 'first' not supported, got {:?}",
                args[0].kind()
            ),
        });
    }

    let ObjectValue::Array(arr) = args[0].get_value() else {
        unreachable!()
    };

    if arr.len() > 0 {
        return arr.first().unwrap().clone();
    }

    Box::new(Null)
}

fn last(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 1 {
        return Box::new(Error {
            message: format!("last expects 1 argument, but {} were supplied", args.len()),
        });
    }

    if args[0].kind() != ObjectType::Array {
        return Box::new(Error {
            message: format!(
                "argument to 'last' not supported, got {:?}",
                args[0].kind()
            ),
        });
    }

    let ObjectValue::Array(arr) = args[0].get_value() else {
        unreachable!()
    };

    if arr.len() > 0 {
        return arr.last().unwrap().clone();
    }

    Box::new(Null)
}

fn rest(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 1 {
        return Box::new(Error {
            message: format!("rest expects 1 argument, but {} were supplied", args.len()),
        });
    }

    if args[0].kind() != ObjectType::Array {
        return Box::new(Error {
            message: format!(
                "argument to 'rest' not supported, got {:?}",
                args[0].kind()
            ),
        });
    }

    let ObjectValue::Array(arr) = args[0].get_value() else {
        unreachable!()
    };

    if arr.len() > 0 {
        Box::new(Array {
            elements: arr[1..arr.len()].to_vec()
        })
    } else {
        Box::new(Null)
    }
}

fn push(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if args.len() != 2 {
        return Box::new(Error {
            message: format!("push expects 2 arguments, but {} were supplied", args.len()),
        });
    }

    if args[0].kind() != ObjectType::Array {
        return Box::new(Error {
            message: format!(
                "argument to 'push' not supported, got {:?}",
                args[0].kind()
            ),
        });
    }

    let ObjectValue::Array(arr) = args[0].get_value() else {
        unreachable!()
    };

    let new_element = args[1].clone();
    let mut elements = arr.clone();
    elements.push(new_element);

    Box::new(Array {
        elements
    })
}

pub fn get_native_function(id: &str) -> Option<Box<dyn Object>> {
    match id {
        "len" => Some(Box::new(NativeFunction { func: len })),
        "first" => Some(Box::new(NativeFunction { func: first })),
        "last" => Some(Box::new(NativeFunction { func: last })),
        "rest" => Some(Box::new(NativeFunction { func: rest })),
        "push" => Some(Box::new(NativeFunction { func: push })),
        _ => None,
    }
}
