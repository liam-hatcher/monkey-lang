use crate::object::{Error, Integer, NativeFunction, Object, ObjectType, ObjectValue};

pub fn len(args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
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
                value: value.len() as i64
            })
        },
        _ => Box::new(Error {
            message: format!("argument to 'len' not supported, got {:?}", args[0].kind()),
        })
    }
}

pub fn get_native_function(id: &str) -> Option<Box<dyn Object>> {
    match id {
        "len" => Some(Box::new(NativeFunction { func: len })),
        _ => None,
    }
}
