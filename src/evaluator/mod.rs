use std::collections::HashMap;

use native_functions::get_native_function;

use crate::{
    ast::{
        BlockStatement, Expression, HashLiteral, IdentifierExpression, IfExpression, Node,
        Statement,
    },
    object::{
        Array, Boolean, Error, Function, HashKey, HashPair, Integer, MonkeyHash, Null, Object,
        ObjectType, ObjectValue, Return, Str,
    },
};

use crate::object::environment::*;

fn is_truthy(condition: Box<dyn Object>) -> bool {
    match condition.get_value() {
        ObjectValue::Null => false,
        ObjectValue::Bool(b) => b,
        _ => true,
    }
}

fn eval_if_expression(if_expression: IfExpression, env: SharedEnvironment) -> Box<dyn Object> {
    let condition = eval(Node::Expression(*if_expression.condition), env.clone());
    if is_error(&condition) {
        return condition;
    }

    if is_truthy(condition) {
        return eval(
            Node::Statement(Statement::Block(*if_expression.consequence.unwrap())),
            env,
        );
    } else if if_expression.alternative.is_some() {
        return eval(
            Node::Statement(Statement::Block(*if_expression.alternative.unwrap())),
            env,
        );
    } else {
        return Box::new(Null);
    }
}

fn unwrap_int(node: &Box<dyn Object>) -> i64 {
    match node.get_value() {
        ObjectValue::Int(i) => i,
        _ => unreachable!("something has gone wrong"),
    }
}

fn eval_integer_infix_expression(
    operator: &str,
    left: Box<dyn Object>,
    right: Box<dyn Object>,
) -> Box<dyn Object> {
    let l = unwrap_int(&left);
    let r = unwrap_int(&right);

    match operator {
        "+" => Box::new(Integer { value: l + r }),
        "-" => Box::new(Integer { value: l - r }),
        "*" => Box::new(Integer { value: l * r }),
        "/" => Box::new(Integer { value: l / r }),
        "<" => Box::new(Boolean { value: l < r }),
        ">" => Box::new(Boolean { value: l > r }),
        "==" => Box::new(Boolean { value: l == r }),
        "!=" => Box::new(Boolean { value: l != r }),
        // "<=" => Box::new(Boolean { value: l <= r }),
        _ => Box::new(Error {
            message: format!(
                "unknown operator: {:?} {} {:?}",
                left.kind(),
                operator,
                right.kind()
            ),
        }),
    }
}

fn eval_string_infix_expression(
    operator: &str,
    left: Box<dyn Object>,
    right: Box<dyn Object>,
) -> Box<dyn Object> {
    if operator != "+" {
        return Box::new(Error {
            message: format!(
                "unknown operator: {:?} {} {:?}",
                left.kind(),
                operator,
                right.kind()
            ),
        });
    }

    let left = left.inspect();
    let right = right.inspect();

    Box::new(Str {
        value: left + &*right,
    })
}

fn eval_infix_expression(
    operator: &str,
    left: Box<dyn Object>,
    right: Box<dyn Object>,
) -> Box<dyn Object> {
    if left.kind() == ObjectType::Integer && right.kind() == ObjectType::Integer {
        return eval_integer_infix_expression(operator, left, right);
    }

    if left.kind() == ObjectType::Str && right.kind() == ObjectType::Str {
        return eval_string_infix_expression(operator, left, right);
    }

    if operator == "==" {
        return Box::new(Boolean {
            value: left.get_value() == right.get_value(),
        });
    }

    if operator == "!=" {
        return Box::new(Boolean {
            value: left.get_value() != right.get_value(),
        });
    }

    if left.kind() != right.kind() {
        return Box::new(Error {
            message: format!("type mismatch: {:?} + {:?}", left.kind(), right.kind()),
        });
    }

    Box::new(Error {
        message: format!(
            "unknown operator: {:?} {} {:?}",
            left.kind(),
            operator,
            right.kind()
        ),
    })
}

fn eval_bang_operator(right: Box<dyn Object>) -> Box<dyn Object> {
    match right.get_value() {
        ObjectValue::Bool(b) => match b {
            true => Box::new(Boolean { value: false }),
            false => Box::new(Boolean { value: true }),
        },
        ObjectValue::Null => Box::new(Boolean { value: true }),
        _ => Box::new(Boolean { value: false }),
    }
}

fn eval_identifier(id: IdentifierExpression, env: SharedEnvironment) -> Box<dyn Object> {
    if let Some(value) = env.borrow().get(&id.value) {
        return value;
    } else if let Some(native) = get_native_function(&id.value) {
        return native;
    }

    Box::new(Error {
        message: format!("identifier not found: {}", id.value),
    })
}

fn eval_minus_prefix_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    if right.kind() != ObjectType::Integer {
        Box::new(Error {
            message: format!("unknown operator: -{:?}", right.kind()),
        })
    } else {
        let ObjectValue::Int(value) = right.get_value() else {
            panic!("expected integer, got {:?}", right.get_value());
        };
        Box::new(Integer { value: -value })
    }
}

fn eval_prefix_expression(operator: &str, right: Box<dyn Object>) -> Box<dyn Object> {
    match operator {
        "!" => eval_bang_operator(right),
        "-" => eval_minus_prefix_operator_expression(right),
        _ => Box::new(Error {
            message: format!("unknown operator: {}{:?}", operator, right.kind()),
        }),
    }
}

fn eval_block_statement(block: BlockStatement, env: SharedEnvironment) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(Null);
    for s in *block.statements {
        result = eval(Node::Statement(s), env.clone());

        if result.kind() == ObjectType::Return || result.kind() == ObjectType::Error {
            return result;
        }
    }

    result
}

fn eval_expressions(args: Vec<Box<Expression>>, env: SharedEnvironment) -> Vec<Box<dyn Object>> {
    let mut result: Vec<Box<dyn Object>> = vec![];

    for expr in args {
        let evaluated = eval(Node::Expression(*expr), env.clone());
        if is_error(&evaluated) {
            result.push(evaluated);
            return result;
        }
        result.push(evaluated);
    }

    result
}

fn eval_array_index_expression(left: Box<dyn Object>, index: Box<dyn Object>) -> Box<dyn Object> {
    let array = left.get_value();
    let ObjectValue::Int(idx) = index.get_value() else {
        unreachable!();
    };

    let elements = match array {
        ObjectValue::Array(objects) => objects,
        _ => unreachable!(),
    };
    let max = if elements.len() > 0 {
        elements.len() - 1
    } else {
        0
    };

    if idx < 0 || idx > max as i64 {
        return Box::new(Null);
    }

    elements[idx as usize].clone()
}

fn unwrap_hash_index(index: ObjectValue) -> HashKey {
    match index {
        ObjectValue::Int(i) => HashKey::Int(i),
        ObjectValue::Bool(b) => HashKey::Bool(b),
        ObjectValue::Str(s) => HashKey::Str(s),
        _ => unreachable!(),
    }
}

fn eval_hash_index_expression(hash: Box<dyn Object>, index: Box<dyn Object>) -> Box<dyn Object> {
    let key = match index.kind() {
        ObjectType::Integer | ObjectType::Boolean | ObjectType::Str => {
            Some(unwrap_hash_index(index.get_value()))
        }
        _ => None,
    };

    if key.is_none() {
        return Box::new(Error {
            message: format!("unusable as hash key: {:?}", index.kind()),
        });
    }

    if let Some(pairs) = hash.get_hash_pairs() {
        let pair = pairs.get(&key.unwrap());
        if let Some(hp) = pair {
            return hp.value.clone();
        } else {
            return Box::new(Null);
        }
    } else {
        return Box::new(Null);
    }
}

fn eval_index_expression(left: Box<dyn Object>, index: Box<dyn Object>) -> Box<dyn Object> {
    if left.kind() == ObjectType::Array && index.kind() == ObjectType::Integer {
        return eval_array_index_expression(left, index);
    }

    if left.kind() == ObjectType::HashObject {
        return eval_hash_index_expression(left, index);
    }

    return Box::new(Error {
        message: format!("index operator not supported: {:?}", left.kind()),
    });
}

fn eval_hash_literal(literal: HashLiteral, env: SharedEnvironment) -> Box<dyn Object> {
    let mut pairs: HashMap<HashKey, HashPair> = HashMap::default();

    for (k, v) in literal.pairs {
        let key = eval(Node::Expression(*k), env.clone());
        if is_error(&key) {
            return key;
        }

        let value = eval(Node::Expression(*v), env.clone());
        if is_error(&value) {
            return value;
        }

        let hash_key = match key.get_value() {
            ObjectValue::Int(i) => HashKey::Int(i),
            ObjectValue::Bool(b) => HashKey::Bool(b),
            ObjectValue::Str(s) => HashKey::Str(s),
            _ => {
                return Box::new(Error {
                    message: format!("key of type {:?} is not hashable", key.get_value()),
                });
            }
        };

        pairs.insert(hash_key, HashPair { key, value });
    }

    Box::new(MonkeyHash { pairs })
}

fn extend_function_env(function: Box<dyn Object>, args: Vec<Box<dyn Object>>) -> SharedEnvironment {
    let func = function.get_fn_object().unwrap();
    let env = Environment::new_enclosed(func.env);

    for (idx, param) in func.parameters.iter().enumerate() {
        env.borrow_mut()
            .set(param.value.clone(), args[idx].clone_box());
    }

    env
}

fn unwrap_return_value(object: Box<dyn Object>) -> Box<dyn Object> {
    if object.kind() == ObjectType::Return {
        return object.get_return_value().unwrap();
    }

    object
}

fn apply_function(func: Box<dyn Object>, args: Vec<Box<dyn Object>>) -> Box<dyn Object> {
    if func.kind() == ObjectType::NativeFunction {
        if let ObjectValue::Native(f) = func.get_value() {
            return f(args);
        }
    }

    let function = func.get_fn_object();
    if function.is_none() {
        // can this ever actually happen?
        return Box::new(Error {
            message: format!("not a function: {:?}", func.kind()),
        });
    }

    let extended_env = extend_function_env(func, args);
    let evaluated = eval(
        Node::Statement(Statement::Block(*function.unwrap().body)),
        extended_env,
    );

    unwrap_return_value(evaluated)
}

fn eval_program(statements: Vec<Statement>, env: SharedEnvironment) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(Null);
    for s in statements {
        result = eval(Node::Statement(s), env.clone());

        match result.kind() {
            ObjectType::Return => {
                return match result.get_value() {
                    ObjectValue::Int(i) => Box::new(Integer { value: i }),
                    ObjectValue::Bool(b) => Box::new(Boolean { value: b }),
                    ObjectValue::Null => Box::new(Null),
                    _ => panic!("unexpected expression in return statement"),
                };
            }
            ObjectType::Error => return result,
            _ => continue,
        }
    }

    result
}

fn is_error(obj: &Box<dyn Object>) -> bool {
    obj.kind() == ObjectType::Error
}

pub fn eval(node: Node, env: SharedEnvironment) -> Box<dyn Object> {
    match node {
        Node::Program(program) => eval_program(program.statements, env),

        Node::Statement(statement) => match statement {
            Statement::Expression(expression_statement) => {
                eval(Node::Expression(*expression_statement.expression), env)
            }

            Statement::Let(let_statement) => {
                let value = eval(Node::Expression(*let_statement.value), env.clone());
                if is_error(&value) {
                    return value;
                }

                env.clone().borrow_mut().set(let_statement.id.value, value);

                Box::new(Null)
            }

            Statement::Return(return_statement) => {
                let mut value: Box<dyn Object> = Box::new(Null);

                if return_statement.value.is_some() {
                    value = eval(Node::Expression(*return_statement.value.unwrap()), env);
                }
                if is_error(&value) {
                    return value;
                }
                return Box::new(Return { value });
            }

            Statement::Block(block_statement) => eval_block_statement(block_statement, env),
        },

        Node::Expression(expression) => match expression {
            Expression::Identifier(identifier_expression) => {
                eval_identifier(identifier_expression, env)
            }

            Expression::Bool(boolean_expression) => Box::new(Boolean {
                // todo: figure out how to return the same shared reference,
                // since booleans are such simple objects
                value: boolean_expression.value,
            }),

            Expression::Integer(integer_literal) => Box::new(Integer {
                value: integer_literal.value,
            }),

            Expression::Prefix(prefix_expression) => {
                let right = eval(Node::Expression(*prefix_expression.right), env);

                if is_error(&right) {
                    return right;
                }

                return eval_prefix_expression(&prefix_expression.operator, right);
            }

            Expression::Infix(infix_expression) => {
                let left = eval(Node::Expression(*infix_expression.left), env.clone());
                if is_error(&left) {
                    return left;
                }

                let right = eval(Node::Expression(*infix_expression.right), env);
                if is_error(&right) {
                    return right;
                }
                return eval_infix_expression(&infix_expression.operator, left, right);
            }

            Expression::If(if_expression) => eval_if_expression(if_expression, env),

            Expression::Function(function_literal) => Box::new(Function {
                parameters: function_literal.parameters,
                body: function_literal.body.clone(),
                env,
            }),

            Expression::Call(call_expression) => {
                let function = eval(Node::Expression(*call_expression.function), env.clone());
                if is_error(&function) {
                    return function;
                }
                let args = eval_expressions(call_expression.arguments, env);
                if args.len() == 1 && is_error(&args[0]) {
                    return args[0].clone();
                }

                return apply_function(function, args);
            }

            Expression::String(string_literal) => Box::new(Str {
                value: string_literal.value.clone(),
            }),

            Expression::Array(array_literal) => {
                let elements = eval_expressions(array_literal.elements, env);

                if elements.len() == 1 && is_error(&elements[0]) {
                    elements[0].clone()
                } else {
                    Box::new(Array { elements })
                }
            }

            Expression::Index(index_expression) => {
                let left = eval(Node::Expression(*index_expression.left), env.clone());

                if is_error(&left) {
                    return left.clone();
                }

                let index = eval(Node::Expression(*index_expression.index), env);

                if is_error(&index) {
                    return index.clone();
                }

                eval_index_expression(left, index)
            }

            Expression::Hash(hash_literal) => eval_hash_literal(hash_literal, env),
        },
    }
}

mod native_functions;

#[cfg(test)]
mod evaluator_tests;
