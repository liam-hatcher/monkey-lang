use crate::{
    ast::{BlockStatement, Expression, IfExpression, Node, Statement},
    object::{Boolean, Integer, Null, Object, ObjectType, ObjectValue, Return},
};

fn is_truthy(condition: Box<dyn Object>) -> bool {
    match condition.get_value() {
        ObjectValue::Null => false,
        ObjectValue::Bool(b) => b,
        _ => true,
    }
}

fn eval_if_expression(if_expression: IfExpression) -> Box<dyn Object> {
    let condition = eval(Node::Expression(*if_expression.condition));

    if is_truthy(condition) {
        return eval(Node::Statement(Statement::Block(
            *if_expression.consequence.unwrap(),
        )));
    } else if if_expression.alternative.is_some() {
        return eval(Node::Statement(Statement::Block(
            *if_expression.alternative.unwrap(),
        )));
    } else {
        return Box::new(Null);
    }
}

fn unwrap_int(node: Box<dyn Object>) -> i64 {
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
    let left = unwrap_int(left);
    let right = unwrap_int(right);

    match operator {
        "+" => Box::new(Integer {
            value: left + right,
        }),
        "-" => Box::new(Integer {
            value: left - right,
        }),
        "*" => Box::new(Integer {
            value: left * right,
        }),
        "/" => Box::new(Integer {
            value: left / right,
        }),
        "<" => Box::new(Boolean {
            value: left < right,
        }),
        ">" => Box::new(Boolean {
            value: left > right,
        }),
        "==" => Box::new(Boolean {
            value: left == right,
        }),
        "!=" => Box::new(Boolean {
            value: left != right,
        }),
        _ => Box::new(Null),
    }
}

fn eval_infix_expression(
    operator: &str,
    left: Box<dyn Object>,
    right: Box<dyn Object>,
) -> Box<dyn Object> {
    if left.kind() == ObjectType::Integer && right.kind() == ObjectType::Integer {
        return eval_integer_infix_expression(operator, left, right);
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

    Box::new(Null)
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

fn eval_minus_prefix_operator_expression(right: Box<dyn Object>) -> Box<dyn Object> {
    if right.kind() != ObjectType::Integer {
        Box::new(Null)
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
        _ => Box::new(Null),
    }
}

fn eval_block_statement(block: BlockStatement) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(Null);
    for s in *block.statements {
        result = eval(Node::Statement(s));

        if result.kind() == ObjectType::Return {
            return result;
        }
    }

    result
}

fn eval_program(statements: Vec<Statement>) -> Box<dyn Object> {
    let mut result: Box<dyn Object> = Box::new(Null);
    for s in statements {
        result = eval(Node::Statement(s));

        if result.kind() == ObjectType::Return {
            return match result.get_value() {
                ObjectValue::Int(i) => Box::new(Integer { value: i }),
                ObjectValue::Bool(b) => Box::new(Boolean { value: b }),
                ObjectValue::Null => Box::new(Null),
                _ => panic!("unexpected expression in return statement"),
            };
        }
    }

    result
}

pub fn eval(node: Node) -> Box<dyn Object> {
    match node {
        Node::Program(program) => eval_program(program.statements),

        Node::Statement(statement) => match statement {
            Statement::Expression(expression_statement) => {
                eval(Node::Expression(*expression_statement.expression))
            }

            Statement::Let(let_statement) => todo!(),

            Statement::Return(return_statement) => {
                let value = if return_statement.value.is_some() {
                    eval(Node::Expression(*return_statement.value.unwrap()))
                } else {
                    Box::new(Null)
                };
                return Box::new(Return { value });
            }

            Statement::Block(block_statement) => eval_block_statement(block_statement),
        },

        Node::Expression(expression) => match expression {
            Expression::Identifier(identifier_expression) => todo!(),

            Expression::Bool(boolean_expression) => Box::new(Boolean {
                // todo: figure out how to return the same shared reference,
                // since booleans are such simple objects
                value: boolean_expression.value,
            }),

            Expression::Integer(integer_literal) => Box::new(Integer {
                value: integer_literal.value,
            }),

            Expression::Prefix(prefix_expression) => {
                let right = eval(Node::Expression(*prefix_expression.right));
                return eval_prefix_expression(&prefix_expression.operator, right);
            }

            Expression::Infix(infix_expression) => {
                let left = eval(Node::Expression(*infix_expression.left));
                let right = eval(Node::Expression(*infix_expression.right));
                return eval_infix_expression(&infix_expression.operator, left, right);
            }

            Expression::If(if_expression) => eval_if_expression(if_expression),

            Expression::Function(function_literal) => todo!(),

            Expression::Call(call_expression) => todo!(),
        },
    }
}

#[cfg(test)]
mod evaluator_tests;
