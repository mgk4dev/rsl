use crate::engine::parser::literals::*;
use crate::engine::parser::{ast, Rule};

use std::error::Error;
use std::sync::Arc;

pub fn parse_single_type_expression(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::TypeExpression, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::single_type_expression);

    let mut types = vec![];
    for single_type_expression_item_rule in rule.into_inner() {
        let type_expression_item = single_type_expression_item_rule
            .into_inner()
            .next()
            .unwrap();
        match type_expression_item.as_rule() {
            Rule::identifier => {
                types.push(ast::TypeExpression::Concrete(parse_identifier(
                    type_expression_item,
                )?));
            }
            Rule::single_generic_type_instance => {
                let mut rule = type_expression_item.into_inner();
                let name = parse_identifier(rule.next().unwrap())?;
                let type_list_rule = rule.next().unwrap().into_inner();
                let mut type_list = vec![];
                for type_expression_rule in type_list_rule {
                    type_list.push(parse_type_expression(type_expression_rule)?)
                }
                types.push(ast::TypeExpression::Generic {
                    name: name,
                    args: type_list,
                });
            }
            _ => {
                eprintln!("Unknown type expression item: {:?}", type_expression_item);
            }
        }
    }
    assert!(types.len() >= 1);
    if types.len() == 1 {
        return Ok(types.pop().unwrap());
    }
    return Ok(ast::TypeExpression::Path(types));
}

pub fn parse_tuple_type_expression(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::TypeExpression, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::tuple_type_expression);
    let mut result = vec![];
    for rule in rule.into_inner() {
        result.push(parse_type_expression(rule)?);
    }
    Ok(ast::TypeExpression::Tuple(result))
}

pub fn parse_type_expression(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::TypeExpression, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::type_expression);

    let mut result = ast::TypeExpression::default();

    let rule = rule.into_inner().next().unwrap();
    match rule.as_rule() {
        Rule::unit_type_expression => {
            result = ast::TypeExpression::Unit;
        }
        Rule::single_type_expression => {
            result = parse_single_type_expression(rule)?;
        }
        Rule::tuple_type_expression => {
            result = parse_tuple_type_expression(rule)?;
        }
        _ => {
            eprintln!("Unknown type expression item: {:?}", rule);
        }
    }

    Ok(result)
}

pub fn parse_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::expression);
    let rule = rule.into_inner().next().unwrap();

    let result = match rule.as_rule() {
        Rule::range_expression => parse_range_expression(rule)?,
        Rule::cast_expression => parse_cast_expression(rule)?,
        Rule::type_test_expression => parse_type_test_expression(rule)?,
        Rule::logical_expression => parse_logical_expression(rule)?,
        _ => {
            eprintln!("!!!!! unknown rule {:#?} {}", rule.as_rule(), rule.as_str());
            ast::Expression::Unknown
        }
    };
    Ok(result)
}

pub fn parse_tuple_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::tuple_expression);
    let mut result = vec![];
    for rule in rule.into_inner() {
        result.push(parse_expression(rule)?);
    }
    Ok(ast::Expression::Tuple(result))
}

pub fn parse_range_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::range_expression);
    let rule = rule.into_inner().next().unwrap();
    let result = match rule.as_rule() {
        Rule::closed_range_expression => {
            let mut rules = rule.into_inner();
            ast::Expression::Range(ast::RangeExpression::Closed(
                Arc::new(parse_primary_expression(rules.next().unwrap())?),
                Arc::new(parse_primary_expression(rules.next().unwrap())?),
            ))
        }
        Rule::open_range_expression => {
            let mut rules = rule.into_inner();
            ast::Expression::Range(ast::RangeExpression::Open(
                Arc::new(parse_primary_expression(rules.next().unwrap())?),
                Arc::new(parse_primary_expression(rules.next().unwrap())?),
            ))
        }
        _ => {
            eprintln!("!!!!! unknown rule {:#?} {}", rule.as_rule(), rule.as_str());
            ast::Expression::Unknown
        }
    };

    Ok(result)
}

pub fn parse_cast_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::cast_expression);
    let rule = rule.into_inner().next().unwrap();
    let mut rules = rule.into_inner();

    let result = ast::CastExpression {
        expression: Arc::new(parse_logical_expression(rules.next().unwrap())?),
        type_: parse_type_expression(rules.next().unwrap())?,
    };

    Ok(ast::Expression::Cast(result))
}

pub fn parse_type_test_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::type_test_expression);
    let rule = rule.into_inner().next().unwrap();
    let mut rules = rule.into_inner();

    let result = ast::TypeTestExpression {
        expression: Arc::new(parse_logical_expression(rules.next().unwrap())?),
        type_: parse_type_expression(rules.next().unwrap())?,
    };

    Ok(ast::Expression::TypeTest(result))
}

pub fn parse_primary_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::primary_expression);
    let rule = rule.into_inner().next().unwrap();
    let result = match rule.as_rule() {
        Rule::type_construction => parse_type_construction(rule)?,
        Rule::expression_path => parse_expression_path(rule)?,
        Rule::literal => parse_literal(rule)?,
        Rule::expression => parse_expression(rule)?,
        Rule::tuple_expression => parse_tuple_expression(rule)?,
        _ => {
            eprintln!("!!!!! unknown rule {:#?} {}", rule.as_rule(), rule.as_str());
            ast::Expression::Unknown
        }
    };

    Ok(result)
}

pub fn parse_type_construction<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::type_construction);

    let mut result = ast::TypeConstruction::default();
    let mut rules = rule.into_inner();
    result.type_ = parse_type_expression(rules.next().unwrap())?;
    for rule in rules.next().unwrap().into_inner() {
        let rule = rule.into_inner().next().unwrap();
        match rule.as_rule() {
            Rule::type_construction_property_set => {
                let mut def = rule.into_inner();
                result.args.push(ast::TypeConstructArg::Set(
                    ast::Identifier(def.next().unwrap().as_str()),
                    parse_expression(def.next().unwrap())?,
                ));
            }
            Rule::type_construction_property_push => {
                let mut def = rule.into_inner();
                result
                    .args
                    .push(ast::TypeConstructArg::Push(parse_expression(
                        def.next().unwrap(),
                    )?));
            }
            _ => {
                println!(
                    "unknown type_construction rule {:#?} {}",
                    rule.as_rule(),
                    rule.as_str()
                );
            }
        }
    }
    Ok(ast::Expression::TypeConstruction(result))
}

pub fn parse_expression_path<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::expression_path);
    let rules = rule.into_inner();
    let mut result = ast::ExpressionPath::default();

    for rule in rules {
        let rule = rule.into_inner().next().unwrap();
        match rule.as_rule() {
            Rule::function_call => {
                result
                    .items
                    .push(ast::ExpressionPathItem::FunctionCall(parse_function_call(
                        rule,
                    )?));
            }
            Rule::single_generic_type_instance => {
                result
                    .items
                    .push(ast::ExpressionPathItem::GenericTypeInstance(
                        parse_single_generic_type_instance(rule)?,
                    ));
            }
            Rule::identifier => {
                result
                    .items
                    .push(ast::ExpressionPathItem::Property(parse_identifier(rule)?));
            }
            _ => {
                println!(
                    "unknown expression_path_item rule {:#?} {}",
                    rule.as_rule(),
                    rule.as_str()
                );
            }
        }
    }

    if result.items.len() == 1 {
        let item = result.items.pop().unwrap();
        match item {
            ast::ExpressionPathItem::Property(id) => {
                return Ok(ast::Expression::Identifier(id));
            }
            ast::ExpressionPathItem::FunctionCall(func_call) => {
                return Ok(ast::Expression::FunctionCall(func_call));
            }
            ast::ExpressionPathItem::GenericTypeInstance(type_instance) => {
                return Ok(ast::Expression::GenericTypeInstance(type_instance));
            }
        }
    }

    Ok(ast::Expression::ExpressionPath(result))
}

pub fn parse_function_call<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::FunctionCall<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::function_call);
    let mut result = ast::FunctionCall::default();
    let mut rules = rule.into_inner();
    result.name = parse_identifier(rules.next().unwrap())?;
    if let Some(function_call_arguments_rule) = rules.next() {
        for expression_rule in function_call_arguments_rule.into_inner() {
            result.arguments.push(parse_expression(expression_rule)?);
        }
    }

    Ok(result)
}

pub fn parse_single_generic_type_instance<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::GenericTypeInstance<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::single_generic_type_instance);
    let mut result = ast::GenericTypeInstance::default();
    let mut rules = rule.into_inner();
    result.name = parse_identifier(rules.next().unwrap())?;
    for rule in rules.next().unwrap().into_inner() {
        result.args.push(parse_type_expression(rule)?);
    }
    Ok(result)
}

pub fn parse_logical_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::logical_expression);

    let mut rules = rule.into_inner();
    let left = parse_equality_expression(rules.next().unwrap())?;
    let mut right_vec = vec![];
    while let Some(op) = rules.next() {
        let op = parse_logical_operator(op)?;
        let expr = parse_equality_expression(rules.next().unwrap())?;
        right_vec.push((op, expr));
    }

    if right_vec.is_empty() {
        return Ok(left);
    }
    return Ok(ast::Expression::Logical(ast::LogicalExpression::new(
        left, right_vec,
    )));
}

pub fn parse_equality_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::equality_expression);

    let mut rules = rule.into_inner();
    let left = parse_relational_expression(rules.next().unwrap())?;

    let mut right_vec = vec![];

    while let Some(op) = rules.next() {
        let op = parse_equality_operator(op)?;
        let expr = parse_relational_expression(rules.next().unwrap())?;
        right_vec.push((op, expr));
    }

    if right_vec.is_empty() {
        return Ok(left);
    }
    return Ok(ast::Expression::Equality(ast::EqualityExpression::new(
        left, right_vec,
    )));
}

pub fn parse_relational_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::relational_expression);

    let mut rules = rule.into_inner();
    let left = parse_additive_expression(rules.next().unwrap())?;
    let mut right_vec = vec![];

    while let Some(op) = rules.next() {
        let op = parse_relational_operator(op)?;
        let expr = parse_additive_expression(rules.next().unwrap())?;
        right_vec.push((op, expr));
    }

    if right_vec.is_empty() {
        return Ok(left);
    }
    return Ok(ast::Expression::Relational(ast::RelationalExpression::new(
        left, right_vec,
    )));
}

pub fn parse_additive_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::additive_expression);
    let mut rules = rule.into_inner();
    let left = parse_multiplicative_expression(rules.next().unwrap())?;
    let mut right_vec = vec![];
    while let Some(op) = rules.next() {
        let op = parse_additive_operator(op)?;
        let expr = parse_multiplicative_expression(rules.next().unwrap())?;
        right_vec.push((op, expr));
    }

    if right_vec.is_empty() {
        return Ok(left);
    }
    return Ok(ast::Expression::Additive(ast::AdditiveExpression::new(
        left, right_vec,
    )));
}

pub fn parse_multiplicative_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::multiplicative_expression);
    let mut rules = rule.into_inner();
    let left = parse_unary_expression(rules.next().unwrap())?;
    let mut right_vec = vec![];
    while let Some(op) = rules.next() {
        let op = parse_multiplicative_operator(op)?;
        let expr = parse_unary_expression(rules.next().unwrap())?;
        right_vec.push((op, expr));
    }

    return if right_vec.is_empty() {
        Ok(left)
    } else {
        Ok(ast::Expression::Multiplicative(
            ast::MultiplicativeExpression::new(left, right_vec),
        ))
    };
}

pub fn parse_unary_expression<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::unary_expression);

    let mut operator: Option<ast::UnaryOperator> = None;
    let mut expression: Option<ast::Expression> = None;

    for rule in rule.into_inner() {
        match rule.as_rule() {
            Rule::unary_op => {
                operator = Some(parse_unary_operator(rule)?);
            }
            Rule::primary_expression => {
                expression = Some(parse_primary_expression(rule)?);
            }
            _ => {
                println!(
                    "unknown unary_expression rule {:#?} {}",
                    rule.as_rule(),
                    rule.as_str()
                );
            }
        }
    }

    return if let Some(operator) = operator {
        let result = ast::UnaryExpression {
            operator: Some(operator),
            expression: Arc::new(expression.unwrap()),
        };
        Ok(ast::Expression::Unary(result))
    } else {
        Ok(expression.unwrap())
    };
}
