use crate::engine::parser::error::ParsingError;
use crate::engine::parser::{ast, Rule};
use std::error::Error;

pub fn parse_literal<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::literal);
    let rule = rule.into_inner().next().unwrap();

    match rule.as_rule() {
        Rule::real_literal => parse_real(rule),
        Rule::integer_literal => parse_integer(rule),
        Rule::bool_literal => parse_boolean(rule),
        Rule::string_literal => Ok(parse_string(rule)),
        _ => {
            eprintln!("Unknown literal rule: {:?}", rule.as_rule());
            Ok(ast::Expression::Unknown)
        }
    }
}

pub fn parse_boolean<'a>(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::Expression<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::bool_literal);
    match rule.as_str() {
        "true" => Ok(ast::Expression::Boolean(true)),
        "false" => Ok(ast::Expression::Boolean(false)),
        _ => Err(Box::new(ParsingError::BooleanParsingError)),
    }
}

pub fn parse_integer<'a>(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::Expression, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::integer_literal);
    return Ok(ast::Expression::Int(rule.as_str().parse::<i128>()?));
}

pub fn parse_real<'a>(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::Expression, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::real_literal);
    return Ok(ast::Expression::Real(rule.as_str().parse::<f64>()?));
}

pub fn parse_string(rule: pest::iterators::Pair<Rule>) -> ast::Expression {
    assert_eq!(rule.as_rule(), Rule::string_literal);
    return ast::Expression::String(rule.as_str());
}

pub fn parse_relational_operator(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::RelationalOperator, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::relational_op);
    match rule.as_str() {
        "<" => Ok(ast::RelationalOperator::Less),
        "<=" => Ok(ast::RelationalOperator::LessEqual),
        ">" => Ok(ast::RelationalOperator::Greater),
        ">=" => Ok(ast::RelationalOperator::GreaterEqual),
        _ => Err(Box::new(ParsingError::RelationalOperatorParsingError)),
    }
}

pub fn parse_equality_operator(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::EqualityOperator, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::equality_op);
    match rule.as_str() {
        "==" => Ok(ast::EqualityOperator::Equal),
        "!=" => Ok(ast::EqualityOperator::NotEqual),
        _ => Err(Box::new(ParsingError::EqualityOperatorParsingError)),
    }
}

pub fn parse_logical_operator(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::LogicalOperator, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::logical_op);
    match rule.as_str() {
        "and" => Ok(ast::LogicalOperator::And),
        "or" => Ok(ast::LogicalOperator::Or),
        _ => Err(Box::new(ParsingError::LogicalOperatorParsingError)),
    }
}

pub fn parse_additive_operator(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::AdditiveOperator, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::additive_op);
    match rule.as_str() {
        "+" => Ok(ast::AdditiveOperator::Plus),
        "-" => Ok(ast::AdditiveOperator::Minus),
        _ => Err(Box::new(ParsingError::AdditiveOperatorParsingError)),
    }
}

pub fn parse_multiplicative_operator(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::MultiplicativeOperator, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::multiplicative_op);
    match rule.as_str() {
        "*" => Ok(ast::MultiplicativeOperator::Multiply),
        "/" => Ok(ast::MultiplicativeOperator::Divide),
        _ => Err(Box::new(ParsingError::MultiplicativeOperatorParsingError)),
    }
}

pub fn parse_unary_operator(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::UnaryOperator, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::unary_op);
    match rule.as_str() {
        "+" => Ok(ast::UnaryOperator::Plus),
        "-" => Ok(ast::UnaryOperator::Minus),
        "not" => Ok(ast::UnaryOperator::Not),
        _ => Err(Box::new(ParsingError::UnaryOperatorParsingError)),
    }
}

pub fn parse_identifier(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::Identifier, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::identifier);
    Ok(ast::Identifier(rule.as_str()))
}
