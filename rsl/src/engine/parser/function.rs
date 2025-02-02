use crate::engine::parser::expression::*;
use crate::engine::parser::literals::*;
use crate::engine::parser::{ast, Rule};

use std::error::Error;

pub fn parse_assignment(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::AssignmentStatement, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::assignment);
    let mut rules = rule.into_inner();
    let mut result = ast::AssignmentStatement::default();
    result.left = parse_expression(rules.next().unwrap())?;
    result.right = parse_expression(rules.next().unwrap())?;
    Ok(result)
}

pub fn parse_variable_declaration(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::VariableDeclarationStatement, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::variable_declaration);
    let mut rules = rule.into_inner();
    let mut result = ast::VariableDeclarationStatement::default();

    result.name = parse_identifier(rules.next().unwrap())?;
    result.type_ = parse_type_expression(rules.next().unwrap())?;
    result.value = parse_expression(rules.next().unwrap())?;

    Ok(result)
}

pub fn parse_break_statement(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::BreakStatement, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::break_statement);
    let mut result = ast::BreakStatement::default();
    if let Some(rule) = rule.into_inner().next() {
        result.expression = Some(parse_expression(rule)?);
    }
    Ok(result)
}

pub fn parse_return_statement(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::ReturnStatement, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::return_statement);
    let mut result = ast::ReturnStatement::default();
    if let Some(rule) = rule.into_inner().next() {
        result.expression = Some(parse_expression(rule)?);
    }
    Ok(result)
}

pub fn parse_if_statement(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::IfStatement, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::if_statement);
    let mut result = ast::IfStatement::default();
    let mut rules = rule.into_inner();

    {
        let rule = rules.next().unwrap();
        let mut rules = rule.into_inner();

        result.main_condition.condition = parse_expression(rules.next().unwrap())?;
        result.main_condition.scope = parse_scope(rules.next().unwrap())?;
    }

    for rule in rules {
        match rule.as_rule() {
            Rule::elif_scope => {
                let mut conditional_scope = ast::ConditionalScope::default();
                let mut rules = rule.into_inner();
                conditional_scope.condition = parse_expression(rules.next().unwrap())?;
                conditional_scope.scope = parse_scope(rules.next().unwrap())?;
                result.elif_conditions.push(conditional_scope);
            }

            Rule::else_scope => {
                let mut rules = rule.into_inner();
                result.else_scope = Some(parse_scope(rules.next().unwrap())?);
            }
            _ => {
                eprintln!("Unknown if-statement rule: {:?}", rule);
            }
        }
    }

    Ok(result)
}

pub fn parse_function_statement(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::FunctionStatement, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::function_statement);
    let rule = rule.into_inner().next().unwrap();
    let result = match rule.as_rule() {
        Rule::variable_declaration => {
            ast::FunctionStatement::Var(parse_variable_declaration(rule)?)
        }
        Rule::assignment => ast::FunctionStatement::Assignment(parse_assignment(rule)?),
        Rule::if_statement => ast::FunctionStatement::If(parse_if_statement(rule)?),
        Rule::for_statement => ast::FunctionStatement::For(parse_for_statement(rule)?),
        Rule::break_statement => ast::FunctionStatement::Break(parse_break_statement(rule)?),
        Rule::return_statement => ast::FunctionStatement::Return(parse_return_statement(rule)?),
        Rule::expression => ast::FunctionStatement::Expression(parse_expression(rule)?),
        _ => {
            eprintln!("Unknown function-statement rule: {:?}", rule);
            ast::FunctionStatement::Unknown
        }
    };

    Ok(result)
}

pub fn parse_scope(rule: pest::iterators::Pair<Rule>) -> Result<ast::Scope, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::scope);
    let mut result = ast::Scope::default();
    for rule in rule.into_inner() {
        match rule.as_rule() {
            Rule::function_statement => result.statements.push(parse_function_statement(rule)?),
            Rule::expression => {
                result.return_expression = Some(parse_expression(rule)?);
            }
            _ => {
                eprintln!("Unknown function-statement rule: {:?}", rule);
            }
        }
    }
    Ok(result)
}

pub fn parse_for_statement(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::ForStatement, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::for_statement);
    let mut result = ast::ForStatement::default();
    let mut rules = rule.into_inner();

    result.var = parse_identifier(rules.next().unwrap())?;
    result.expression = parse_expression(rules.next().unwrap())?;
    result.scope = parse_scope(rules.next().unwrap())?;

    Ok(result)
}

pub fn parse_function(rule: pest::iterators::Pair<Rule>) -> Result<ast::Function, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::function);
    let mut result = ast::Function::default();
    let mut rules = rule.into_inner();
    result.name = parse_identifier(rules.next().unwrap())?;
    result.signature = parse_function_signature(rules.next().unwrap())?;
    result.scope = parse_scope(rules.next().unwrap())?;
    Ok(result)
}

pub fn parse_function_signature(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::FunctionSignature, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::function_signature);
    let mut rules = rule.into_inner();
    let mut signature = ast::FunctionSignature::default();
    signature.arguments = parse_identifier_and_type_list(rules.next().unwrap())?;
    if let Some(return_type) = rules.next() {
        signature.return_type = Some(parse_type_expression(
            return_type.into_inner().next().unwrap(),
        )?);
    }
    Ok(signature)
}

pub fn parse_identifier_and_type_list(
    rule: pest::iterators::Pair<Rule>,
) -> Result<Vec<ast::IndentifierAndType>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::identifier_and_type_list);
    let mut result = Vec::new();
    for identifier_and_type_rule in rule.into_inner() {
        let mut identifier_and_type = ast::IndentifierAndType::default();
        let mut rules = identifier_and_type_rule.into_inner();
        identifier_and_type.name = parse_identifier(rules.next().unwrap())?;
        identifier_and_type.type_ = parse_type_expression(rules.next().unwrap())?;
        result.push(identifier_and_type);
    }
    Ok(result)
}
