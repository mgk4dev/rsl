use crate::engine::parser::expression::parse_type_expression;
use crate::engine::parser::function::*;
use crate::engine::parser::literals::*;
use crate::engine::parser::{ast, Rule};
use std::error::Error;

pub fn parse_structure(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::Structure, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::structure);
    let mut result = ast::Structure::default();
    let mut rules = rule.into_inner();

    result.name = parse_identifier(rules.next().unwrap())?;

    if let Some(rule) = rules.next() {
        result.fields = parse_identifier_and_type_list(rule)?;
    }

    Ok(result)
}

pub fn parse_enumeration(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::Enumeration, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::enumeration);
    let mut rules = rule.into_inner();
    let mut result = ast::Enumeration::default();
    result.name = parse_identifier(rules.next().unwrap())?;
    for rule in rules {
        result.fields.push(parse_type_expression(rule)?);
    }
    Ok(result)
}

pub fn parse_trait(rule: pest::iterators::Pair<Rule>) -> Result<ast::Trait, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::trait_);
    let mut result = ast::Trait::default();
    
    let mut rules = rule.into_inner();
    
    result.name = parse_identifier(rules.next().unwrap())?;
    
    for rule in rules {
        result.
    }
    
    
    Ok(result)
}
