use crate::engine::parser::ast;
use crate::engine::parser::builtin::*;
use crate::engine::parser::expression::*;
use crate::engine::parser::function::*;
use crate::engine::parser::literals::*;

use pest::Parser;
use pest_derive::Parser;

use std::error::Error;

#[derive(Parser)]
#[grammar = "src/engine/parser/grammar.pest"]
pub struct Program;

pub fn parse_program<'a>(input: &'a str) -> Result<ast::ModuleDefinition<'a>, Box<dyn Error>> {
    let program = Program::parse(Rule::program, input)?.next().unwrap();
    let mut program_ast = ast::ModuleDefinition::default();
    for rule in program.into_inner() {
        match rule.as_rule() {
            Rule::module_statement => {
                program_ast.statements.push(parse_module_statement(rule)?);
            }
            Rule::expression => {
                program_ast.return_expression = Some(parse_expression(rule)?);
            }
            _ => {}
        }
    }
    Ok(program_ast)
}

pub fn parse_module_statement<'a>(
    rule: pest::iterators::Pair<'a, Rule>,
) -> Result<ast::ModuleStatement<'a>, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::module_statement);
    let rule = rule.into_inner().next().unwrap();
    let result = match rule.as_rule() {
        Rule::import_statement => ast::ModuleStatement::Import(parse_import_statement(rule)?),
        Rule::function => ast::ModuleStatement::Function(parse_function(rule)?),
        Rule::structure => ast::ModuleStatement::Structure(parse_structure(rule)?),
        Rule::enumeration => ast::ModuleStatement::Enum(parse_enumeration(rule)?),
        Rule::trait_ => ast::ModuleStatement::Trait(parse_trait(rule)?),
        Rule::assignment => ast::ModuleStatement::Assignment(parse_assignment(rule)?),
        Rule::expression => ast::ModuleStatement::Expression(parse_expression(rule)?),
        Rule::variable_declaration => {
            ast::ModuleStatement::VariableDeclaration(parse_variable_declaration(rule)?)
        }
        Rule::if_statement => ast::ModuleStatement::If(parse_if_statement(rule)?),
        Rule::for_statement => ast::ModuleStatement::For(parse_for_statement(rule)?),
        Rule::module_definition => ast::ModuleStatement::Module(parse_module_definition(rule)?),
        _ => {
            eprintln!("Unknown module statement: {:?}", rule);
            ast::ModuleStatement::Unknown
        }
    };

    Ok(result)
}

pub fn parse_import_statement(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::ImportStatement, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::import_statement);
    let mut rules = rule.into_inner();
    let mut result = ast::ImportStatement::default();
    let rule = rules.next().unwrap();
    result.module_name = parse_identifier(rule)?;
    result.alias = if let Some(rule) = rules.next() {
        Some(rule.as_str())
    } else {
        None
    };

    Ok(result)
}

pub fn parse_module_definition(
    rule: pest::iterators::Pair<Rule>,
) -> Result<ast::ModuleDefinition, Box<dyn Error>> {
    assert_eq!(rule.as_rule(), Rule::module_definition);
    let mut result = ast::ModuleDefinition::default();
    let mut rules = rule.into_inner();
    result.name = parse_identifier(rules.next().unwrap())?;
    for rule in rules {
        match rule.as_rule() {
            Rule::module_statement => result.statements.push(parse_module_statement(rule)?),
            Rule::expression => result.return_expression = Some(parse_expression(rule)?),
            _ => {
                eprintln!("Unknown module definition rule: {:?}", rule);
            }
        }
    }
    Ok(result)
}
