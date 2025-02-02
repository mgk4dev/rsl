extern crate core;

use pest::Parser;
use rsl::engine::parser;
use rsl::engine::parser::{Program, Rule};
use std::error::Error;
use std::fmt::{Debug, Display};

fn check_parse_result<T: Debug, E: Display>(result: &Result<T, E>, print_result: bool) {
    match result {
        Ok(val) => {
            if print_result {
                println!("Result: {:#?}", val);
            }
        }
        Err(err) => {
            println!("Error: {}", err);
            assert!(false);
        }
    }
}

#[test]
fn parse_bool() {
    let inputs = ["true", "false"];
    for input in &inputs {
        assert_eq!(Program::parse(Rule::bool_literal, input).is_ok(), true);
    }
}

#[test]
fn parse_integer() {
    let inputs = ["1", "-1"];
    for input in &inputs {
        assert_eq!(Program::parse(Rule::integer_literal, input).is_ok(), true);
    }
}

#[test]
fn parse_real() {
    let inputs = ["1.4", "-1.58", "1.8e+8", "1.8e-8", "1.8e8"];
    for input in &inputs {
        assert_eq!(Program::parse(Rule::real_literal, input).is_ok(), true);
    }
}

#[test]
fn parse_string() {
    let inputs = [r#""hello""#, r#""hello \" str""#];
    for input in &inputs {
        assert_eq!(Program::parse(Rule::string_literal, input).is_ok(), true);
    }
}

#[test]
fn parse_identifier() {
    let inputs = ["toto", "toto22", "_toto", "_22"];
    for input in &inputs {
        assert_eq!(Program::parse(Rule::identifier, input).is_ok(), true);
    }
}

#[test]
fn parse_literals() {
    let inputs = [
        "toto",
        "toto22",
        "_toto",
        "_22",
        r#""hello""#,
        r#""hello \" str""#,
        "1.4",
        "-1.58",
        "1.8e+8",
        "1.8e-8",
        "1.8e8",
        "1",
        "-1",
        "true",
        "false",
    ];
    for input in &inputs {
        assert!(Program::parse(Rule::expression, input).is_ok());
    }
}

#[test]
fn parse_type_expression() {
    let inputs = [
        "Int",
        "Array<Int>",
        "Map<Int, String>",
        "mod.Type",
        "mod.sub.Type",
        "mod.sub.Type<A>",
        "mod.sub.Type<A, B>",
    ];
    for input in &inputs {
        assert!(Program::parse(Rule::type_expression, input).is_ok());
    }
}

#[test]
fn parse_expression_path() {
    let inputs = [
        "a",
        "a.b",
        "a.b()",
        "a.b(1, 2)",
        "a(1).b(2)",
        "a(1).b",
        "a().b()",
    ];
    for input in &inputs {
        check_parse_result(&Program::parse(Rule::expression_path, input), false);
    }

    for input in &inputs {
        check_parse_result(&Program::parse(Rule::expression, input), false);
    }
}

#[test]
fn parse_struct() {
    let inputs = ["struct Foo {}", "struct Foo {a : A, b : B}"];

    for input in &inputs {
        assert!(Program::parse(Rule::structure, input).is_ok());
    }
}

#[test]
fn parse_assignment() {
    let inputs = ["a = 10;", "a = B {};", "a = B { a: 10, b : 20};"];
    for input in &inputs {
        assert!(Program::parse(Rule::assignment, input).is_ok());
    }
}

#[test]
fn parse_type_instantiation() {
    let inputs = ["Foo {}", "Foo {a : 1, b : 2}", "Foo {a : 1 + 5, b : 2 + a}"];

    for input in &inputs {
        assert!(Program::parse(Rule::expression, input).is_ok());
    }
}

#[test]
fn parse_function() {
    let inputs = [
        "fn foo() -> C {}",
        "fn foo() {}",
        "fn foo(a : A, b : B) -> C {}",
        "fn foo(a : A, b : B)  {}",
    ];

    for input in &inputs {
        assert!(Program::parse(Rule::function, input).is_ok());
    }
}
#[test]
fn parse_expression() {
    let inputs = [
        "1",
        "true",
        "1.8",
        "\"hello\"",
        "a",
        "a.b",
        "(1 + 2)",
        "1..2",
    ];

    for input in &inputs {
        check_parse_result(&Program::parse(Rule::expression, input), false);
    }
}

#[test]
fn parse_for_loop() {
    let inputs = ["for i in 0 .. 10 { i = i * 2; break; }"];

    for input in &inputs {
        check_parse_result(&Program::parse(Rule::for_statement, input), false);
    }
}

#[test]
fn parse_program_statements() -> Result<(), Box<dyn Error>> {
    let inputs = [
        "fn foo(a : A, b : B) -> C {}",
        "struct Foo { a : A, b : B }",
        "55",
    ];

    for input in &inputs {
        Program::parse(Rule::program, input)?;
    }
    Ok(())
}

#[test]
fn parse_program() {
    let input = r#"
        import foo;
        import foo as bar;
        fn foo (a : A, b : B) -> Foo {}

        struct Foo { a : A, b : B}
        enum Bar {
            Int,
            Array<String>,
            Bool
        }

        trait Foo {
            fn say_hello(self: Self, a : A);
        }

        let a : A = A {};
        a = B { a: 10, b : 20};
        if a > 10 { let b : B = 1; }
        elif a == 10 { let b : B = 2; }
        else { let b : B = 3; }
        for i in 0 ..= 10 { i = i * 2; break; }
        33 ;
        58
    "#;

    check_parse_result(&parser::parse_program(input), false);
}

#[test]
fn debug() {
    let inputs = ["T", "(A, B)", "(A, B, C<D>)", "A<(B,C<E>)>", "A.B.C<D,E>"];

    for input in &inputs {
        let parsed = Program::parse(Rule::type_expression, input);
        println!("============= input '{}'", input);
        check_parse_result(&parsed, false);
        check_parse_result(
            &parser::expression::parse_type_expression(parsed.unwrap().next().unwrap()),
            false,
        );
    }
}
