use std::error::Error;

use rsl::engine::parser;

fn main() -> Result<(), Box<dyn Error>> {
    
    let input = std::fs::read_to_string(
        "/Users/mgk/Documents/workspace/prototypes/rsl/rsl/tests/resources/001.rsl",
    )?;

    let ast = parser::parse_program(&input)?;
    dbg!(ast);

    Ok(())
}
