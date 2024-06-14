use std::{fs, path::Path};

mod args;
use args::Args;
use compiler::parser::Parser;

pub type Result<T> = anyhow::Result<T>;

fn main() -> Result<()> {
    let args = Args::read();

    let _build_path = Path::new(&args.build_script)
        .to_str()
        .ok_or(anyhow::anyhow!("Invalid build script path"))?;
    let build = fs::read_to_string(&args.build_script)?;
    let build = build.as_str();
    println!("Parsing Build File");

    let mut parser = Parser::new(build);
    let ast = parser.parse_input("test.ask");

    // print ast
    println!("{:#?}", ast);

    Ok(())
}
