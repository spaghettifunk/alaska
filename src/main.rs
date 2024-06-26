use std::{fs, path::Path};

mod args;
use args::Args;
use compiler::eval::SemanticAnalyzer;
use compiler::parser::ast::AST;
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

    let mut ast = AST { files: Vec::new() };

    // TODO: here I should incorporate the "package" system
    // here we should iterate through all the files
    // and parse them
    let source_file = parser.parse_input("test.ask");
    match source_file {
        Ok(file) => {
            // print ast
            // println!("{:#?}", file);

            ast.files.push(file);
        }
        Err(e) => {
            eprintln!("Error parsing source file: {}", e);
            std::process::exit(1);
        }
    }

    let mut sema = SemanticAnalyzer::new();
    match sema.symbols_collection(ast) {
        Ok(_) => {
            println!("Analysis complete");
        }
        Err(e) => {
            eprintln!("Error analyzing source file: {}", e);
            std::process::exit(1);
        }
    }

    Ok(())
}
