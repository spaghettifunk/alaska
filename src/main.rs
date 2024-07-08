use std::{fs, path::Path};

mod args;
use args::Args;
use compiler_core::parser::ast::AST;
use compiler_core::parser::Parser;
use compiler_core::sema::SemanticAnalyzer;

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
            ast.files.push(file);
        }
        Err(e) => {
            eprintln!("Error parsing source file: {}", e);
            std::process::exit(1);
        }
    }

    let mut sema = SemanticAnalyzer::new();
    let result = sema.symbols_collection(ast);
    match result {
        Ok(_) => {
            println!("First pass (symbols collection) - completed");
            match sema.forward_references() {
                Ok(_) => {
                    println!("Second pass (forward references) - completed");
                    println!("");
                    println!("{}", sema);
                }
                Err(errors) => {
                    for error in errors {
                        eprintln!("{}", error);
                    }
                    std::process::exit(1);
                }
            }
        }
        Err(errors) => {
            for error in errors {
                eprintln!("{}", error);
            }
            std::process::exit(1);
        }
    }

    Ok(())
}
