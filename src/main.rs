#![feature(test)]
#![feature(box_patterns)]
#![feature(log_syntax)]

#[macro_use] extern crate log;
extern crate env_logger;
#[macro_use] extern crate lazy_static;
extern crate test;
extern crate clap;


mod error;
mod lexer;
mod bracket_tree;
mod syntax_tree;
mod compiler;
mod page;
mod types;

use std::fs::File;
use std::io::Read;

use clap::{
    Arg,
    App,
};

use lexer::Preprocessor;
use bracket_tree::BracketTree;
use syntax_tree::parse_program;
use syntax_tree::Root;
use page::{
    PageElement,
    Context
};
use compiler::Compiler;
use error::Error;

fn main() {
    env_logger::init();

    let matches = App::new("Interactives")
        .version("0.1")
        .about("Build html page from \"C\" code.")
        .arg(
            Arg::with_name("INPUT")
                .help("Path to \"C\" file.")
                .required(true)
                .index(1)
        ).arg(
            Arg::with_name("OUTPUT")
                .short("o")
                .value_name("OUTPUT")
                .default_value("out.html")
                .help("Path to resulting html page.")
        ).get_matches();

    // get arguments
    let input = matches.value_of("INPUT").unwrap();
    let output = matches.value_of("OUTPUT").unwrap();

    // read file content

    let mut input = File::open(input).unwrap();
    let mut code = String::new();
    input.read_to_string(&mut code).unwrap();

    let line_starts: Vec<usize> = [0 as usize].iter()
        .map(|x| *x)
        .chain(code.char_indices()
                   .filter(|(_, x)| *x == '\n')
                   .map(|(i, _)| i+1)
        ).collect();

    // tokenize
    println!("Tokenize.");
    let mut preprocessor = Preprocessor::new(&code);
    let tokens = preprocessor.tokenize().unwrap_or_else(|err|{
        err.err_print_message(&line_starts);
        panic!("Execution aborted due to present error.")
    });
    // build syntax tree
    println!("Parse into AST.");
    let bracket_tree = BracketTree::new(&tokens).unwrap_or_else(|err|{
        err.err_print_message(&line_starts);
        panic!("Execution aborted due to present error.")
    });
    let mut walker = bracket_tree.walker();
    let mut error_stream = vec![];
    let syntax_tree = match parse_program(&mut walker, &mut error_stream){
        Some(x) => x,
        None => {
            Error::print_errors(&error_stream, &line_starts);
            panic!("Execution aborted due to present error.")
        }
    };
    // build html page
    println!("Build HTML page.");
    let mut output = File::create(output).unwrap();
    let mut context = Context::new();
    for r in &syntax_tree{
        r.write_page(&mut output, &mut context).unwrap();
    }
    output.sync_all().unwrap();

    println!("Write compiled");
    let mut compiler = Compiler::new();
    let res = syntax_tree.iter()
        .filter_map(|x|{
            match x{
                Root::FunctionDefinition(x) => Some(x),
                _ => None
            }
        })
        .map(|x| compiler.compile_function(x))
        .fold(Some(0), |a, b| a.and(b));

    if let Some(_) = res{
        compiler.print();
    }else {
        Error::print_errors(compiler.errors(), &line_starts);
    }
}
