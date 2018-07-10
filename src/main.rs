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
use std::io::Write;

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
use compiler::{
    Compiler,
    Operation,
};
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
    let preprocessor = Preprocessor::new(&code);
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


    // compile
    println!("Write compiled");
    let mut compiler = Compiler::new();
    let mut res = Some(0);
    for r in &syntax_tree{
        match r{
            Root::VariableDefinition(_) => {
                unimplemented!("Global variable yet to be supported")
            },
            Root::FunctionDefinition(x) => {
                res = compiler.register_function_definition(x).ok();
            }
        }
    }

    let res: Option<Vec<_>> = res.and_then(|_|{
        syntax_tree.iter()
            .filter_map(|x|{
                match x{
                    Root::FunctionDefinition(x) => Some(x),
                    _ => None
                }
            })
            .map(|x| compiler.compile_function(x))
            .collect()
    });

    let function_entry_points: Vec<(usize, usize)> = if let Some(res) = res{
        //compiler.print();
        res.iter().map(|x| *x).enumerate().collect()
    }else {
        Error::print_errors(compiler.errors(), &line_starts);
        return;
    };

    // build html page
    println!("Build HTML page.");
    let mut out = File::create(output).unwrap();
    let mut context = Context::new();
    context.set_debug_info(compiler.get_debug_info());
    for r in &syntax_tree{
        r.write_page(&mut out, &mut context).unwrap();
    }
    out.sync_all().unwrap();

    // build vm code
    println!("Build VM page.");
    let mut out= File::create(output.to_string() + ".js").unwrap();
    write_compiled_to_js(&mut out, &compiler, function_entry_points.as_slice()).unwrap();
    out.sync_all().unwrap();

}


/// Write virtual machine (vm/vm.js) bytecode of compiled code into writer
/// 
/// # Arguments
/// 
/// * `writer` - buffer into which code will be writter. May be `std::fs::File`
/// 
/// * `compiler` - instance of `Compiler`, which compiled syntax tree into code
/// 
/// * `function_entry_points` - array of pairs (function_id, function_start) where function_id is
/// id of compiled function provided by `compiler` and function_start - offset of function entry
/// point in compiled bytecode
fn write_compiled_to_js<'a, T: Write>(writer: &mut T, compiler: &Compiler<'a>, function_entry_points: &[(usize, usize)])->std::io::Result<()>{
    write!(writer, "var compiled = {{commands: [")?;
    let mut iter = compiler.code().iter();
    if let Some(op) = iter.next(){
        write_operation_to_js(writer, op)?;
    }
    for op in iter{
        write!(writer, ",")?;
        write_operation_to_js(writer, op)?;
    }
    write!(writer, "], function_enters: [")?;
    let mut iter = function_entry_points.iter();
    if let Some((fid, entry)) = iter.next(){
        write!(writer, "[{}, {}]", fid, entry)?;
    }
    for (fid, entry) in iter {
        write!(writer, ",[{}, {}]", fid, entry)?;
    }
    write!(writer, "]}}")?;
    Ok(())
}

/// Write a single `Operation` into javascript representation of vm bytecode
fn write_operation_to_js<T: Write>(writer: &mut T, operation: &Operation)->std::io::Result<()>{
    write!(writer, "[{}", operation.code as usize)?;
    for arg in &operation.args{
        write!(writer, ", {}", arg)?;
    }
    write!(writer, "]")
}
