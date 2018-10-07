#![feature(test)]
#![feature(box_patterns)]
#![feature(log_syntax)]

#[macro_use] extern crate log;
extern crate env_logger;
#[macro_use] extern crate lazy_static;
extern crate test;
extern crate clap;
extern crate strfmt;


pub mod vm_optimizer;
pub mod error;
pub mod lexer;
pub mod bracket_tree;
pub mod syntax_tree;
pub mod compiler;
pub mod page;
pub mod types;

use std::fs::File;
use std::io::Read;
use std::io::Write as IOWrite;
use std::fmt::Write;
use std::collections::HashMap;

use clap::{
    Arg,
    App,
};

use strfmt::strfmt;

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
    DebugInfo,
    Value,
};
use vm_optimizer::Optimizer;
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
        ).arg(
            Arg::with_name("TEMPLATE")
                .short("t")
                .value_name("TEMPLATE")
                .default_value("vm/index.html")
                .help("Template to generate result")
        ).get_matches();

    // get arguments
    let input = matches.value_of("INPUT").unwrap();
    let output = matches.value_of("OUTPUT").unwrap();
    let template = matches.value_of("TEMPLATE").unwrap();

    // read file content
    let mut input = File::open(input).unwrap();
    let mut code = String::new();
    input.read_to_string(&mut code).unwrap();

    let line_starts: Vec<usize> = [0 as usize].iter()
        .cloned()
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
    let (mut code, debug_info) = match Compiler::new().compile(&syntax_tree){
        Ok(res) => res,
        Err(errors) => {
            Error::print_errors(&errors, &line_starts);
            panic!("Execution aborted due to present error.")
        }
    };

    Optimizer::new(&code).optimize(&mut code);
    
    // get template
    let mut template_file = File::open(template).unwrap();
    let mut template = String::new();
    template_file.read_to_string(&mut template).unwrap();

    let mut vars = HashMap::new();
    vars.insert("code_html".to_string(), page_string(&syntax_tree, &debug_info));
    vars.insert("code_js".to_string(), js_string(&code, &debug_info));
    let res = strfmt(&template, &vars).unwrap();

    let mut out = File::create(output).unwrap();
    write!(out, "{}", res);
    out.sync_all().unwrap();
}


fn page_string<'a>(syntax_tree: &[Root<'a>], debug_info: &DebugInfo)->String{
    let mut res = String::new();
    let mut context = Context::new();
    context.set_debug_info(debug_info);
    for r in syntax_tree{
        r.write_page(&mut res, &mut context).unwrap();
    }
    res
}

fn js_string(code: &[Operation], debug_info: &DebugInfo)->String{
    let mut res = String::new();
    write_compiled_to_js(&mut res, code, debug_info).unwrap();
    res
}


fn write_compiled_to_js<T: Write>(writer: &mut T, code: &[Operation],debug_info: &DebugInfo)->std::fmt::Result{
    write!(writer, "var compiled = {{commands: [")?;
    let mut iter = code.iter();
    if let Some(op) = iter.next(){
        write_operation_to_js(writer, op)?;
    }
    for op in iter{
        write!(writer, ",")?;
        write_operation_to_js(writer, op)?;
    }
    write!(writer, "], function_enters: [")?;
    let mut iter = debug_info.get_function_entry_points().iter();
    if let Some((fid, entry)) = iter.next(){
        write!(writer, "[{}, {}]", fid, entry)?;
    }
    for (fid, entry) in iter {
        write!(writer, ",[{}, {}]", fid, entry)?;
    }
    write!(writer, "], function_links: [")?;
    let mut iter = debug_info.get_function_links().iter();
    if let Some((fid, entry)) = iter.next(){
        write!(writer, "[{}, '{}']", fid, entry)?;
    }
    for (fid, entry) in iter {
        write!(writer, ",[{}, '{}']", fid, entry)?;
    }
    write!(writer, "], statements: [")?;
    let mut iter = debug_info.statements_offsets().iter();
    if let Some(i) = iter.next(){
        write!(writer, "{}", i)?;
    }
    for i in iter{
        write!(writer, ", {}", i)?;
    }

    write!(writer, "], variable_transactions: [")?;
    let mut iter = debug_info.get_variable_transactions().iter();
    if let Some(i) = iter.next(){
        write!(writer, "{{name: '{}', add: {}, pos: {}, id: {}, pair: {}}}", i.name, i.add, i.pos, i.var_id, i.pair)?;
    }
    for i in iter{
        write!(writer, ", {{name: '{}', add: {}, pos: {}, id: {}, pair: {}}}", i.name, i.add, i.pos, i.var_id, i.pair)?;
    }
    write!(writer, "]}}")?;
    Ok(())
}

/// Write a single `Operation` into javascript representation of vm bytecode
fn write_operation_to_js<T: Write>(writer: &mut T, operation: &Operation)->std::fmt::Result{
    write!(writer, "[{}", operation.code as usize)?;
    for arg in &operation.args{
        write!(writer, ", {}", arg)?;
    }
    match operation.value{
        Some(Value::Int(i)) => {
            write!(writer, ", {}", i)?;
        },
        Some(Value::Float(f)) => {
            write!(writer, ", {}", f)?;
        },
        _ => {}
    }
    write!(writer, "]")
}
