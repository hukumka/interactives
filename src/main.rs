#![feature(test)]
#![feature(box_patterns)]
#![feature(log_syntax)]

#[macro_use] extern crate log;
extern crate env_logger;
#[macro_use] extern crate lazy_static;
extern crate test;
extern crate clap;
extern crate strfmt;


mod vm_optimizer;
mod error;
mod lexer;
mod bracket_tree;
mod syntax_tree;
mod compiler;
mod page;
mod types;

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
    Value,
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
    let mut res = true;
    let mut errors = vec![];
    for r in &syntax_tree{
        match r{
            Root::VariableDefinition(_) => {
                unimplemented!("Global variable yet to be supported")
            },
            Root::FunctionDefinition(x) => {
                match compiler.register_function_definition(x){
                    Ok(_) => {},
                    Err(e) => {
                        res = false;
                        errors.push(e);
                    }
                }
            }
            Root::FunctionDeclaration(x) => {
                match compiler.register_function_declaration(x){
                    Ok(_) => {},
                    Err(e) => {
                        res = false;
                        errors.push(e);
                    }
                }
            }
        }
    }

    if !res{
        Error::print_errors(&errors, &line_starts);
        return;
    }

    let res: Option<Vec<_>> = {
        syntax_tree.iter()
            .filter_map(|x|{
                match x{
                    Root::FunctionDefinition(x) => Some(x),
                    _ => None
                }
            })
            .map(|x| compiler.compile_function(x))
            .collect()
    };

    let res = if let Some(res) = res{
        res
    }else{
        Error::print_errors(compiler.errors(), &line_starts);
        return;
    };

    let mut function_entry_points = vec![];
    let mut function_links = vec![];
    let mut func_def_id = 0;
    for r in &syntax_tree{
        match r{
            Root::VariableDefinition(_) => {},
            Root::FunctionDefinition(func) => {
                let id = compiler.get_func_id(func.name.token_str()).unwrap();
                function_entry_points.push((id, res[func_def_id]));
                func_def_id += 1;
            },
            Root::FunctionDeclaration(func) => {
                let id = compiler.get_func_id(func.name.token_str()).unwrap();
                function_links.push((id, func.name.token_str().to_string()));
            }
        }
    }

    // get template
    let mut template_file = File::open(template).unwrap();
    let mut template = String::new();
    template_file.read_to_string(&mut template).unwrap();

    let mut vars = HashMap::new();
    vars.insert("code_html".to_string(), page_string(&syntax_tree, &compiler));
    vars.insert("code_js".to_string(), js_string(&compiler, &function_entry_points, &function_links));
    let res = strfmt(&template, &vars).unwrap();

    let mut out = File::create(output).unwrap();
    write!(out, "{}", res);
    out.sync_all().unwrap();
}


fn page_string<'a>(syntax_tree: &[Root<'a>], compiler: &Compiler)->String{
    let mut res = String::new();
    let mut context = Context::new();
    context.set_debug_info(compiler.get_debug_info());
    for r in syntax_tree{
        r.write_page(&mut res, &mut context).unwrap();
    }
    res
}

fn js_string(compiler: &Compiler, function_entry_points: &[(usize, usize)], function_links: &[(usize, String)])->String{
    let mut res = String::new();
    write_compiled_to_js(&mut res, &compiler, function_entry_points, function_links).unwrap();
    res
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
fn write_compiled_to_js<'a, T: Write>(
    writer: &mut T, 
    compiler: &Compiler<'a>, 
    function_entry_points: &[(usize, usize)], 
    function_links: &[(usize, String)], 
)->std::fmt::Result{
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
    write!(writer, "], function_links: [")?;
    let mut iter = function_links.iter();
    if let Some((fid, entry)) = iter.next(){
        write!(writer, "[{}, '{}']", fid, entry)?;
    }
    for (fid, entry) in iter {
        write!(writer, ",[{}, '{}']", fid, entry)?;
    }
    write!(writer, "], statements: [")?;
    let mut iter = compiler.get_debug_info().statements_offsets().iter();
    if let Some(i) = iter.next(){
        write!(writer, "{}", i)?;
    }
    for i in iter{
        write!(writer, ", {}", i)?;
    }

    write!(writer, "], variable_transactions: [")?;
    let mut iter = compiler.get_debug_info().get_variable_transactions().iter();
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
