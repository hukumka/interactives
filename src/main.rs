#![feature(test)]
#![feature(box_patterns)]
#![feature(log_syntax)]

#[macro_use] extern crate log;
extern crate env_logger;
#[macro_use] extern crate lazy_static;
extern crate test;
extern crate clap;
extern crate strfmt;


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

    // get template
    let mut template_file = File::open(template).unwrap();
    let mut template = String::new();
    template_file.read_to_string(&mut template).unwrap();

    let mut vars = HashMap::new();
    vars.insert("code_html".to_string(), page_string(&syntax_tree, &compiler));
    vars.insert("code_js".to_string(), js_string(&compiler, &function_entry_points));
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

fn js_string(compiler: &Compiler, function_entry_points: &[(usize, usize)])->String{
    let mut res = String::new();
    write_compiled_to_js(&mut res, &compiler, function_entry_points).unwrap();
    res
}


fn write_compiled_to_js<'a, T: Write>(writer: &mut T, compiler: &Compiler<'a>, function_entry_points: &[(usize, usize)])->std::fmt::Result{
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
        write!(writer, "{{name: '{}', add: {}, pos: {}, id: {}}}", i.name, i.add, i.pos, i.var_id)?;
    }
    for i in iter{
        write!(writer, ", {{name: '{}', add: {}, pos: {}, id: {}}}", i.name, i.add, i.pos, i.var_id)?;
    }
    write!(writer, "]}}")?;
    Ok(())
}

fn write_operation_to_js<T: Write>(writer: &mut T, operation: &Operation)->std::fmt::Result{
    write!(writer, "[{}", operation.code as usize)?;
    for arg in &operation.args{
        write!(writer, ", {}", arg)?;
    }
    write!(writer, "]")
}
