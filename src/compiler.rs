use std::collections::HashMap;

use lexer::TokenData;
use syntax_tree::{
    Root,
    VariableDefinition,
    Variable,
    FunctionDefinition,
    FunctionCall,
    Block,
    Statement,
    ForLoop,
    ForLoopInitialization,
    Condition,
    Expression,
    ExpressionData,
    BinaryOperator,
    SyffixOperator,
    PrefixOperator,
    Index
};
use error::Error;
use error::compiler::*;
use types::{
    Type,
    FunctionType
};


#[derive(Debug)]
pub enum Code {
    Jump,
    JumpZ,
    JumpNZ,

    Eq,
    NotEq,
    Less,
    LessOrEq,

    Sum,
    Diff,
    Mul,
    DivI,
    Mod,

    Dereference,
    PointerSet,

    Nop,
    Clone,
    ConstInt,
    PointerToLocal,

    Return,
    Call,
}

#[derive(Debug)]
struct Operation{
    code: Code,
    args: Vec<usize>,
}


struct VariableManager<'a>{
    namespaces: Vec<HashMap<&'a str, usize>>,
    types: Vec<Type>,
    definitions: Vec<&'a Variable<'a>>
}

impl<'a> VariableManager<'a>{
    fn new()->Self{
        Self{
            namespaces: vec![],
            types: vec![],
            definitions: vec![]
        }
    }

    fn push(&mut self){
        self.namespaces.push(HashMap::new())
    }

    fn pop(&mut self)->Option<()>{
        self.namespaces.pop().map(|_| ())
    }

    fn define_variable(&mut self, var: &'a Variable<'a>)->Result<usize, Error<'a>>{
        let id = self.definitions.len();
        let name = var.name.token_str();
        let type_ = Type::from_type(&var.type_).or_else(|x|{
            Err(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, x.base))
        })?;
        let top = self.namespaces.last_mut().unwrap();
        if top.get(name).is_some(){
            Err(Error::from_token(
                ERROR_COMPILER_VARIABLE_REDEFINED,
                var.name
            ))
        }else{
            top.insert(name, id);
            self.types.push(type_);
            self.definitions.push(var);
            Ok(id)
        }
    }

    fn get_id(&mut self, name: &'a str)->Option<usize>{
        for n in &self.namespaces{
            if let Some(id) = n.get(name){
                return Some(*id);
            }
        }
        None
    }

    fn len(&self)->usize{
        self.namespaces.iter().map(|x| x.len()).sum()
    }
}


struct FunctionManager<'a>{
    map: HashMap<&'a str, usize>,
    types: Vec<FunctionType>,
}

impl<'a> FunctionManager<'a>{
    fn new()->Self{
        Self{
            map: HashMap::new(),
            types: vec![]
        }
    }

    fn register_function_definition(&mut self, func: &'a FunctionDefinition<'a>)->Result<usize, Error<'a>>{
        let id = self.types.len();
        let name = func.name.token_str();
        if self.map.get(name).is_none(){
            let type_ = FunctionType::from_definition(func).or_else(|t|{
                Err(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, t.base))
            })?;
            self.types.push(type_);
            self.map.insert(name, id);
            Ok(id)
        }else{
            Err(Error::from_token(ERROR_COMPILER_FUNCTION_REDEFINED, func.name))
        }
    }

    fn get_function_id(&mut self, name: &'a str)->Option<usize>{
        self.map.get(name).map(|x| *x)
    }
}


pub struct Compiler<'a>{
    operations: Vec<Operation>,
    variables: VariableManager<'a>,
    temp_values: usize,
    current_function: Option<&'a FunctionDefinition<'a>>,
    functions: FunctionManager<'a>,
    errors: Vec<Error<'a>>
}


impl<'a> Compiler<'a>{
    pub fn new()->Self{
        Self{
            operations: vec![],
            variables: VariableManager::new(),
            functions: FunctionManager::new(),
            temp_values: 0,
            current_function: None,
            errors: vec![],
        }
    }

    pub fn print(&self){
        for op in &self.operations{
            println!("{:?}", op);
        }
    }

    pub fn errors(&self)->&[Error<'a>]{
        self.errors.as_slice()
    }

    pub fn compile_function(&mut self, func: &'a FunctionDefinition<'a>)->Option<usize>{
        self.current_function = Some(func);
        let start = self.operations.len();
        self.variables.push();
        let arguments_parsed = func.arguments.iter()
            .map(|v| {
                if let Err(x) = self.variables.define_variable(v){
                    self.errors.push(x);
                    false
                }else{
                    true
                }
            }).fold(true, |a, b| a && b);
        if arguments_parsed{
            self.compile_block(&func.body)?;
            self.current_function = None;
            Some(start)
        }else{
            self.current_function = None;
            None
        }
    }

    fn compile_block(&mut self, block: &'a Block<'a>) ->Option<()>{
        block.statements.iter()
            .map(|s| self.compile_statement(s))
            .fold(Some(()), |a, b| a.and(b))
    }

    fn compile_statement(&mut self, statement: &'a Statement<'a>)->Option<()>{
        match statement{
            Statement::Expression(e) => {
                let res = self.compile_expression(&e).map(|_| ());
                self.take_latest_expr();
                res
            },
            Statement::Return(e) => self.compile_return(&e),
            Statement::VariableDefinition(v) => self.compile_variable_definition(&v),
            Statement::ForLoop(l) => self.compile_for_loop(&l),
            Statement::Condition(c) => self.compile_condition(&c)
        }
    }

    fn compile_return(&mut self, ret: &'a Expression<'a>)->Option<()>{
        let required_type = Type::from_type(&self.current_function.unwrap().return_type).or_else(|e|{
            self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, e.base));
            Err(e)
        }).ok()?;
        self.compile_expression_of_type(ret, required_type)?;
        let id = self.take_latest_expr();
        self.operations.push(Operation{
            code: Code::Return,
            args: vec![id]
        });
        Some(())
    }

    fn compile_variable_definition(&mut self, var: &'a VariableDefinition<'a>)->Option<()>{
        let id = self.variables.define_variable(&var.variable).ok()?;
        let required_type = Type::from_type(&var.variable.type_).or_else(|e|{
            self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, e.base));
            Err(e)
        }).ok()?;
        let type_ = self.compile_expression_of_type(&var.set_to, required_type)?;
        let from = self.take_latest_expr();
        self.operations.push(Operation{
            code: Code::Clone,
            args: vec![from, id]
        });
        Some(())
    }

    fn compile_for_loop(&mut self, for_loop: &'a ForLoop<'a>)->Option<()>{
        self.variables.push();
        let succ = match &for_loop.init{
            ForLoopInitialization::VariableDefinition(v) => self.compile_variable_definition(&v),
            ForLoopInitialization::Expression(e) => {
                let res = self.compile_expression(&e);
                self.take_latest_expr();
                res.map(|_| ())
            }
        };
        let loop_start = self.operations.len();
        let succ = succ.and(self.compile_expression_of_type(&for_loop.condition, Type::int()));
        let jump_op = self.operations.len();
        let id = self.take_latest_expr();
        self.operations.push(Operation {
            code: Code::JumpZ,
            args: vec![0, id]
        });
        let succ = succ.and(self.compile_block(&for_loop.body));
        let succ = succ.and(self.compile_expression_of_type(&for_loop.step, Type::int()));
        self.take_latest_expr();
        self.operations.push(Operation{
            code: Code::Jump,
            args: vec![loop_start]
        });
        self.operations[jump_op].args[0] = self.operations.len(); // set jump address to after loop
        self.variables.pop().unwrap();
        succ
    }

    fn compile_condition(&mut self, cond: &'a Condition<'a>)->Option<()>{
        let mut succ = self.compile_expression_of_type(&cond.condition, Type::int());
        let jump_op = self.operations.len();
        let id = self.take_latest_expr();
        self.operations.push(Operation{
            code: Code::JumpZ,
            args: vec![0, id]
        });
        succ = succ.and(self.compile_block(&cond.then));
        if let Some(ref else_) = cond.else_ {
            let second_jump_op = self.operations.len();
            self.operations.push(Operation {
                code: Code::Jump,
                args: vec![0]
            });
            self.operations[jump_op].args[0] = self.operations.len(); // set jump address to after begining of else block
            succ = succ.and(self.compile_block(else_));
            self.operations[second_jump_op].args[0] = self.operations.len(); // set jump to end of
        }
        succ
    }

    fn compile_expression(&mut self, expr: &'a Expression<'a>)->Option<(Type, bool)>{
        match expr.0.as_ref(){
            ExpressionData::Variable(v) => self.compile_expression_variable(&v),
            ExpressionData::Constant(c) => self.compile_expression_constant(&c),
            ExpressionData::Index(i) => self.compile_expression_index(&i),
            ExpressionData::FunctionCall(f) => self.compile_expression_function_call(&f),
            ExpressionData::BinaryOperator(op) => self.compile_expression_binary_op(&op),
            ExpressionData::SyffixOperator(op) => self.compile_expression_syffix_op(&op),
            ExpressionData::PrefixOperator(op) => self.compile_expression_prefix_op(&op),
        }
    }

    fn compile_expression_syffix_op(&mut self, expr: &'a SyffixOperator<'a>)->Option<(Type, bool)>{
        None
    }

    fn compile_expression_prefix_op(&mut self, expr: &'a PrefixOperator<'a>)->Option<(Type, bool)>{
        None
    }

    fn compile_expression_variable(&mut self, var: &'a TokenData<'a>)->Option<(Type, bool)>{
        let var_id = self.variables.get_id(var.token_str())?;
        let type_ = self.variables.types[var_id];
        let id = self.put_expr();
        self.operations.push(Operation{
            code: Code::PointerToLocal,
            args: vec![var_id, id]
        });
        Some((type_, true))
    }

    fn compile_expression_constant(&mut self, c: &'a TokenData<'a>)->Option<(Type, bool)>{
        let i = c.token_str().parse::<usize>().ok()?;
        let id = self.put_expr();
        self.operations.push(Operation{
            code: Code::ConstInt,
            args: vec![i, id]
        });
        Some((Type::int(), false))
    }

    fn compile_expression_index(&mut self, i: &'a Index<'a>)->Option<(Type, bool)>{
        let pointer_type = self.compile_expression_rvalue(&i.expr)?;
        let index_type = self.compile_expression_rvalue(&i.index)?;
        if index_type != Type::int(){
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, i.index.token()));
            None
        }else if let Some(type_) = pointer_type.dereference(){
            let index = self.take_latest_expr();
            let pointer = self.take_latest_expr();
            let id = self.put_expr();
            self.operations.push(Operation{
                code: Code::Sum,
                args: vec![id, pointer, index]
            });
            Some((pointer_type.dereference().unwrap(), true))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, i.expr.token()));
            None
        }
    }

    fn compile_expression_function_call(&mut self, func: &'a FunctionCall<'a>)->Option<(Type, bool)>{
        let func_id = self.functions.get_function_id(func.name.token_str()).or_else(||{
            self.errors.push(Error::from_token(ERROR_COMPILER_UNDEFINED_FUNCTION, func.name));
            None
        })?;
        let type_ = self.functions.types[func_id].clone();
        if func.arguments.len() == type_.args.len(){
            let res = func.arguments.iter()
                .zip(&type_.args)
                .map(|(arg, type_)| self.compile_expression_of_type(arg, *type_))
                .fold(Some(()), |a, b| a.and(b));
            self.temp_values -= func.arguments.len();
            let id = self.put_expr();
            self.operations.push(Operation{
                code: Code::Call,
                args: vec![id]
            });
            res.map(|_| (type_.ret, false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_ARGUMENTS_COUNT, func.name));
            None
        }
    }

    fn compile_expression_binary_op(&mut self, operator: &'a BinaryOperator<'a>)->Option<(Type, bool)>{
        let op = operator.operator.token_str();
        if op == "||"{
            self.compile_expression_of_type(&operator.left, Type::int())?;
            let jump_id = self.operations.len();
            let id = self.take_latest_expr();
            self.operations.push(Operation {
                code: Code::JumpNZ,
                args: vec![0, id]
            });
            self.compile_expression_of_type(&operator.right, Type::int())?;
            self.operations[jump_id].args[0] = self.operations.len();
            Some((Type::int(), false))
        }else if op == "&&" {
            self.compile_expression_of_type(&operator.left, Type::int())?;
            let jump_id = self.operations.len();
            let id = self.take_latest_expr();
            self.operations.push(Operation {
                code: Code::JumpZ,
                args: vec![0, id]
            });
            self.compile_expression_of_type(&operator.right, Type::int())?;
            self.operations[jump_id].args[0] = self.operations.len();
            Some((Type::int(), false))
        }else if op == "="{
            let (type_, is_reference) = self.compile_expression(&operator.left)?;
            if is_reference{
                let right = self.compile_expression_of_type(&operator.right, type_)?;
                let r = self.take_latest_expr();
                let l = self.take_latest_expr();
                self.operations.push(Operation{
                    code: Code::PointerSet,
                    args: vec![l, r]
                });
                Some((type_, true))
            }else{
                self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, operator.operator));
                None
            }
        }else{
            let left_type = self.compile_expression_rvalue(&operator.left)?;
            let right_type = self.compile_expression_rvalue(&operator.right)?;
            match op{
                "+" => self.compile_operator_plus(operator.operator, left_type, right_type),
                "-" => self.compile_operator_minus(operator.operator, left_type, right_type),
                "*" => self.compile_operator_mul(operator.operator, left_type, right_type),
                "/" => self.compile_operator_div(operator.operator, left_type, right_type),
                "%" => self.compile_operator_mod(operator.operator, left_type, right_type),
                _ => {
                    self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_OPERATOR, operator.operator));
                    None
                }
            }
        }
    }

    fn compile_operator_plus(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l.pointer_count > 0 && r == Type::int() {
            self.operations.push(Operation {
                code: Code::Sum,
                args: vec![res, left, right]
            });
            Some((l.clone(), false))
        }else if l == Type::int() && r.pointer_count > 0 {
            self.operations.push(Operation {
                code: Code::Sum,
                args: vec![res, left, right]
            });
            Some((r.clone(), false))
        }else if l == Type::int() && r == Type::int(){
            self.operations.push(Operation {
                code: Code::Sum,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_operator_minus(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l.pointer_count > 0 && r == Type::int() {
            self.operations.push(Operation {
                code: Code::Diff,
                args: vec![res, left, right]
            });
            Some((l.clone(), false))
        }else if l.pointer_count > 0 && l == r {
            self.operations.push(Operation {
                code: Code::Diff,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else if l == Type::int() && r == Type::int(){
            self.operations.push(Operation {
                code: Code::Diff,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_operator_mul(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == Type::int() && r == Type::int(){
            self.operations.push(Operation {
                code: Code::Mul,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_operator_div(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == Type::int() && r == Type::int(){
            self.operations.push(Operation {
                code: Code::DivI,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_operator_mod(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == Type::int() && r == Type::int(){
            self.operations.push(Operation {
                code: Code::Mod,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_expression_rvalue(&mut self, expr: &'a Expression<'a>)->Option<Type>{
        let (type_, is_rvalue) = self.compile_expression(expr)?;
        let id = self.take_latest_expr();
        if is_rvalue{
            self.operations.push(Operation{
                code: Code::Dereference,
                args: vec![id, id]
            });
        }
        self.temp_values += 1;
        Some(type_)
    }

    fn compile_expression_of_type(&mut self, expr: &'a Expression<'a>, required_type: Type)->Option<()>{
        let type_ = self.compile_expression_rvalue(expr)?;
        let id = self.take_latest_expr();
        self.compile_convert_type(expr.token(), type_, required_type, id)?;
        self.put_expr();
        Some(())
    }

    fn compile_convert_type(&mut self, token: &'a TokenData<'a>, from: Type, into: Type, id: usize)->Option<()>{
        if from == into{
            Some(())
        }else if from.pointer_count>0 && into == Type::int(){
            Some(())
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn put_expr(&mut self)->usize{
        self.temp_values += 1;
        self.temp_values + self.variables.len() - 1
    }

    fn take_latest_expr(&mut self)->usize{
        if self.temp_values > 0 {
            self.temp_values -= 1;
        }else{
            warn!("Warning: Attempt to take expression value then none exists.")
        }
        self.temp_values + self.variables.len()
    }
}
