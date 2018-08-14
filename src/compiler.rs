use std::collections::HashMap;

use lexer::TokenData;
use syntax_tree::{
    Root,
    VariableDefinition,
    Variable,
    FunctionDefinition,
    FunctionDeclaration,
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
    Index,

    TreeItem
};
use error::Error;
use error::compiler::*;
use types::{
    Type,
    FunctionType
};


#[derive(Debug, Copy, Clone)]
pub enum Code {
    /// Unconditional jump to args[0]
    Jump,
    /// Jump to args[0] if value in cell args[1] == 0
    JumpZ,
    /// Jump to args[0] if values in cell args[1] != 0
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

    Inc,
    Dec,

    Dereference,
    PointerSet,

    Nop,
    Clone,
    ConstInt,
    ConstFloat,
    PointerToLocal,

    Return,
    Call,

    AddSp,
    SubSb,

    Div,
    FloatToInt,
}

#[derive(Debug)]
pub struct Operation{
    pub code: Code,
    pub args: Vec<usize>,
    pub value: Option<Value>
}


#[derive(Debug)]
pub enum Value{
    Int(i32),
    Float(f32),
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
            definitions: vec![],
        }
    }

    fn push(&mut self){
        self.namespaces.push(HashMap::new());
    }

    fn pop(&mut self)->Option<()>{
        self.namespaces.pop().map(|_| ())
    }

    fn alloc_empty(&mut self){
        self.namespaces.clear();
        self.types.clear();
        self.definitions.clear();
    }

    fn define_variable(&mut self, var: &'a Variable<'a>)->Result<usize, Error<'a>>{
        let id = self.definitions.len() + 1;
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

    fn get_type(&self, id: usize)->Type{
        self.types[id-1].clone()
    }

    fn len(&self)->usize{
        self.namespaces.iter().map(|x| x.len()).sum::<usize>() + 1
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

    fn register_function_declaration(&mut self, func: &'a FunctionDeclaration<'a>)->Result<usize, Error<'a>>{
        let id = self.types.len();
        let name = func.name.token_str();
        if self.map.get(name).is_none(){
            let type_ = FunctionType::from_declaration(func).or_else(|t|{
                Err(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, t.base))
            })?;
            self.types.push(type_);
            self.map.insert(name, id);
            Ok(id)
        }else{
            Err(Error::from_token(ERROR_COMPILER_FUNCTION_REDEFINED, func.name))
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

    fn get_function_id(&self, name: &'a str)->Option<usize>{
        self.map.get(name).map(|x| *x)
    }
}


#[derive(Debug)]
pub struct DebugInfo{
    /// Map from offset of variable reference to its offset in stack
    variables: HashMap<usize, (usize, usize)>,
    /// Map from offset of statement to its address in VM code
    statements: HashMap<usize, usize>,
    statements_by_id: Vec<usize>,
    variables_transactions: Vec<VariableTransaction>,
    variables_transactions_stack: Vec<usize>,

    /// function offset table. Contains pairs (function_id, offset, arg_count)
    functions: Vec<(usize, usize)>,
    /// function links. Contains pairs (function_id, function_name, arg_count)
    function_links: Vec<(usize, String)>,
}


#[derive(Debug)]
pub struct VariableTransaction{
    pub pos: usize,
    pub name: String,
    pub var_id: usize,
    pub add: bool,
    pub pair: usize,
}


impl DebugInfo{
    fn new()->Self{
        Self{
            variables: HashMap::new(),
            statements: HashMap::new(),
            statements_by_id: vec![],
            variables_transactions: vec![],
            variables_transactions_stack: vec![],
            functions: vec![],
            function_links: vec![],
        }
    }

    pub fn get_function_entry_points(&self)->&[(usize, usize)]{
        self.functions.as_slice()
    }
    pub fn get_function_links(&self)->&[(usize, String)]{
        self.function_links.as_slice()
    }

    pub fn get_variable_transactions(&self)->&[VariableTransaction]{
        self.variables_transactions.as_slice()
    }

    fn make_stack_floor(&self)->usize{
        self.variables_transactions_stack.len()
    }

    fn recover_to_stack_floor(&mut self, code_offset: usize, floor: usize){
        let count = self.variables_transactions_stack.len() - floor;
        for _ in 0..count{
            self.pop_variable(code_offset);
        }
    }

    fn reg_variable_offset<'a>(&mut self, name: &'a TokenData<'a>, stack_offset: usize){
        let tr_id = self.variables_transactions_stack[stack_offset-1];
        self.variables.insert(name.get_pos(), (stack_offset, tr_id));
    }

    fn push_variable<'a>(&mut self, name: &'a TokenData<'a>, stack_offset: usize, code_offset: usize){
        self.variables_transactions_stack.push(self.variables_transactions.len());
        self.variables_transactions.push(VariableTransaction{
            pos: code_offset,
            name: name.token_str().to_string(),
            add: true,
            var_id: self.variables_transactions_stack.len(),
            pair: 0,
        });
        self.reg_variable_offset(name, stack_offset);
    }

    fn pop_variable(&mut self, code_offset: usize){
        if let Some(x) = self.variables_transactions_stack.pop(){
            let name = self.variables_transactions[x].name.clone();
            let var_id = self.variables_transactions[x].var_id;
            assert!(self.variables_transactions[x].add);
            self.variables_transactions[x].pair = self.variables_transactions.len();
            self.variables_transactions.push(VariableTransaction{
                pos: code_offset,
                name,
                add: false,
                var_id,
                pair: x,
            });
        }
    }

    fn reg_statement_offset<'a>(&mut self, statement: &'a Statement<'a>, vm_offset: usize){
        self.statements_by_id.push(vm_offset);
        self.statements.insert(statement.first_token().get_pos(), vm_offset);
    }

    pub fn get_variable_data<'a>(&self, name: &'a TokenData<'a>)->Option<(usize, usize)>{
        self.variables.get(&name.get_pos()).map(|x| x.clone())
    }

    pub fn get_statement_offset<'a>(&self, statement: &'a Statement<'a>)->Option<usize>{
        self.statements.get(&statement.first_token().get_pos()).map(|x| *x)
    }

    pub fn get_statement_offset_by_pos<'a>(&self, pos: usize)->Option<usize>{
        self.statements.get(&pos).map(|x| *x)
    }

    pub fn statements_offsets(&self)->&[usize]{
        self.statements_by_id.as_slice()
    }
}


pub struct Compiler<'a>{
    operations: Vec<Operation>,
    variables: VariableManager<'a>,
    temp_values: usize,
    current_function: Option<&'a FunctionDefinition<'a>>,
    functions: FunctionManager<'a>,
    debug_info: DebugInfo,
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
            debug_info: DebugInfo::new(),
            errors: vec![],
        }
    }

    pub fn compile(mut self, tree: &'a [Root<'a>])->Result<(Vec<Operation>, DebugInfo), Vec<Error<'a>>>{
        // define all functions
        for t in tree{
            match t{
                Root::FunctionDefinition(def) => {
                    if let Err(e) = self.functions.register_function_definition(def){
                        self.errors.push(e);
                    }
                },
                Root::FunctionDeclaration(decl) => {
                    match self.functions.register_function_declaration(decl){
                        Ok(i) => {
                            self.debug_info.function_links.push((i, decl.name.token_str().to_string()));
                        },
                        Err(e) => {
                            self.errors.push(e);
                        }
                    }
                },
                _ => {}
            }
        }
        // compile all functions
        for t in tree{
            match t{
                Root::FunctionDefinition(def) => {
                    let _ = self.compile_function(def);
                },
                _ => {}
            }
        }
        if self.errors.is_empty(){
            Ok((self.operations, self.debug_info))
        }else{
            Err(self.errors)
        }
    }

    pub fn compile_function(&mut self, func: &'a FunctionDefinition<'a>)->Option<usize>{
        let function_id = self.functions.get_function_id(func.name.token_str())?;
        self.debug_info.functions.push((function_id, self.operations.len()));

        self.current_function = Some(func);
        let start = self.operations.len();
        self.variables.alloc_empty();
        self.variables.push();
        self.temp_values = 0;
        let arguments_parsed = func.arguments.iter()
            .map(|v| {
                match self.variables.define_variable(v){
                    Ok(x) => {
                        self.debug_info.push_variable(v.name, x, self.operations.len());
                        true
                    },
                    Err(x) => {
                        self.errors.push(x);
                        false
                    }
                }
            }).fold(true, |a, b| a && b);
        if arguments_parsed{
            self.compile_block(&func.body)?;
            self.operations.push(Operation{
                value: None,
                code: Code::Return,
                args: vec![]
            });
            self.current_function = None;
            self.variables.pop().unwrap();
            // release debug info
            for _ in &func.arguments{
                self.debug_info.pop_variable(self.operations.len());
            }
            Some(start)
        }else{
            self.current_function = None;
            self.variables.pop().unwrap();
            for _ in &func.arguments{
                self.debug_info.pop_variable(self.operations.len());
            }
            None
        }
    }

    fn compile_block(&mut self, block: &'a Block<'a>) ->Option<()>{
        let stack_frame = self.debug_info.make_stack_floor();
        let res = block.statements.iter()
            .map(|s| self.compile_statement(s))
            .fold(Some(()), |a, b| a.and(b));
        self.debug_info.recover_to_stack_floor(self.operations.len(), stack_frame);
        res
    }

    fn compile_statement(&mut self, statement: &'a Statement<'a>)->Option<()>{
        match statement{
            Statement::Expression(e) => {
                self.debug_info.reg_statement_offset(statement, self.operations.len());
                let res = self.compile_expression(&e).map(|_| ());
                self.take_latest_expr();
                res
            },
            Statement::Return(e) => {
                self.debug_info.reg_statement_offset(statement, self.operations.len());
                self.compile_return(&e.expression)
            },
            Statement::VariableDefinition(v) => {
                self.debug_info.reg_statement_offset(statement, self.operations.len());
                self.compile_variable_definition(&v)
            },
            Statement::ForLoop(l) => self.compile_for_loop(&l, statement),
            Statement::Condition(c) => {
                self.debug_info.reg_statement_offset(statement, self.operations.len());
                self.compile_condition(&c)
            }
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
            value: None,
            code: Code::Clone,
            args: vec![0, id]
        });
        self.operations.push(Operation{
            value: None,
            code: Code::Return,
            args: vec![]
        });
        Some(())
    }

    fn compile_variable_definition(&mut self, var: &'a VariableDefinition<'a>)->Option<()>{
        let id = self.variables.define_variable(&var.variable).ok()?;
        let required_type = Type::from_type(&var.variable.type_).or_else(|e|{
            self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, e.base));
            Err(e)
        }).ok()?;
        let _type_ = self.compile_expression_of_type(&var.set_to, required_type)?;
        let from = self.take_latest_expr();
        self.operations.push(Operation{
            value: None,
            code: Code::Clone,
            args: vec![id, from]
        });
        self.debug_info.push_variable(var.name(), id, self.operations.len());
        Some(())
    }

    fn compile_for_loop(&mut self, for_loop: &'a ForLoop<'a>, statement: &'a Statement<'a>)->Option<()>{
        let stack_floor = self.debug_info.make_stack_floor();
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
        // for loop has breakpoint at condition check
        self.debug_info.reg_statement_offset(statement, self.operations.len());
        let succ = succ.and(self.compile_expression_of_type(&for_loop.condition, Type::int()));
        let jump_op = self.operations.len();
        let id = self.take_latest_expr();
        self.operations.push(Operation {
            value: None,
            code: Code::JumpZ,
            args: vec![0, id]
        });
        let succ = succ.and(self.compile_block(&for_loop.body));
        let succ = succ.and(self.compile_expression_of_type(&for_loop.step, Type::int()));
        self.take_latest_expr();
        self.operations.push(Operation{
            value: None,
            code: Code::Jump,
            args: vec![loop_start]
        });
        self.operations[jump_op].args[0] = self.operations.len(); // set jump address to after loop
        self.variables.pop().unwrap();
        self.debug_info.recover_to_stack_floor(self.operations.len(), stack_floor);
        succ
    }

    fn compile_condition(&mut self, cond: &'a Condition<'a>)->Option<()>{
        let mut succ = self.compile_expression_of_type(&cond.condition, Type::int());
        let jump_op = self.operations.len();
        let id = self.take_latest_expr();
        self.operations.push(Operation{
            value: None,
            code: Code::JumpZ,
            args: vec![0, id]
        });
        succ = succ.and(self.compile_block(&cond.then));
        if let Some(ref else_) = cond.else_ {
            let second_jump_op = self.operations.len();
            self.operations.push(Operation {
                value: None,
                code: Code::Jump,
                args: vec![0]
            });
            self.operations[jump_op].args[0] = self.operations.len(); // set jump address to after begining of else block
            succ = succ.and(self.compile_block(else_));
            self.operations[second_jump_op].args[0] = self.operations.len(); // set jump to end of
        }else{
            self.operations[jump_op].args[0] = self.operations.len(); // set jump address to after begining of else block
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

    fn compile_expression_syffix_op(&mut self, op: &'a SyffixOperator<'a>)->Option<(Type, bool)>{
        let (type_, is_rvalue) = self.compile_expression(&op.left)?;
        if is_rvalue && type_ == Type::int() || type_.pointer_count > 0 {
            self.take_latest_expr();
            let id = self.put_expr();
            if op.operator.token_str() == "++" {
                self.operations.push(Operation {
                    value: None,
                    code: Code::Inc,
                    args: vec![id]
                });
                Some((type_, is_rvalue))
            }else if op.operator.token_str() == "--"{
                self.operations.push(Operation {
                    value: None,
                    code: Code::Dec,
                    args: vec![id]
                });
                Some((type_, is_rvalue))
            } else {
                self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_OPERATOR, op.operator));
                None
            }
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, op.operator));
            None
        }
    }

    fn compile_expression_prefix_op(&mut self, op: &'a PrefixOperator<'a>)->Option<(Type, bool)>{
        match op.operator.token_str(){
            x if x == "--" || x == "++" => {
                let (type_, is_rvalue) = self.compile_expression(&op.right)?;
                let id = self.take_latest_expr();
                let _res_id = self.put_expr();
                if is_rvalue && (type_ == Type::int() || type_.pointer_count > 0) {
                    if x == "--"{
                        self.operations.push(Operation {
                            value: None,
                            code: Code::Dec,
                            args: vec![id]
                        });
                    }else{
                        self.operations.push(Operation {
                            value: None,
                            code: Code::Inc,
                            args: vec![id]
                        });
                    }
                    Some((type_, is_rvalue))
                }else{
                    self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, op.operator));
                    None
                }
            },
            "&" => {
                let (type_, is_rvalue) = self.compile_expression(&op.right)?;
                let _id = self.take_latest_expr();
                let _res_id = self.put_expr();
                if is_rvalue{
                    Some((type_.pointer(), false))
                }else{
                    self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, op.operator));
                    None
                }
            },
            "*" => {
                let type_ = self.compile_expression_rvalue(&op.right)?;
                let _id = self.take_latest_expr();
                let _res_id = self.put_expr();
                if let Some(type_) = type_.dereference(){
                    Some((type_, true))
                }else{
                    self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, op.operator));
                    None
                }
            }
            _ => {
                self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_OPERATOR, op.operator));
                None
            }
        }
    }

    fn compile_expression_variable(&mut self, var: &'a TokenData<'a>)->Option<(Type, bool)>{
        let var_id = self.variables.get_id(var.token_str()).or_else(||{
            self.errors.push(Error::from_token(ERROR_COMPILER_UNDEFINED_VARIABLE, var));
            None
        })?;
        let type_ = self.variables.get_type(var_id);
        let id = self.put_expr();
        self.debug_info.reg_variable_offset(var, var_id);
        self.operations.push(Operation{
            value: None,
            code: Code::PointerToLocal,
            args: vec![id, var_id]
        });
        Some((type_, true))
    }

    fn compile_expression_constant(&mut self, c: &'a TokenData<'a>)->Option<(Type, bool)>{
        let id = self.put_expr();
        if let Some(i) = c.token_str().parse::<i32>().ok(){
            self.operations.push(Operation{
                code: Code::ConstInt,
                args: vec![id],
                value: Some(Value::Int(i))
            });
            Some((Type::int(), false))
        }else if let Some(f) = c.token_str().parse::<f32>().ok(){
            self.operations.push(Operation{
                code: Code::ConstFloat,
                args: vec![id],
                value: Some(Value::Float(f))
            });
            Some((Type::float(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_INVALID_CONSTANT, c));
            None
        }
    }

    fn compile_expression_index(&mut self, i: &'a Index<'a>)->Option<(Type, bool)>{
        let pointer_type = self.compile_expression_rvalue(&i.expr)?;
        let index_type = self.compile_expression_rvalue(&i.index)?;
        if index_type != Type::int(){
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, i.index.token()));
            None
        }else if let Some(_type_) = pointer_type.dereference(){
            let index = self.take_latest_expr();
            let pointer = self.take_latest_expr();
            let id = self.put_expr();
            self.operations.push(Operation{
                code: Code::Sum,
                args: vec![id, pointer, index],
                value: None,
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
            let sp_offset = self.put_expr(); // reserve space for result
            let res = func.arguments.iter()
                .zip(&type_.args)
                .map(|(arg, type_)| self.compile_expression_of_type(arg, type_.clone()))
                .fold(Some(()), |a, b| a.and(b));
            self.temp_values -= func.arguments.len();
            self.operations.push(Operation{
                code: Code::AddSp,
                args: vec![sp_offset],
                value: None,
            });
            self.operations.push(Operation{
                value: None,
                code: Code::Call,
                args: vec![func_id]
            });
            self.operations.push(Operation{
                value: None,
                code: Code::SubSb,
                args: vec![sp_offset]
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
                value: None,
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
                value: None,
                code: Code::JumpZ,
                args: vec![0, id]
            });
            self.compile_expression_of_type(&operator.right, Type::int())?;
            self.operations[jump_id].args[0] = self.operations.len();
            Some((Type::int(), false))
        }else if op == "="{
            let (type_, is_reference) = self.compile_expression(&operator.left)?;
            if is_reference && type_ != Type::void(){
                let _right = self.compile_expression_of_type(&operator.right, type_.clone())?;
                let r = self.take_latest_expr();
                let l = self.take_latest_expr();
                self.operations.push(Operation{
                    value: None,
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
                "==" => self.compile_eq_operator(operator.operator, left_type, right_type),
                "!=" => self.compile_not_eq_operator(operator.operator, left_type, right_type),
                "<" => self.compile_less_operator(operator.operator, left_type, right_type),
                ">" => self.compile_greater_operator(operator.operator, left_type, right_type),
                "<=" => self.compile_less_or_eq_operator(operator.operator, left_type, right_type),
                ">=" => self.compile_greater_or_eq_operator(operator.operator, left_type, right_type),
                _ => {
                    self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_OPERATOR, operator.operator));
                    None
                }
            }
        }
    }

    fn compile_less_operator(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == Type::int() && r == Type::int(){
            self.operations.push(Operation{
                value: None,
                code: Code::Less,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else if l.autocast(&Type::float()) && r.autocast(&Type::float()){
            self.operations.push(Operation {
                value: None,
                code: Code::Less,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_greater_operator(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == Type::int() && r == Type::int(){
            self.operations.push(Operation{
                value: None,
                code: Code::Less,
                args: vec![res, right, left]
            });
            Some((Type::int(), false))
        }else if l.autocast(&Type::float()) && r.autocast(&Type::float()){
            self.operations.push(Operation {
                value: None,
                code: Code::Less,
                args: vec![res, right, left]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_less_or_eq_operator(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == Type::int() && r == Type::int(){
            self.operations.push(Operation{
                value: None,
                code: Code::LessOrEq,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else if l.autocast(&Type::float()) && r.autocast(&Type::float()){
            self.operations.push(Operation {
                value: None,
                code: Code::LessOrEq,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_greater_or_eq_operator(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == Type::int() && r == Type::int(){
            self.operations.push(Operation{
                value: None,
                code: Code::LessOrEq,
                args: vec![res, right, left]
            });
            Some((Type::int(), false))
        }else if l.autocast(&Type::float()) && r.autocast(&Type::float()){
            self.operations.push(Operation {
                value: None,
                code: Code::LessOrEq,
                args: vec![res, right, left]
            });
            Some((Type::int(), false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_eq_operator(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == r{
            self.operations.push(Operation{
                value: None,
                code: Code::Eq,
                args: vec![res, left, right]
            });
            Some((l, false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_not_eq_operator(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l == r{
            self.operations.push(Operation{
                value: None,
                code: Code::NotEq,
                args: vec![res, left, right]
            });
            Some((l, false))
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_TYPES, token));
            None
        }
    }

    fn compile_operator_plus(&mut self, token: &'a TokenData<'a>, l: Type, r: Type)->Option<(Type, bool)>{
        let right = self.take_latest_expr();
        let left = self.take_latest_expr();
        let res = self.put_expr();
        if l.pointer_count > 0 && r == Type::int() {
            self.operations.push(Operation {
                value: None,
                code: Code::Sum,
                args: vec![res, left, right]
            });
            Some((l.clone(), false))
        }else if l == Type::int() && r.pointer_count > 0 {
            self.operations.push(Operation {
                value: None,
                code: Code::Sum,
                args: vec![res, left, right]
            });
            Some((r.clone(), false))
        }else if l == Type::int() && r == Type::int(){
            self.operations.push(Operation {
                value: None,
                code: Code::Sum,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else if l.autocast(&Type::float()) && r.autocast(&Type::float()){
            self.operations.push(Operation {
                value: None,
                code: Code::Sum,
                args: vec![res, left, right]
            });
            Some((Type::float(), false))
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
                value: None,
                code: Code::Diff,
                args: vec![res, left, right]
            });
            Some((l.clone(), false))
        }else if l.pointer_count > 0 && l == r {
            self.operations.push(Operation {
                value: None,
                code: Code::Diff,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else if l == Type::int() && r == Type::int(){
            self.operations.push(Operation {
                value: None,
                code: Code::Diff,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else if l.autocast(&Type::float()) && r.autocast(&Type::float()){
            self.operations.push(Operation {
                value: None,
                code: Code::Diff,
                args: vec![res, left, right]
            });
            Some((Type::float(), false))
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
                value: None,
                code: Code::Mul,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else if l.autocast(&Type::float()) && r.autocast(&Type::float()){
            self.operations.push(Operation {
                value: None,
                code: Code::Mul,
                args: vec![res, left, right]
            });
            Some((Type::float(), false))
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
                value: None,
                code: Code::DivI,
                args: vec![res, left, right]
            });
            Some((Type::int(), false))
        }else if l.autocast(&Type::float()) && r.autocast(&Type::float()){
            self.operations.push(Operation {
                value: None,
                code: Code::Div,
                args: vec![res, left, right]
            });
            Some((Type::float(), false))
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
                value: None,
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
                value: None,
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

    fn compile_convert_type(&mut self, token: &'a TokenData<'a>, from: Type, into: Type, _id: usize)->Option<()>{
        if from == into{
            Some(())
        }else if from.pointer_count>0 && into == Type::int(){
            Some(())
        }else if from.autocast(&into){
            Some(())
        }else if from == Type::float() && into == Type::int(){
            let id1 = self.take_latest_expr();
            self.operations.push(Operation{
                value: None,
                code: Code::FloatToInt,
                args: vec![id1, id1]
            });
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
