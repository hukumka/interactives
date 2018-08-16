//! Compiler module

use std::collections::HashMap;

use lexer::TokenData;
use syntax_tree::{
    Root,
    VariableDefinition,
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
    FunctionType,
    BaseType,
};


#[derive(Debug, Copy, Clone)]
pub enum Code {
    /// Unconditional jump to args[0]
    Jump,
    /// Jump to args[0] if value in cell args[1] == 0
    JumpZ,
    /// Jump to args[0] if values in cell args[1] != 0
    JumpNZ,

    /// put value of args[1]==args[2] into args[0]
    Eq,
    /// put value of args[1]!=args[2] into args[0]
    NotEq,
    /// put value of args[1]<args[2] into args[0]
    Less,
    /// put value of args[1]<=args[2] into args[0]
    LessOrEq,

    /// put value of args[1]+args[2] into args[0]
    Sum,
    /// put value of args[1]-args[2] into args[0]
    Diff,
    /// put value of args[1]*args[2] into args[0]
    Mul,
    /// put value of args[1]/args[2] into args[0]; args[1] and args[2] interpreted as integers
    DivI,
    /// put value of args[1]%args[2] into args[0]
    Mod,

    /// increase value in cell referenced by args[0]
    Inc,
    /// decrease value in cell referenced by args[0]
    Dec,

    /// move value referenced by args[1] into args[0]
    Dereference,
    /// move value in args[1] into cell referenced by args[0]
    PointerSet,

    /// do nothing
    Nop,
    /// Move value from args[1] into args[0]
    Clone,
    /// Move int Value into args[0]
    ConstInt,
    /// Move float Value into args[0]
    ConstFloat,
    /// Move value sp+args[1] into args[0], which is equal to
    /// pointer to local variable with id=args[1]
    PointerToLocal,

    /// Continue evaluation of caller
    Return,
    /// Put current ip into call stack and move to begining called function
    Call,

    /// add args[0] to stack pointer
    AddSp,
    /// sub args[0] from stack pointer
    SubSb,

    /// put value of args[1]/args[2] into args[0]; args[1] and args[2] interpreted as float
    Div,
    /// converts float in args[1] to int and put result into args[0]
    FloatToInt,
}

/// struct carrying data about single operation for virtual machine
#[derive(Debug)]
pub struct Operation{
    /// operation code. Classifies action and determines meaning of other fields.
    pub code: Code,
    /// command arguments. See ```Code``` for extra info
    pub args: Vec<usize>,
    /// extra argument, which cannot be represented as usize
    pub value: Option<Value>
}


#[derive(Debug)]
pub enum Value{
    Int(i32),
    Float(f32),
}


/// Struct, handling variables available in current scope.
struct VariableManager<'a>{
    /// globals variables
    globals: HashMap<&'a str, VariableHandle<'a>>,
    /// local variables
    locals: Vec<HashMap<&'a str, VariableHandle<'a>>>,
    /// total space currently used by local variables
    /// Not equals to ```locals.iter().map(|x| x.len()).sum::<usize>()```!
    /// This difference may be caused by function wanting to use space for return value
    locals_len: usize,
}

impl<'a> VariableManager<'a>{
    /// creates variable manager
    fn new()->Self{
        Self{
            globals: HashMap::new(),
            locals: vec![],
            locals_len: 0,
        }
    }

    /// open new namespace scope
    fn push_namespace(&mut self){
        self.locals.push(HashMap::new());
    }

    /// close current namespace scope
    fn pop_namespace(&mut self)->Option<()>{
        if let Some(discard) = self.locals.pop(){
            self.locals_len -= discard.len();
            Some(())
        }else{
            None
        }
    }

    /// clear data about local variables
    fn clear_locals(&mut self){
        self.locals.clear();
        self.locals_len = 0;
    }

    /// Forces manager to pretend to have one extra local variable
    fn alloc_space_for_return_value(&mut self){
        assert_eq!(self.locals_len, 0);
        assert!(!self.locals.is_empty());
        self.locals_len = 1;
    }

    /// get total amount of local variables !(or how much manager want you to beleive)
    fn locals_len(&self)->usize{
        self.locals_len
    }

    /// register variable with type ```type_``` and name ```name``` in current scope.
    /// default scope is global variables
    /// return corresponding ```VariableHandleAddress``` if succesfull
    fn register_variable(&mut self, name: &'a TokenData<'a>, type_: Type)->Result<VariableHandleAddress, Error<'a>>{
        let (namespace, address) = if let Some(namespace) = self.locals.last_mut(){
            (namespace, VariableHandleAddress::Local(self.locals_len))
        }else{
            let addr = self.globals.len();
            (&mut self.globals, VariableHandleAddress::Global(addr))
        };
        if namespace.get(name.token_str()).is_some(){
            Err(Error::from_token(ERROR_COMPILER_VARIABLE_REDEFINED, name))
        }else{
            if let VariableHandleAddress::Local(_) = address{
                self.locals_len += 1;
            }
            namespace.insert(name.token_str(), VariableHandle{
                type_,
                name,
                address,
            });
            Ok(address)
        }
    }

    /// register function pointer to function corresponding to definition as global variable
    /// panic if already in scope other then globals
    fn register_function_definition(&mut self, func: &'a FunctionDefinition<'a>)->Result<usize, Error<'a>>{
        self.register_function_declaration(&func.decl)
    }
    
    /// register function pointer to function corresponding to declaration as global variable
    /// panic if already in scope other then globals
    fn register_function_declaration(&mut self, func: &'a FunctionDeclaration<'a>)->Result<usize, Error<'a>>{
        assert!(self.locals.is_empty()); // only in global scope
        let type_ = FunctionType::from_declaration(func)
            .map_err(|e| Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, e.first_token()))?;
        let type_ = Type::function_pointer(type_);
        let address = self.register_variable(func.ret_name.name, type_)?;
        if let VariableHandleAddress::Global(id) = address{
            Ok(id)
        }else{
            unreachable!()
        }
    }

    /// return id of function with given name
    fn get_function_id(&self, name: &'a TokenData<'a>)->Option<usize>{
        match self.globals.get(name.token_str()){
            Some(VariableHandle{address: VariableHandleAddress::Global(x), ..}) => {
                Some(*x)
            },
            _ => None
        }
    }

    /// return ```VariableHandle``` for "closest" variable with given name
    /// closest means it belongs to first scope (counting from top to globals)
    /// which contains variable with such name
    fn get_variable(&self, name: &'a str)->Option<&VariableHandle<'a>>{
        for namespace in self.locals.iter().rev(){
            if let Some(x) = namespace.get(name){
                return Some(x);
            }
        }
        self.globals.get(name)
    }
}

#[derive(Debug, Clone)]
/// handles data about variable
struct VariableHandle<'a>{
    pub type_: Type,
    pub name: &'a TokenData<'a>,
    /// variable virtual address. This has some resemblanse to address of 
    /// cell in there variable data will be stored, but it is not the same
    pub address: VariableHandleAddress,
}

#[derive(Debug, Copy, Clone)]
/// variable virtual address structure
/// holds variants of different memory allocation types
pub enum VariableHandleAddress{
    Global(usize),
    Local(usize)
}

impl VariableHandleAddress{
    /// converts address to string
    /// for local variables it is just address
    /// for globals its $address
    pub fn to_string(&self)->String{
        use std::fmt::Write;
        let mut res = String::new();
        match self{
            VariableHandleAddress::Global(a) => write!(res, "${}", a).unwrap(),
            VariableHandleAddress::Local(a) => write!(res, "{}", a).unwrap(),
        }
        res
    }
}


#[derive(Debug)]
/// debug info required to generate nice interactive page
pub struct DebugInfo{
    /// Map from offset of variable reference to its offset in stack
    variables: HashMap<usize, (VariableHandleAddress, usize)>,
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
/// Part of debug info responcible for determining variable visibility scopes
pub struct VariableTransaction{
    /// position in code there transaction happened
    pub pos: usize,
    /// variable name
    pub name: String,
    /// local variable id
    pub var_id: usize,
    /// if true then variable added to scope, otherwise removed
    pub add: bool,
    /// position of paired transaction (add for remove and visa versa)
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

    /// get list of entry points for all registered functions.
    /// each entry is a tuple (function_id, function_offset)
    pub fn get_function_entry_points(&self)->&[(usize, usize)]{
        self.functions.as_slice()
    }
    /// get list of entry points for all registered function declarations (without a body).
    /// each entry is a tuple (function_id, function_name)
    pub fn get_function_links(&self)->&[(usize, String)]{
        self.function_links.as_slice()
    }

    /// get list of all ```VariableTransactions```
    pub fn get_variable_transactions(&self)->&[VariableTransaction]{
        self.variables_transactions.as_slice()
    }

    /// stores the point to there ```recover_to_stack_floor``` can remove added in inner scope
    /// variables
    fn make_stack_floor(&self)->usize{
        self.variables_transactions_stack.len()
    }

    /// removes all variables added in scope
    fn recover_to_stack_floor(&mut self, code_offset: usize, floor: usize){
        let count = self.variables_transactions_stack.len() - floor;
        for _ in 0..count{
            self.pop_variable(code_offset);
        }
    }

    /// register variable use
    fn reg_variable_offset<'a>(&mut self, name: &'a TokenData<'a>, stack_offset: VariableHandleAddress){
        let tr_id = match stack_offset{
            VariableHandleAddress::Local(x) => self.variables_transactions_stack[x-1],           
            _ => 0
        };
        self.variables.insert(name.get_pos(), (stack_offset, tr_id));
    }

    /// register variable definition
    fn push_variable<'a>(&mut self, name: &'a TokenData<'a>, stack_offset: VariableHandleAddress, code_offset: usize){
        if let VariableHandleAddress::Local(_) = stack_offset{
            self.variables_transactions_stack.push(self.variables_transactions.len());
            self.variables_transactions.push(VariableTransaction{
                pos: code_offset,
                name: name.token_str().to_string(),
                add: true,
                var_id: self.variables_transactions_stack.len(),
                pair: 0,
            });
        }
        self.reg_variable_offset(name, stack_offset);
    }

    /// register variable removal from scope
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

    /// register statement start at offset
    fn reg_statement_offset<'a>(&mut self, statement: &'a Statement<'a>, vm_offset: usize){
        self.statements_by_id.push(vm_offset);
        self.statements.insert(statement.first_token().get_pos(), vm_offset);
    }

    /// return data about variable use
    pub fn get_variable_data<'a>(&self, name: &'a TokenData<'a>)->Option<(VariableHandleAddress, usize)>{
        self.variables.get(&name.get_pos()).map(|x| x.clone())
    }

    /// return vm code offset for statement
    pub fn get_statement_offset<'a>(&self, statement: &'a Statement<'a>)->Option<usize>{
        self.statements.get(&statement.first_token().get_pos()).map(|x| *x)
    }

    /// return vm code offset for statement (statement determined by its offset in source)
    pub fn get_statement_offset_by_pos<'a>(&self, pos: usize)->Option<usize>{
        self.statements.get(&pos).map(|x| *x)
    }

    /// get all statement vm code offsets
    pub fn statements_offsets(&self)->&[usize]{
        self.statements_by_id.as_slice()
    }
}


/// struct designed to take syntax tree and produce virtual machine code
pub struct Compiler<'a>{
    /// compiled operations
    operations: Vec<Operation>,
    /// variable manager
    variables: VariableManager<'a>,
    /// amount of temporary value used to compute expression
    temp_values: usize,
    /// currently compiled function
    current_function: Option<&'a FunctionDefinition<'a>>,
    debug_info: DebugInfo,
    /// errors happened during compiling
    errors: Vec<Error<'a>>
}


impl<'a> Compiler<'a>{
    /// creates new instance of compiler
    pub fn new()->Self{
        Self{
            operations: vec![],
            variables: VariableManager::new(),
            temp_values: 0,
            current_function: None,
            debug_info: DebugInfo::new(),
            errors: vec![],
        }
    }

    /// compile syntax tree and get result
    pub fn compile(mut self, tree: &'a [Root<'a>])->Result<(Vec<Operation>, DebugInfo), Vec<Error<'a>>>{
        // define all functions
        for t in tree{
            match t{
                Root::FunctionDefinition(def) => {
                    if let Err(e) = self.variables.register_function_definition(def){
                        self.errors.push(e);
                    }
                },
                Root::FunctionDeclaration(decl) => {
                    match self.variables.register_function_declaration(decl){
                        Ok(i) => {
                            self.debug_info.function_links.push((i, decl.ret_name.name.token_str().to_string()));
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

    /// compile single function
    pub fn compile_function(&mut self, func: &'a FunctionDefinition<'a>)->Option<usize>{
        let function_id = self.variables.get_function_id(func.decl.ret_name.name)?;
        self.debug_info.functions.push((function_id, self.operations.len()));

        self.current_function = Some(func);
        let start = self.operations.len();
        self.variables.clear_locals();
        self.variables.push_namespace();
        self.variables.alloc_space_for_return_value();
        self.temp_values = 0;
        let arguments_parsed = func.decl.arguments.iter()
            .map(|v| {
                let type_ = Type::from_type(&v.type_).map_err(|e|{
                   self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, e.first_token()));
                   ()
                })?;
                match self.variables.register_variable(v.name, type_){
                    Ok(x) => {
                        self.debug_info.push_variable(v.name, x, self.operations.len());
                        Ok(())
                    },
                    Err(x) => {
                        self.errors.push(x);
                        Err(())
                    }
                }
            }).fold(Ok(()), |a, b| a.and(b));
        if arguments_parsed.is_ok(){
            self.compile_block(&func.body)?;
            self.operations.push(Operation{
                value: None,
                code: Code::Return,
                args: vec![]
            });
            self.current_function = None;
            self.variables.pop_namespace().unwrap();
            // release debug info
            for _ in &func.decl.arguments{
                self.debug_info.pop_variable(self.operations.len());
            }
            Some(start)
        }else{
            self.current_function = None;
            self.variables.pop_namespace().unwrap();
            for _ in &func.decl.arguments{
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
                let res = self.compile_variable_definition(&v);
                res
            },
            Statement::ForLoop(l) => self.compile_for_loop(&l, statement),
            Statement::Condition(c) => {
                self.debug_info.reg_statement_offset(statement, self.operations.len());
                self.compile_condition(&c)
            }
        }
    }

    fn compile_return(&mut self, ret: &'a Expression<'a>)->Option<()>{
        let required_type = Type::from_type(&self.current_function.unwrap().decl.ret_name.type_).or_else(|e|{
            self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, e.first_token()));
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
        let required_type = Type::from_type(&var.variable.type_).or_else(|e|{
            self.errors.push(Error::from_token(ERROR_COMPILER_UNSUPPORTED_TYPE, e.first_token()));
            Err(e)
        }).ok()?;
        let variable_address = self.variables.register_variable(&var.variable.name, required_type.clone()).ok()?;
        let _type_ = self.compile_expression_of_type(&var.set_to, required_type)?;
        self.debug_info.push_variable(var.name(), variable_address, self.operations.len());
        let from = self.take_latest_expr();
        match variable_address{
            VariableHandleAddress::Local(addr) => {
                self.operations.push(Operation{
                    value: None,
                    code: Code::Clone,
                    args: vec![addr, from]
                });
            },
            _ => {
                unimplemented!();
            }
        }
        Some(())
    }

    fn compile_for_loop(&mut self, for_loop: &'a ForLoop<'a>, statement: &'a Statement<'a>)->Option<()>{
        let stack_floor = self.debug_info.make_stack_floor();
        self.variables.push_namespace();
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
        self.variables.pop_namespace().unwrap();
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
        let variable = if let Some(v) = self.variables.get_variable(var.token_str()){
            v.clone()
        }else{
            self.errors.push(Error::from_token(ERROR_COMPILER_UNDEFINED_VARIABLE, var));
            return None;
        };
        let type_ = variable.type_.clone();
        let id = self.put_expr();
        self.debug_info.reg_variable_offset(var, variable.address);
        match variable.address{
            VariableHandleAddress::Local(address) => {
                self.operations.push(Operation{
                    value: None,
                    code: Code::PointerToLocal,
                    args: vec![id, address]
                });
            },
            VariableHandleAddress::Global(address) => {
                self.operations.push(Operation{
                    value: None,
                    code: Code::ConstInt,
                    args: vec![id, address]
                });
            },
        }
        let rvalue = !type_.final_;
        Some((type_, rvalue))
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
        let type_ = self.compile_expression_rvalue(&func.func)?;
        let type_ = match type_{
            Type{pointer_count:0, base: BaseType::Function(box func), final_: _} => {
                func
            },
            _ => {
                self.errors.push(Error::from_token(ERROR_COMPILER_EXPECTED_FUNCTION, func.first_token()));
                return None;
            }
        };
        let func_id = self.take_latest_expr();

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
            self.errors.push(Error::from_token(ERROR_COMPILER_MISMATCHED_ARGUMENTS_COUNT, func.first_token()));
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
        self.temp_values + self.variables.locals_len() - 1
    }

    fn take_latest_expr(&mut self)->usize{
        if self.temp_values > 0 {
            self.temp_values -= 1;
        }else{
            warn!("Warning: Attempt to take expression value then none exists.")
        }
        self.temp_values + self.variables.locals_len()
    }
}
