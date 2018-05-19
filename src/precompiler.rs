use std::collections::HashMap;

use lexer::TokenData;
use syntax_tree::{
    Type as SyntaxType,
    Root,
    VariableDefinition,
    Variable,
    FunctionDefinition,
    Block,
    Statement,
    Condition,
    ForLoop,
    ForLoopInitialization,
    Expression,
    ExpressionData,
    Index,
    BinaryOperator,
    SyffixOperator,
    PrefixOperator,
    FunctionCall,
};

use error::Error;
use error::precompiler::*;


/// holds info about currently available variables
struct NamespaceStack<'a>{
    /// holds info about global variables present in code
    globals: Namespace<'a>,
    /// holds info about local variables in current scope
    locals: Vec<Namespace<'a>>,
}

impl<'a> NamespaceStack<'a>{
    /// creates new instance of `NamespaceStack`
    fn new(globals: Namespace<'a>)->Self{
        Self{globals, locals: vec![]}
    }

    fn register(&mut self, var: &'a Variable<'a>, error_stream: &mut Vec<Error<'a>>)->Option<usize>{
        self.locals.first_mut().and_then(|x| x.register(var, error_stream))
    }

    /// get data about variable and its id for execution
    fn get_id(&self, name: &'a str)->Option<usize>{
        for i in (0..self.locals.len()).rev(){
            let namespace = &self.locals[i];
            if let Some(id) = namespace.get(name){
                let id = id + self.globals.len()
                    + self.locals[..i].iter().map(|x| x.len()).sum::<usize>();
                return Some(id);
            }
        }

        self.globals.get(name)
    }

    /// get `VariableDefinition` corresponding to variable id for current namespace
    fn get_tree_by_id(&self, mut id: usize)->Option<&'a Variable<'a>>{
        if id < self.globals.len(){
            Some(self.globals.get_tree(id))
        }else{
            id -= self.globals.len();
            for n in &self.locals{
                if id < n.len(){
                    return Some(n.get_tree(id));
                }else{
                    id -= n.len();
                }
            }
            None
        }
    }

    /// begin new namespace
    fn push(&mut self){
        self.locals.push(Namespace::new());
    }

    /// end top namespace
    fn pop(&mut self)->Option<()>{
        self.locals.pop().map(|_| ())
    }
}


/// holds info about variables inside namespace level
struct Namespace<'a>{
    /// holds info about variable ids
    map: HashMap<&'a str, usize>,
    /// holds info about `VariableDefinition` trees
    data: Vec<&'a Variable<'a>>
}

impl<'a> Namespace<'a>{
    fn new()->Self{
        Self{map: HashMap::new(), data: vec![]}
    }

    fn from_tree(tree: &'a [Root], error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        let mut res = Self::new();
        for r in tree{
            match r {
                Root::VariableDefinition(var_def) => {res.register(&var_def.variable, error_stream)?;},
                _ => {}
            }
        }
        Some(res)
    }

    fn register(&mut self, var: &'a Variable<'a>, error_stream: &mut Vec<Error<'a>>)->Option<usize>{
        let name_token = var.name;
        let name = name_token.token_str();
        if self.map.get(name).is_none(){
            let id = self.data.len();
            self.map.insert(name, id);
            self.data.push(var);
            Some(id)
        }else{
            let error = Error::new(
                name_token.code(),
                ERROR_PRECMP_VARIABLE_REDEFINITION,
                name_token.get_pos()
            );
            error_stream.push(error);
            None
        }
    }

    fn len(&self)->usize{
        self.data.len()
    }

    fn get(&self, name: &'a str)->Option<usize>{
        self.map.get(name).map(|&x| x)
    }

    fn get_tree(&self, id: usize)->&'a Variable<'a>{
        self.data[id]
    }

}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type<'a>{
    pointer_count: usize,
    is_reference: bool,
    base: &'a str,
}


#[derive(Debug)]
struct FunctionType<'a>{
    return_type: Type<'a>,
    arguments: Vec<Type<'a>>
}

impl<'a> Type<'a>{
    fn from_syntax_type(type_: &'a SyntaxType<'a>)->Self{
        let base = type_.base.token_str();
        let pointer_count = type_.pointer_count;
        Self{base, pointer_count, is_reference: false}
    }

    fn int()->Self{
        Type{base: "int", pointer_count: 0, is_reference: false}
    }

    fn int_ref()->Self{
        Type{base: "int", pointer_count: 0, is_reference: true}
    }

    fn dereference(&self)->Option<Self>{
        if self.pointer_count > 0{
            Some(Type{
                pointer_count: self.pointer_count - 1,
                base: self.base,
                is_reference: true
            })
        }else{
            None
        }
    }

    fn can_be_converted(&self, other: &Type<'a>)->bool{
        if self.pointer_count != other.pointer_count{
            return false;
        }
        // type can be converted into itself, also reference type can be converted into its
        // compound without reference
        if self.base == other.base && (self.is_reference || !other.is_reference) {
            return true;
        }
        false
    }

    fn to_lvalue(&self)->Self{
        Self{
            pointer_count: self.pointer_count,
            base: self.base,
            is_reference: false
        }
    }
    fn to_rvalue(&self)->Self{
        Self{
            pointer_count: self.pointer_count,
            base: self.base,
            is_reference: true
        }
    }

    fn can_be_force_converted(&self, other: &Type<'a>)->bool{
        self.can_be_converted(other)
            || (self.pointer_count > 0 && other == &Self::int())
    }
}

use std::fmt;
impl<'a> fmt::Display for Type<'a>{
    fn fmt(&self, formatter: &mut fmt::Formatter)->fmt::Result{
        if self.is_reference{
            write!(formatter, "ref ")?;
        }
        write!(formatter, "{}", self.base)?;
        for _ in 0..self.pointer_count{
            write!(formatter, "*")?;
        }
        Ok(())
    }
}



struct FunctionManager<'a>{
    declarations: Vec<(&'a TokenData<'a>, FunctionType<'a>)>
}

impl<'a> FunctionManager<'a>{
    fn from_tree(tree: &'a [Root<'a>])->Self{
        let mut res = FunctionManager{declarations: vec![]};
        for r in tree{
            match r{
                Root::FunctionDefinition(func) => {
                    let return_type = Type::from_syntax_type(&func.return_type);
                    let arguments: Vec<_> = func.arguments.iter()
                        .map(|x| Type::from_syntax_type(&x.type_))
                        .collect();
                    let function_type = FunctionType{return_type, arguments};
                    res.declarations.push((func.name, function_type));
                },
                _ => {}
            }
        }
        res
    }

    fn find_function(&self, name: &'a str, args: &[Type<'a>], _error_stream: &mut Vec<Error<'a>>)->Option<usize>{
        println!("{:?}", self.declarations);
        self.declarations.iter().position(|(n, type_)|{
            name == n.token_str() && type_.arguments.iter().zip(args).all(|(a, b)| b.can_be_converted(a))
        })
    }

    fn ret_type(&self, id: usize)->Type<'a>{
        self.declarations[id].1.return_type.clone()
    }

    fn arg_type(&self, id: usize, arg_index: usize)->Type<'a>{
        self.declarations[id].1.arguments[arg_index].clone()
    }
}


/// holds information about operations
#[derive(Debug, PartialEq, Eq)]
pub enum Operation{
    /// determine that variable with such reference just became alive
    /// indicates that Ref(i) before this not equal to Ref(i) after
    VarDef(usize),
    /// access to variable reference with `id`, returns reference to it
    Ref(usize),
    /// returns integer value
    ValueInt(i32),
    /// returns reference to value inside array. First index is offset of operation
    /// returning reference to array and second is offset to operation returning index
    Index(usize, usize),

    /// create stack frame
    CreateStackFrame,
    /// put argument into stack
    PushArg(usize),
    /// call
    Call(usize),

    /// Conversion from one type into other
    Convert(usize),

    /// sum of two integers
    SumInt(usize, usize),
    /// sum of pointer and integer
    SumPointer(usize, usize),
    /// difference between first and second arguments
    DiffInt(usize, usize),
    /// difference between pointer and integer
    DiffPointer(usize, usize),
    /// difference between two pointers
    DiffPointerPointer(usize, usize),
    /// multiply integer by integer
    MulInt(usize, usize),
    /// divide integer by integer
    DivInt(usize, usize),
    /// find modulo of division integer by integer
    ModInt(usize, usize),
    Eq(usize, usize),
    Less(usize, usize),
    Greater(usize, usize),
    LessOrEq(usize, usize),
    GreaterOrEq(usize, usize),
    NotEq(usize, usize),
    And(usize, usize),
    Or(usize, usize),

    /// not (as !)
    NotInt(usize),
    /// negate
    NegInt(usize),

    /// integer increment
    IncInt(usize),
    /// integer decrement
    DecInt(usize),
    /// pointer increment
    IncPointer(usize),
    /// pointer decrement
    DecPointer(usize),

    /// dereference
    Deref(usize),
    /// turn reference into pointer
    GetPointer(usize),

    /// set value of reference returned by first expr to value of second expr, returns value
    Set(usize, usize),

    /// jump to position
    Jump(usize),
    /// jump to position(first arg) if condition(second arg) is zero
    JumpIfZero(usize, usize),
    /// return
    Return(usize),
}

/// holds info about operation and token where its takes place
#[derive(Debug)]
pub struct OperationData<'a>{
    pub operation: Operation,
    pub token: &'a TokenData<'a>,
    pub return_type: Option<Type<'a>>,
}


/// structure used to build OperationData
struct Precompiler<'a>{
    error_stream: Vec<Error<'a>>,
    operations: Vec<OperationData<'a>>,
    namespace_stack: NamespaceStack<'a>,
    function_manager: FunctionManager<'a>
}

/// used to generate compilation for comparison methods
macro_rules! generate_compile_comparison{
    ($name: ident, $token: tt) => {
        fn $name(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize> {
            let left_type = self.operations[left].return_type.clone()?;
            let right_type = self.operations[right].return_type.clone()?;
            if left_type.can_be_converted(&Type::int()) && right_type.can_be_converted(&Type::int()){
                let left = self.compile_convert(left, &Type::int())?;
                let right = self.compile_convert(right, &Type::int())?;
                Some(self.push(OperationData {
                    operation: Operation::$token(left, right),
                    token,
                    return_type: Some(Type::int())
                }))
            }else if left_type.pointer_count > 0 && left_type.can_be_converted(&right_type.to_lvalue()){
                let left = self.compile_convert(left, &left_type.to_lvalue())?;
                let right = self.compile_convert(right, &left_type.to_lvalue())?;
                Some(self.push(OperationData {
                    operation: Operation::$token(left, right),
                    token,
                    return_type: Some(Type::int())
                }))
            }else{
                self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
                None
            }
        }
    };
}

impl<'a> Precompiler<'a>{
    pub fn new(root: &'a [Root], error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        let globals = Namespace::from_tree(root, error_stream)?;
        let namespace_stack = NamespaceStack::new(globals);
        let function_manager = FunctionManager::from_tree(root);
        Some(Precompiler{error_stream: vec![], operations: vec![], namespace_stack, function_manager})
    }

    /// precompile function; returns position at there it starts
    pub fn compile_function(&mut self, func: &'a FunctionDefinition<'a>)->Option<usize>{
        self.namespace_stack.push();
        for arg in &func.arguments{
            self.namespace_stack.register(arg, &mut self.error_stream)?;
        }
        let starting_position = self.operations.len();
        self.compile_block(&func.body)?;
        self.namespace_stack.pop().unwrap();
        Some(starting_position)
    }

    fn compile_block(&mut self, block: &'a Block<'a>)->Option<usize>{
        let pos = self.operations.len();
        self.namespace_stack.push();
        for statement in &block.statements{
            self.compile_statement(statement)?;
        }
        self.namespace_stack.pop().unwrap();
        Some(pos)
    }

    /// precompile statement; returns position at there its starts
    fn compile_statement(&mut self, statement: &'a Statement<'a>)->Option<usize>{
        match statement{
            Statement::Condition(ref cond) => self.compile_condition(cond),
            Statement::ForLoop(ref for_loop) => self.compile_for_loop(for_loop),
            Statement::Expression(ref expr) => self.compile_expression(expr),
            Statement::VariableDefinition(ref var_def) => self.compile_variable_definition(var_def),
            Statement::Return(ref expr) => self.compile_return(expr),
        }
    }

    /// compile variable definition
    fn compile_variable_definition(&mut self, var_def: &'a VariableDefinition<'a>)->Option<usize>{
        let ref_id = self.namespace_stack.register(&var_def.variable, &mut self.error_stream)?;

        let res = self.push(OperationData{
            operation: Operation::VarDef(ref_id),
            token: var_def.name(),
            return_type: None
        });

        let ref_pos = self.compile_get_variable(&var_def.variable.name)?;
        let value = self.compile_expression(&var_def.set_to)?;

        self.push(OperationData{
            operation: Operation::Set(ref_pos, value),
            token: var_def.name(),
            return_type: None
        });
        Some(res)
    }

    /// compile for loop
    fn compile_for_loop(&mut self, loop_: &'a ForLoop<'a>)->Option<usize>{
        let start = self.operations.len();
        self.namespace_stack.push();
        match loop_.init{
            ForLoopInitialization::Expression(ref e) => {self.compile_expression(e)?;},
            ForLoopInitialization::VariableDefinition(ref v) => {self.compile_variable_definition(v)?;}
        }
        let cond_start = self.operations.len();
        let cond = self.compile_expression(&loop_.condition)?;
        let token = self.operations[cond].token;
        let skip_jump = self.push(OperationData{
            operation: Operation::JumpIfZero(0, cond),
            token,
            return_type: None
        });
        self.compile_block(&loop_.body)?;
        let step = self.compile_expression(&loop_.step)?;
        // return to beginning jump
        let token = self.operations[step].token;
        self.push(OperationData{
            operation: Operation::Jump(cond_start),
            token,
            return_type: None
        });
        let end = self.operations.len();
        let token = self.operations[cond].token;
        self.operations[skip_jump] = OperationData{
            operation: Operation::JumpIfZero(end, cond),
            token, 
            return_type: None
        };
        self.namespace_stack.pop().unwrap();
        Some(start)
    }
    
    /// compile condition
    fn compile_condition(&mut self, cond: &'a Condition<'a>)->Option<usize>{
        let start = self.operations.len();
        let cond_expr = self.compile_expression(&cond.condition)?;
        let token = self.operations[cond_expr].token; 
        let jump = self.push(OperationData{
            operation: Operation::JumpIfZero(0, cond_expr), // jump position set to zero,
            // because if only will be determined after block compilation
            token,
            return_type: None
        });
        self.compile_block(&cond.then)?;
        let then_end_pos = if let Some(ref block) = cond.else_{
            let jump = self.push(OperationData{
                operation: Operation::Jump(0),
                token,
                return_type: None
            });
            let pos = self.operations.len();
            self.compile_block(block)?;
            let end = self.operations.len();
            self.operations[jump].operation = Operation::Jump(end);
            pos
        }else{
            self.operations.len()
        };
        self.operations[jump].operation = Operation::JumpIfZero(then_end_pos, cond_expr);

        Some(start)
    }

    /// compile return statement
    fn compile_return(&mut self, expr: &'a Expression<'a>)->Option<usize>{
        let val = self.compile_expression(expr)?;
        let token = self.operations[val].token;
        Some(self.push(OperationData{
            operation: Operation::Return(val),
            token,
            return_type: None
        }))
    }

    /// precompile expression; returns position of `Operation` returning result
    fn compile_expression(&mut self, expr: &'a Expression<'a>)->Option<usize>{
        match expr.0.as_ref(){
            ExpressionData::Constant(constant) => self.compile_constant(constant),
            ExpressionData::Variable(variable) => self.compile_get_variable(variable),
            ExpressionData::Index(index) => self.compile_index(&index),
            ExpressionData::FunctionCall(call) => self.compile_function_call(&call),
            ExpressionData::BinaryOperator(operator) => self.compile_binary_operator(&operator),
            ExpressionData::PrefixOperator(operator) => self.compile_prefix_operator(&operator),
            ExpressionData::SyffixOperator(operator) => self.compile_syffix_operator(&operator),
            ExpressionData::Array(arr) => self.compile_build_array(&arr)
        }
    }

    fn compile_build_array(&mut self, arr: &[Expression<'a>])->Option<usize>{
        unimplemented!();
    }

    /// precompile constant expression
    fn compile_constant(&mut self, expr: &'a TokenData<'a>)->Option<usize>{
        if let Ok(value) = expr.token_str().parse::<i32>(){
            let operation = OperationData{
                operation: Operation::ValueInt(value),
                token: expr,
                return_type: Some(Type{base: "int", pointer_count: 0, is_reference: false})
            };
            Some(self.push(operation))
        }else {
            self.error(expr, ERROR_PRECMP_UNKNOWN_CONSTANT_TYPE);
            None
        }
    }

    /// precompile access variable
    fn compile_get_variable(&mut self, expr: &'a TokenData<'a>)->Option<usize>{
        if let Some(var_id) = self.namespace_stack.get_id(expr.token_str()){
            let var_def = self.namespace_stack.get_tree_by_id(var_id);
            let var_def = var_def.unwrap();
            let return_type = Type::from_syntax_type(&var_def.type_);
            let operation = OperationData{
                operation: Operation::Ref(var_id),
                token: expr,
                return_type: Some(return_type.to_rvalue())
            };
            Some(self.push(operation))
        }else{
            self.error(expr, ERROR_PRECMP_UNDEFINED_VARIABLE);
            None
        }
    }

    /// precompile index access
    fn compile_index(&mut self, index: &'a Index<'a>)->Option<usize>{
        let array = self.compile_expression(&index.expr)?;
        let index = self.compile_expression(&index.index)?;
        let token = self.operations[array].token.clone();
        let return_type = if let Some(type_) = self.operations[array].return_type.clone().and_then(|x| x.dereference()){
            type_
        }else{
            self.error(token, ERROR_PRECMP_DEREFERENCE_OF_NOT_POINTER);
            return None;
        };
        Some(self.push(OperationData{
            operation: Operation::Index(array, index),
            token,
            return_type: Some(return_type)
        }))
    }

    /// compile function call into stack frame, set of arguments pushes and one call
    fn compile_function_call(&mut self, func: &'a FunctionCall<'a>)->Option<usize>{
        let name = func.name;
        let mut args = vec![];
        let mut arg_types = vec![];

        for arg in &func.arguments{
            let id = self.compile_expression(arg)?;
            if let Some(t) = self.operations[id].return_type.clone(){
                args.push(id);
                arg_types.push(t);
            }else{
                let token = self.operations[id].token;
                self.error(token, ERROR_PRECMP_TRYING_TO_GET_VOID_VALUE);
                return None;
            }
        }

        let call_id = if let Some(x) = self.function_manager.find_function(name.token_str(), arg_types.as_slice(), &mut self.error_stream){
            x
        }else{
            self.error(func.name, ERROR_PRECMP_NO_FITTING_FUNCTION);
            return None;
        };

        self.push(OperationData{
            operation: Operation::CreateStackFrame,
            token: name,
            return_type: None
        });

        for (i, id) in args.iter().enumerate(){
            let type_ = self.function_manager.arg_type(call_id, i);
            let id = self.compile_convert(*id, &type_)?;
            let token = self.operations[id].token;
            self.push(OperationData{
                operation: Operation::PushArg(id),
                token,
                return_type: None
            });
        }
                
        let return_type = Some(self.function_manager.ret_type(call_id)); 
        Some(self.push(OperationData{
            operation: Operation::Call(call_id),
            token: name,
            return_type
        }))
    }

    fn compile_binary_operator(&mut self, operator: &'a BinaryOperator<'a>)->Option<usize>{
        let left = self.compile_expression(&operator.left)?;
        let right = self.compile_expression(&operator.right)?;
        let op_token = &operator.operator;
        if self.operations[left].return_type.is_none() || self.operations[right].return_type.is_none(){
            let token = self.operations[left].token;
            self.error(token, ERROR_PRECMP_TRYING_TO_GET_VOID_VALUE);
            None
        }else{ match op_token.token_str(){
            "+" => self.operator_plus(op_token, left, right),
            "-" => self.operator_minus(op_token, left, right),
            "*" => self.operator_multiply(op_token, left, right),
            "/" => self.operator_divide(op_token, left, right),
            "%" => self.operator_modulo(op_token, left, right),
            "=" => self.operator_set(op_token, left, right),
            "==" => self.operator_eq(op_token, left, right),
            "!=" => self.operator_not_eq(op_token, left, right),
            "&&" => self.operator_and(op_token, left, right),
            "||" => self.operator_or(op_token, left, right),
            "<" => self.operator_less(op_token, left, right),
            ">" => self.operator_greater(op_token, left, right),
            "<=" => self.operator_less_or_eq(op_token, left, right),
            ">=" => self.operator_greater_or_eq(op_token, left, right),
            _ => {
                self.error(op_token, ERROR_PRECMP_OPERATOR_NOT_IMPLEMENTED);
                None
            }
        }}
    }

    generate_compile_comparison!{operator_less, Less}
    generate_compile_comparison!{operator_greater, Greater}
    generate_compile_comparison!{operator_less_or_eq, LessOrEq}
    generate_compile_comparison!{operator_greater_or_eq, GreaterOrEq}
    generate_compile_comparison!{operator_not_eq, NotEq}
    generate_compile_comparison!{operator_eq, Eq}

    fn operator_and(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.can_be_converted(&Type::int()) && right_type.can_be_converted(&Type::int()) {
            // int
            let left = self.compile_convert(left, &Type::int())?;
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::And(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn operator_or(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.can_be_converted(&Type::int()) && right_type.can_be_converted(&Type::int()) {
            // int
            let left = self.compile_convert(left, &Type::int())?;
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::Or(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }


    fn operator_plus(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.can_be_converted(&Type::int()) && right_type.can_be_converted(&Type::int()) {
            // int
            let left = self.compile_convert(left, &Type::int())?;
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::SumInt(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else if left_type.pointer_count > 0 && right_type.can_be_converted(&Type::int()){
            // left is pointer, right is integer
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::SumPointer(left, right),
                token,
                return_type: Some(left_type.to_lvalue())
            }))
        }else if left_type.can_be_converted(&Type::int()) && right_type.pointer_count > 0{
            // left is integer, right is pointer
            let left = self.compile_convert(left, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::SumPointer(right, left),
                token,
                return_type: Some(right_type.to_lvalue())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn operator_minus(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.can_be_converted(&Type::int()) && right_type.can_be_converted(&Type::int()) {
            // int
            let left = self.compile_convert(left, &Type::int())?;
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::DiffInt(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else if left_type.pointer_count > 0 && (left_type.dereference() == right_type.dereference()){
            // pointer minus pointer is integer
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::DiffPointerPointer(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else if left_type.can_be_converted(&Type::int()) && right_type.pointer_count > 0{
            // pointer minus integer is pointer
            let left = self.compile_convert(left, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::DiffPointer(left, right),
                token,
                return_type: Some(left_type.to_lvalue())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn operator_multiply(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.can_be_converted(&Type::int()) && right_type.can_be_converted(&Type::int()) {
            // int
            let left = self.compile_convert(left, &Type::int())?;
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::MulInt(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn operator_divide(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.can_be_converted(&Type::int()) && right_type.can_be_converted(&Type::int()) {
            // int
            let left = self.compile_convert(left, &Type::int())?;
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::DivInt(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn operator_modulo(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.can_be_converted(&Type::int()) && right_type.can_be_converted(&Type::int()) {
            // int
            let left = self.compile_convert(left, &Type::int())?;
            let right = self.compile_convert(right, &Type::int())?;
            Some(self.push(OperationData {
                operation: Operation::ModInt(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn operator_set(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.is_reference && right_type.can_be_converted(&left_type.to_lvalue()) {
            // int
            let right = self.compile_convert(right, &left_type.to_lvalue())?;
            Some(self.push(OperationData {
                operation: Operation::Set(left, right),
                token,
                return_type: Some(left_type)
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    /// compile prefix operator
    fn compile_prefix_operator(&mut self, operator: &'a PrefixOperator<'a>)->Option<usize>{
        let left = self.compile_expression(&operator.right)?;
        let op_token = &operator.operator;
        match op_token.token_str(){
            "++" => self.compile_increment(op_token, left),
            "--" => self.compile_decrement(op_token, left),
            "*" => self.compile_dereference(op_token, left),
            "&" => self.compile_reference(op_token, left),
            "!" => self.compile_not(op_token, left),
            "-" => self.compile_negative(op_token, left),
            _ => {
                self.error(op_token, ERROR_PRECMP_OPERATOR_NOT_IMPLEMENTED);
                None
            }
        }
    }

    fn compile_negative(&mut self, token: &'a TokenData<'a>, val: usize)->Option<usize>{
        let type_ = self.operations[val].return_type.clone()?;
        if type_.can_be_converted(&Type::int()){
            let val = self.compile_convert(val, &Type::int()).unwrap();
            Some(self.push(OperationData {
                operation: Operation::NegInt(val),
                token,
                return_type: Some(Type::int())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn compile_not(&mut self, token: &'a TokenData<'a>, val: usize)->Option<usize>{
        let type_ = self.operations[val].return_type.clone()?;
        if type_.can_be_converted(&Type::int()) || type_.pointer_count > 0{
            let val = self.compile_convert(val, &Type::int()).unwrap();
            Some(self.push(OperationData {
                operation: Operation::NotInt(val),
                token,
                return_type: Some(Type::int())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn compile_dereference(&mut self, token: &'a TokenData<'a>, val: usize)->Option<usize>{
        let type_ = self.operations[val].return_type.clone()?;
        if type_.pointer_count > 0{
            Some(self.push(OperationData{
                operation: Operation::Deref(val),
                token,
                return_type: type_.dereference()
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

    fn compile_reference(&mut self, token: &'a TokenData<'a>, val: usize)->Option<usize>{
        let type_ = self.operations[val].return_type.clone()?;
        if type_.is_reference{
            Some(self.push(OperationData{
                operation: Operation::GetPointer(val),
                token,
                return_type: Some(Type{
                    base: type_.base,
                    pointer_count: type_.pointer_count + 1,
                    is_reference: false
                })
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }


    /// compile syffix operator
    fn compile_syffix_operator(&mut self, operator: &'a SyffixOperator<'a>)->Option<usize>{
        let left = self.compile_expression(&operator.left)?;
        let op_token = &operator.operator;
        match op_token.token_str(){
            "++" => self.compile_increment(op_token, left),
            "--" => self.compile_decrement(op_token, left),
            _ => {
                self.error(op_token, ERROR_PRECMP_OPERATOR_NOT_IMPLEMENTED);
                None
            }
        }
    }

    fn compile_increment(&mut self, token: &'a TokenData<'a>, left: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        if left_type.is_reference{
            if left_type.pointer_count > 0 {
                Some(self.push(OperationData {
                    operation: Operation::IncPointer(left),
                    token,
                    return_type: Some(left_type)
                }))
            }else if left_type.base == "int"{
                Some(self.push(OperationData{
                    operation: Operation::IncInt(left),
                    token,
                    return_type: Some(left_type)
                }))
            }else{
                self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
                None
            }
        }else{
            self.error(token, ERROR_PRECMP_CANNOT_ASSIGN_LVALUE);
            None
        }
    }

    fn compile_decrement(&mut self, token: &'a TokenData<'a>, left: usize)->Option<usize>{
        let left_type = self.operations[left].return_type.clone()?;
        if left_type.is_reference{
            if left_type.pointer_count > 0 {
                Some(self.push(OperationData {
                    operation: Operation::DecPointer(left),
                    token,
                    return_type: Some(left_type)
                }))
            }else if left_type.base == "int"{
                Some(self.push(OperationData{
                    operation: Operation::DecInt(left),
                    token,
                    return_type: Some(left_type)
                }))
            }else{
                self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
                None
            }
        }else{
            self.error(token, ERROR_PRECMP_CANNOT_ASSIGN_LVALUE);
            None
        }
    }

    /// compile convertion from one type to other
    fn compile_convert(&mut self, value: usize, into: &Type<'a>)->Option<usize>{
        if let Some(x) = self.operations[value].return_type.clone(){
            if x.can_be_force_converted(into) && x != *into{
                let token = self.operations[value].token.clone();
                Some(self.push(
                    OperationData{
                        operation: Operation::Convert(value),
                        token,
                        return_type: Some(into.clone())
                    }
                ))
            }else if x == *into{
                Some(value)
            }else{
                None
            }
        }else{
            None
        }
    }

    fn push(&mut self, op: OperationData<'a>)->usize{
        let id = self.operations.len();
        self.operations.push(op);
        id
    }

    fn error(&mut self, token: &'a TokenData<'a>, error_code: usize){
        let error = Error::new(token.code(), error_code, token.get_pos());
        self.error_stream.push(error);
    }
}


#[cfg(test)]
mod tests{
    use syntax_tree::Parseable;

    use lexer::Preprocessor;
    use bracket_tree::BracketTree;
    use syntax_tree::parse_program;
    use super::*;

    macro_rules! prepare_tree{
        (let $i: ident = $code: expr, $type: tt) => {
            let preprocessor = Preprocessor::new($code);
            let tokens = match preprocessor.tokenize(){
                Ok(x) => x,
                Err(e) => panic!("error: {:?}", e)
            };
            let bt = match BracketTree::new(&tokens){
                Ok(x) => x,
                Err(e) => panic!("error: {:?}", e)
            };
            let mut walker = bt.walker();
            let mut error_stream = vec![];
            let $i = if let Some(x) = $type::parse(&mut walker, &mut error_stream){
                x
            }else{
                for error in error_stream{
                    eprintln!("{:?}", error);
                }
                panic!()
            };
        }
    }

    macro_rules! prepare_precompiler{
        () => {
            {
                let mut err = vec![];
                let root = &[];
                let mut x = if let Some(x) = Precompiler::new(root, &mut err){
                    x
                }else{
                    for error in err{
                        eprintln!("{:?}", error);
                    }
                    panic!()
                };
                x.namespace_stack.push();
                x
            }
        }
    }

    macro_rules! assert_operations{
        (#one $data: expr; ($op_expr: expr, $ty_expr: expr)) => {
            assert_eq!($data.operation, $op_expr);
            assert_eq!($data.return_type, $ty_expr, "{:?}", $data);
        };
        ($data: expr; $($x: tt),*) => {
            {
                let mut _counter = 0;
                $(
                    if _counter >= $data.len(){
                        panic!("Operation assert failed: too many operations expected {}/{}", _counter, $data.len());
                    }
                    assert_operations!(#one $data[_counter]; $x);
                    _counter += 1;
                )*
                assert_eq!(_counter, $data.len());
            }
        };
    }

    #[test]
    fn test_compile_constant(){
        prepare_tree!(let expr = "3", Expression);
        let mut precompiler = prepare_precompiler!();
        let x = precompiler.compile_expression(&expr);
        assert_eq!(x, Some(0));
        assert_operations!(precompiler.operations;
            (Operation::ValueInt(3), Some(Type::int()))
        );
    }

    #[test]
    fn test_compile_define_and_use_variable(){
        prepare_tree!(let var_def = "int x = 0;", VariableDefinition);
        prepare_tree!(let expr = "x", Expression);
        prepare_tree!(let expr2 = "x = 3;", Expression);
        let mut precompiler = prepare_precompiler!();
        let x = precompiler.compile_variable_definition(&var_def).unwrap();
        let x = precompiler.compile_expression(&expr);
        assert_eq!(x, Some(4));
        
        let x = precompiler.compile_expression(&expr2);
        assert_eq!(x, Some(7));

        assert_operations!(precompiler.operations;
            (Operation::VarDef(0), None),
            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::ValueInt(0), Some(Type::int())),
            (Operation::Set(1, 2), None),

            (Operation::Ref(0), Some(Type::int_ref())),

            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::ValueInt(3), Some(Type::int())),
            (Operation::Set(5, 6), Some(Type::int_ref()))
        );
    }

    #[test]
    fn test_bin_operator(){
        // +
        prepare_tree!(let expr0 = "1 + 2", Expression);
        // -
        prepare_tree!(let expr1 = "1 - 2", Expression);
        // /
        prepare_tree!(let expr2 = "1 / 2", Expression);
        // *
        prepare_tree!(let expr3 = "1 * 2", Expression);
        // %
        prepare_tree!(let expr4 = "1 % 2", Expression);
        // ==
        prepare_tree!(let expr5 = "1 == 2", Expression);
        // <
        prepare_tree!(let expr6 = "1 < 2", Expression);
        // >
        prepare_tree!(let expr7 = "1 > 2", Expression);
        // ==
        prepare_tree!(let expr8 = "1 != 2", Expression);
        // ||
        prepare_tree!(let expr9 = "1 || 2", Expression);
        let mut precompiler = prepare_precompiler!();
        precompiler.compile_expression(&expr0).unwrap_or_else(|| panic!("0"));
        precompiler.compile_expression(&expr1).unwrap_or_else(|| panic!("1"));
        precompiler.compile_expression(&expr2).unwrap_or_else(|| panic!("2"));
        precompiler.compile_expression(&expr3).unwrap_or_else(|| panic!("3"));
        precompiler.compile_expression(&expr4).unwrap_or_else(|| panic!("4"));
        precompiler.compile_expression(&expr5).unwrap_or_else(|| panic!("5"));
        precompiler.compile_expression(&expr6).unwrap_or_else(|| panic!("6"));
        precompiler.compile_expression(&expr7).unwrap_or_else(|| panic!("7"));
        precompiler.compile_expression(&expr8).unwrap_or_else(|| panic!("8"));
        precompiler.compile_expression(&expr9).unwrap_or_else(|| panic!("9"));

        assert_operations!(precompiler.operations;
            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::SumInt(0, 1), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::DiffInt(3, 4), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::DivInt(6, 7), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::MulInt(9, 10), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::ModInt(12, 13), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::Eq(15, 16), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::Less(18, 19), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::Greater(21, 22), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::NotEq(24, 25), Some(Type::int())),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::Or(27, 28), Some(Type::int()))
        );

        // test complex
        prepare_tree!(let expr = "1 + 2*(3 + 4)/(5 - 6)", Expression);
        let mut precompiler = prepare_precompiler!();
        precompiler.compile_expression(&expr).unwrap_or_else(|| panic!("complex"));
        assert_operations!(precompiler.operations;
            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::ValueInt(3), Some(Type::int())),
            (Operation::ValueInt(4), Some(Type::int())),
            (Operation::SumInt(2, 3), Some(Type::int())),
            (Operation::MulInt(1, 4), Some(Type::int())),
            (Operation::ValueInt(5), Some(Type::int())),
            (Operation::ValueInt(6), Some(Type::int())),
            (Operation::DiffInt(6, 7), Some(Type::int())),
            (Operation::DivInt(5, 8), Some(Type::int())),
            (Operation::SumInt(0, 9), Some(Type::int()))
        );
    }

    #[test]
    fn test_compile_prefix_operator(){
        prepare_tree!(let var_def = "int x = 0;", VariableDefinition);
        prepare_tree!(let expr0 = "-1", Expression);
        prepare_tree!(let expr1 = "!2", Expression);
        prepare_tree!(let expr2 = "&x", Expression);
        prepare_tree!(let expr3 = "* &x", Expression);
        let mut precompiler = prepare_precompiler!();
        precompiler.compile_variable_definition(&var_def).unwrap_or_else(|| panic!("var def"));
        precompiler.compile_expression(&expr0).unwrap_or_else(|| panic!("-"));
        precompiler.compile_expression(&expr1).unwrap_or_else(|| panic!("!"));
        precompiler.compile_expression(&expr2).unwrap_or_else(|| panic!("&"));
        precompiler.compile_expression(&expr3).unwrap_or_else(|| panic!("*&"));
        assert_operations!(precompiler.operations;
            (Operation::VarDef(0), None),
            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::ValueInt(0), Some(Type::int())),
            (Operation::Set(1, 2), None),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::NegInt(4), Some(Type::int())),

            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::NotInt(6), Some(Type::int())),

            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::GetPointer(8), Some(Type{base: "int", pointer_count: 1, is_reference: false})),

            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::GetPointer(10), Some(Type{base: "int", pointer_count: 1, is_reference: false})),
            (Operation::Deref(11), Some(Type::int_ref()))
        );
    }

    #[test]
    fn test_compile_func_call(){
        prepare_tree!(let root = "int f(int a, int b, int c){return a + b + c;}", Root);
        let funcs = vec![root];
        prepare_tree!(let expr1 = "f(1, 2, 3)", Expression);
        prepare_tree!(let var_def = "int a = 3;", VariableDefinition);
        prepare_tree!(let expr2 = "f(1, 2, a)", Expression);
        let mut precompiler = prepare_precompiler!();
        precompiler.function_manager = FunctionManager::from_tree(&funcs);
        let a = precompiler.compile_expression(&expr1);
        if a.is_none(){
            println!("{:?}", precompiler.error_stream);
            panic!("expr1");
        }
        let a = precompiler.compile_variable_definition(&var_def);
        if a.is_none(){
            println!("{:?}", precompiler.error_stream);
            panic!("var_def");
        }
        let a = precompiler.compile_expression(&expr2);
        if a.is_none(){
            println!("{:?}", precompiler.error_stream);
            panic!("expr2");
        }
        assert_operations!(precompiler.operations;
            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::ValueInt(3), Some(Type::int())),
            (Operation::CreateStackFrame, None),
            (Operation::PushArg(0), None),
            (Operation::PushArg(1), None),
            (Operation::PushArg(2), None),
            (Operation::Call(0), Some(Type::int())),
            
            (Operation::VarDef(0), None),
            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::ValueInt(3), Some(Type::int())),
            (Operation::Set(9, 10), None),

            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::CreateStackFrame, None),
            (Operation::PushArg(12), None),
            (Operation::PushArg(13), None),
            (Operation::Convert(14), Some(Type::int())),
            (Operation::PushArg(18), None),
            (Operation::Call(0), Some(Type::int()))
        );
    }

    #[test]
    fn test_indexing_expression(){
        prepare_tree!(let var_def = "int a = 3;", VariableDefinition);
        prepare_tree!(let expr1 = "(&a)[2]", Expression);
        let mut precompiler = prepare_precompiler!();
        precompiler.compile_variable_definition(&var_def).unwrap();
        precompiler.compile_expression(&expr1).unwrap();
        
        assert_operations!(precompiler.operations;
            (Operation::VarDef(0), None),
            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::ValueInt(3), Some(Type::int())),
            (Operation::Set(1, 2), None),

            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::GetPointer(4), Some(Type{base: "int", pointer_count: 1, is_reference: false})),
            (Operation::ValueInt(2), Some(Type::int())),
            (Operation::Index(5, 6), Some(Type::int_ref()))
        );
    }

    #[test]
    fn test_compile_condition(){
        prepare_tree!(let cond = "if(3 > 1){4;}else{2;}", Statement);
        let mut precompiler = prepare_precompiler!();
        precompiler.compile_statement(&cond).unwrap();
        assert_operations!(precompiler.operations;
            (Operation::ValueInt(3), Some(Type::int())),
            (Operation::ValueInt(1), Some(Type::int())),
            (Operation::Greater(0, 1), Some(Type::int())),
            (Operation::JumpIfZero(6, 2), None),
            (Operation::ValueInt(4), Some(Type::int())),
            (Operation::Jump(7), None),
            (Operation::ValueInt(2), Some(Type::int()))
        );
    }

    #[test]
    fn test_compile_loop(){
        prepare_tree!(let cond = "for(int i=0; i<10; ++i){2;}", Statement);
        let mut precompiler = prepare_precompiler!();
        precompiler.compile_statement(&cond).unwrap();
        assert_operations!(precompiler.operations;
            (Operation::VarDef(0), None),
            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::ValueInt(0), Some(Type::int())),
            (Operation::Set(1, 2), None),

            // condition check: 4
            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::ValueInt(10), Some(Type::int())),
            (Operation::Convert(4), Some(Type::int())),
            (Operation::Less(6, 5), Some(Type::int())),
            (Operation::JumpIfZero(13, 7), None),

            // body
            (Operation::ValueInt(2), Some(Type::int())),
            // step
            (Operation::Ref(0), Some(Type::int_ref())),
            (Operation::IncInt(10), Some(Type::int_ref())),
            (Operation::Jump(4), None)
        );
    }
}
