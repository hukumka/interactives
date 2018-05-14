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

    fn register(&mut self, var: &'a Variable<'a>, error_stream: &mut Vec<Error<'a>>)->Option<()>{
        self.locals.first_mut().and_then(|mut x| x.register(var, error_stream))
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
                Root::VariableDefinition(var_def) => res.register(&var_def.variable, error_stream)?,
                _ => {}
            }
        }
        Some(res)
    }

    fn register(&mut self, var: &'a Variable<'a>, error_stream: &mut Vec<Error<'a>>)->Option<()>{
        let name_token = var.name;
        let name = name_token.token_str();
        if self.map.get(name).is_none(){
            let id = self.data.len();
            self.map.insert(name, id);
            self.data.push(var);
            Some(())
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


#[derive(Clone, Eq, PartialEq)]
pub struct Type<'a>{
    pointer_count: usize,
    is_reference: bool,
    base: &'a str,
}


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

    fn can_be_force_converted(&self, other: &Type<'a>)->bool{
        self.can_be_converted(other)
            || (self.pointer_count > 0 && other == &Self::int())
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

    fn find_function(&self, name: &'a str, args: &[Type<'a>], error_stream: &mut Vec<Error<'a>>)->Option<usize>{
        self.declarations.iter().position(|(n, type_)|{
            name == n.token_str() && type_.arguments.iter().zip(args).all(|(a, b)| a.can_be_converted(b))
        })
    }

    fn ret_type(&self, id: usize)->Type<'a>{
        self.declarations[id].1.return_type.clone()
    }
}


/// holds information about operations
pub enum Operation{
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
    And(usize, usize),

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
    /// jump if condition is zero to position
    JumpIfZero(usize, usize),
    /// return
    Return(usize),
}

/// holds info about operation and token where its takes place
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

impl<'a> Precompiler<'a>{
    pub fn new(root: &'a [Root], error_stream: &'a mut Vec<Error<'a>>)->Option<Self>{
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
        self.namespace_stack.register(&var_def.variable, &mut self.error_stream)?;
        let ref_pos = self.compile_get_variable(&var_def.variable.name)?;
        let value = self.compile_expression(&var_def.set_to)?;
        Some(self.push(OperationData{
            operation: Operation::Set(ref_pos, value),
            token: var_def.name(),
            return_type: None
        }))
    }

    /// compile for loop
    fn compile_for_loop(&mut self, loop_: &'a ForLoop<'a>)->Option<usize>{
        let start = self.operations.len();
        self.namespace_stack.push();
        match loop_.init{
            ForLoopInitialization::Expression(ref e) => {self.compile_expression(e)?;},
            ForLoopInitialization::VariableDefinition(ref v) => {}

        }
        self.namespace_stack.pop().unwrap();
        Some(start)
    }
    
    /// compile condition
    fn compile_condition(&mut self, cond: &'a Condition<'a>)->Option<usize>{
        let start = self.operations.len();
        let cond_expr = self.compile_expression(&cond.condition)?;
        let token = self.operations[cond_expr].token; 
        let jump = self.push(OperationData{
            operation: Operation::JumpIfZero(cond_expr, 0), // jump position set to zero,
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
        self.operations[jump].operation = Operation::JumpIfZero(cond_expr, then_end_pos);

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
            ExpressionData::SyffixOperator(operator) => self.compile_syffix_operator(&operator)
        }
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
                return_type: Some(return_type)
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

        self.push(OperationData{
            operation: Operation::CreateStackFrame,
            token: name,
            return_type: None
        });

        let mut args_types = vec![];
        for arg in &func.arguments{
            let id = self.compile_expression(arg)?;
            let token = self.operations[id].token;
            self.push(OperationData{
                operation: Operation::PushArg(id),
                token,
                return_type: None
            });
            if let Some(t) = self.operations[id].return_type.clone(){
                args_types.push(t);
            }else{
                let token = self.operations[id].token;
                self.error(token, ERROR_PRECMP_TRYING_TO_GET_VOID_VALUE);
                return None;
            }
        }
        let call_id = if let Some(x) = self.function_manager.find_function(name.token_str(), args_types.as_slice(), &mut self.error_stream){
            x
        }else{
            self.error(func.name, ERROR_PRECMP_NO_FITTING_FUNCTION);
            return None;
        };
        
        let return_type = Some(self.function_manager.ret_type(call_id)); 
        self.push(OperationData{
            operation: Operation::Call(call_id),
            token: name,
            return_type
        });

        None
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
            "&&" => self.operator_eq(op_token, left, right),
            _ => {
                self.error(op_token, ERROR_PRECMP_OPERATOR_NOT_IMPLEMENTED);
                None
            }
        }}
    }

    fn operator_eq(&mut self, token: &'a TokenData<'a>, left: usize, right: usize)->Option<usize> {
        let left_type = self.operations[left].return_type.clone()?;
        let right_type = self.operations[right].return_type.clone()?;
        if left_type.can_be_converted(&right_type.to_lvalue()){
            let left = self.compile_convert(left, &right_type.to_lvalue())?;
            Some(self.push(OperationData {
                operation: Operation::Eq(left, right),
                token,
                return_type: Some(Type::int())
            }))
        }else{
            self.error(token, ERROR_PRECMP_OPERATOR_TYPE_MISMATCH);
            None
        }
    }

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
