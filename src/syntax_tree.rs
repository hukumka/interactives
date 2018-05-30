use lexer::TokenData;
use bracket_tree::BracketTreeWalker;
use error::Error;
use error::syntax_tree_parser::*;


#[derive(Debug)]
pub enum Root<'a>{
    FunctionDefinition(FunctionDefinition<'a>),
    VariableDefinition(VariableDefinition<'a>)
}


#[derive(Debug)]
pub struct FunctionDefinition<'a>{
    pub return_type: Type<'a>,
    pub arguments: Vec<Variable<'a>>,
    pub name: &'a TokenData<'a>,
    pub body: Block<'a>
}

#[derive(Debug)]
pub struct Variable<'a>{
    pub type_: Type<'a>,
    pub name: &'a TokenData<'a>
}


#[derive(Debug)]
pub struct Type<'a>{
    pub base: &'a TokenData<'a>,
    pub pointer_count: usize
}

impl<'a> Type<'a>{
    fn is(&self, base: &str, pointer_count: usize)->bool{
        self.base.token_str() == base && self.pointer_count == pointer_count
    }
}


#[derive(Debug)]
pub struct Block<'a>{
    pub statements: Vec<Statement<'a>>
}


#[derive(Debug)]
pub enum Statement<'a>{
    Expression(Expression<'a>),
    VariableDefinition(VariableDefinition<'a>),
    Condition(Condition<'a>),
    ForLoop(ForLoop<'a>),
    Return(Expression<'a>),
}


#[derive(Debug)]
pub struct VariableDefinition<'a>{
    pub variable: Variable<'a>,
    pub set_to: Expression<'a>
}

impl<'a> VariableDefinition<'a>{
    pub fn name(&self)->&TokenData<'a>{
        &self.variable.name
    }
}


#[derive(Debug)]
pub struct Expression<'a>(pub Box<ExpressionData<'a>>);

#[derive(Debug)]
pub enum ExpressionData<'a>{
    FunctionCall(FunctionCall<'a>),
    BinaryOperator(BinaryOperator<'a>),
    PrefixOperator(PrefixOperator<'a>),
    SyffixOperator(SyffixOperator<'a>),
    Index(Index<'a>),
    Variable(&'a TokenData<'a>),
    Constant(&'a TokenData<'a>),
    Array(Vec<Expression<'a>>)
}


#[derive(Debug)]
pub struct Condition<'a>{
    pub condition: Expression<'a>,
    pub then: Block<'a>,
    pub else_: Option<Block<'a>>
}


#[derive(Debug)]
pub struct ForLoop<'a>{
    pub init: ForLoopInitialization<'a>,
    pub step: Expression<'a>,
    pub condition: Expression<'a>,
    pub body: Block<'a>
}


#[derive(Debug)]
pub enum ForLoopInitialization<'a>{
    VariableDefinition(VariableDefinition<'a>),
    Expression(Expression<'a>)
}


#[derive(Debug)]
pub struct FunctionCall<'a>{
    pub name: &'a TokenData<'a>,
    pub arguments: Vec<Expression<'a>>
}


#[derive(Debug)]
pub struct Index<'a>{
    pub expr: Expression<'a>,
    pub index: Expression<'a>
}


#[derive(Debug)]
pub struct BinaryOperator<'a>{
    pub operator: &'a TokenData<'a>,
    pub left: Expression<'a>,
    pub right: Expression<'a>
}


#[derive(Debug)]
pub struct PrefixOperator<'a>{
    pub operator: &'a TokenData<'a>,
    pub right: Expression<'a>
}


#[derive(Debug)]
pub struct SyffixOperator<'a>{
    pub operator: &'a TokenData<'a>,
    pub left: Expression<'a>
}

pub trait Parseable<'a>: Sized{
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>;

    fn parse_silent(walker: &mut BracketTreeWalker<'a>)->Option<Self>{
        let mut v = vec![];
        Self::parse(walker, &mut v)
    }
}


macro_rules! expect_or_put_error{
    ($walker: ident . $function: ident ($($args: expr),*) | $error_stream: ident << $error_code: expr) => {
        $walker.$function($($args),*).or_else(||{
            $error_stream.push(Error::new(
                $walker.code(),
                $error_code,
                $walker.get_pos()
            ));
            None
        })
    }
}


pub fn parse_program<'a>(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Vec<Root<'a>>>{
    let mut res = vec![];
    while !walker.is_empty(){
        res.push(Root::parse(walker, error_stream)?);
    }
    Some(res)
}


/// Root is either
/// type name = *expr*;
/// type name(*args*){*body*}
impl<'a> Parseable<'a> for Root<'a>{
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        // Root is either 
        // VariableDefinition: [Type, Name, Operator(=), Expression, Operator(;), ..]
        // FunctionDefinition: [Type, Name, Layer(Arguments), Layer{Block}]
        // start same for both
        let type_ = Type::parse(walker, error_stream)?;
        let name = expect_or_put_error!(
            walker.expect_name() 
            | error_stream << ERROR_PARSING_ROOT_EXPECT_NAME
        )?;
        // if it's operator(=) go into parsing variable
        if walker.expect_exact_operator("=").is_some(){
            let expr = Expression::parse(walker, error_stream)?;
            expect_or_put_error!(
                walker.expect_exact_operator(";")
                | error_stream << ERROR_PARSING_ROOT_EXPECT_SEMICOLON
            )?;
            Some(Root::VariableDefinition(VariableDefinition{
                variable: Variable{
                    type_,
                    name,
                },
                set_to: expr
            }))
        }else{
            let mut args_walker = expect_or_put_error!(
                walker.expect_layer("(") 
                | error_stream << ERROR_PARSING_ROOT_EXPECT_ROOT
            )?;
            let args = FunctionDefinition::parse_arguments(&mut args_walker, error_stream);
            let mut body_walker = expect_or_put_error!(
                walker.expect_layer("{")
                | error_stream << ERROR_PARSING_FUNC_EXPECT_BODY
            )?;
            let body = Block::parse(&mut body_walker, error_stream);
            Some(Root::FunctionDefinition(FunctionDefinition{
                return_type: type_,
                name,
                arguments: args?,
                body: body?
            }))
        }
    }
}


impl<'a> Parseable<'a> for Type<'a>{
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        // get type name (int from int** *)
        let base = expect_or_put_error!(
            walker.expect_name()
            | error_stream << ERROR_PARSING_TYPE_EXPECT_NAME
        )?;
        // get amount of pointers (3 from int** *)
        let mut pointer_count = 0;
        while let Some(x) = walker.expect_operator_checked(
            |x| x.chars().all(|a| a == '*')
        ){
            pointer_count += x.token_str().len();
        }
        Some(Self{base, pointer_count})
    }
}


impl<'a> FunctionDefinition<'a>{
    fn parse_arguments(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Vec<Variable<'a>>>{
        if walker.is_empty(){
            return Some(vec![]);
        }
        let mut res = vec![];
        loop{
            let type_ = Type::parse(walker, error_stream)?;
            let name = expect_or_put_error!(
                walker.expect_name()
                | error_stream << ERROR_PARSING_FUNC_ARGS_EXPECT_NAME
            )?;
            res.push(Variable{type_, name});

            if walker.is_empty(){
                return Some(res);
            }

            expect_or_put_error!(
                walker.expect_exact_operator(",")
                | error_stream << ERROR_PARSING_FUNC_ARGS_EXPECT_COMA
            )?;
        }
    }
}


impl<'a> Parseable<'a> for Block<'a>{
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        let mut statements = vec![];
        while !walker.is_empty(){
            statements.push(Statement::parse(walker, error_stream)?);
        }
        Some(Block{statements})
    }
}

impl<'a> Block<'a>{
    fn parse_inner_block(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        if let Some(mut body_walker) = walker.expect_layer("{"){
            Some(Block::parse(&mut body_walker, error_stream)?)
        }else{
            let statement = Statement::parse(walker, error_stream)?;
            Some(Block{statements: vec![statement]})
        }
    }
}


impl<'a> Parseable<'a> for Statement<'a>{
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        let walker_start = walker.clone();
        if let Some(key) = walker.expect_name(){
            match key.token_str(){
                "if" => Condition::parse(walker, error_stream).map(|x| Statement::Condition(x)),
                "for" => ForLoop::parse(walker, error_stream).map(|x| Statement::ForLoop(x)),
                "return" => {
                    let expr = Expression::parse(walker, error_stream)?;
                    expect_or_put_error!(
                        walker.expect_exact_operator(";")
                        | error_stream << ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON
                    )?;
                    Some(Statement::Return(expr))
                }
                _ => {
                    *walker = walker_start.clone();   
                    if let Some(vardef) = VariableDefinition::parse(walker, error_stream){
                        Some(Statement::VariableDefinition(vardef))
                    }else{
                        *walker = walker_start;   
                        let expr = Expression::parse(walker, error_stream)?;
                        expect_or_put_error!(
                            walker.expect_exact_operator(";")
                            | error_stream << ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON
                        )?;
                        Some(Statement::Expression(expr))
                    }
                }
            }
        }else{
            let expr = Expression::parse(walker, error_stream)?;
            expect_or_put_error!(
                walker.expect_exact_operator(";")
                | error_stream << ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON
            )?;
            Some(Statement::Expression(expr))
        }
    }
}


impl<'a> Parseable<'a> for VariableDefinition<'a>{
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        let type_ = Type::parse(walker, error_stream)?;
        let name = walker.expect_name()?;
        walker.expect_exact_operator("=")?;
        let expr = Expression::parse(walker, error_stream)?;
        expect_or_put_error!(
            walker.expect_exact_operator(";")
            | error_stream << ERROR_PARSING_VARDEF_EXPECT_SEMICOLON
        )?;
        Some(VariableDefinition{
            variable: Variable{
                type_,
                name
            },
            set_to: expr
        })
    }
}


impl<'a> Parseable<'a> for Condition<'a>{
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        let mut cond_walker = expect_or_put_error!(
            walker.expect_layer("(")
            | error_stream << ERROR_PARSING_COND_EXPECT_CONDITION
        )?;
        let condition = Expression::parse(&mut cond_walker, error_stream)?;
        expect_or_put_error!(
            cond_walker.expect_empty()
            | error_stream << ERROR_PARSING_COND_EXPECT_CONDITION_END
        )?;
        let block = Block::parse_inner_block(walker, error_stream)?;
        let else_ = walker.expect_exact_name("else").and_then(|_|{
            Block::parse_inner_block(walker, error_stream)
        });
        Some(Condition{
            condition,
            then: block,
            else_
        })
    }
}


impl<'a> Parseable<'a> for ForLoop<'a>{
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
        let mut setup_walker = expect_or_put_error!(
            walker.expect_layer("(")
            | error_stream << ERROR_PARSING_FOR_EXPECT_INIT
        )?;
        let type_name = Type::parse(&mut setup_walker, error_stream)
            .and_then(|x| setup_walker.expect_name().map(|y| (x, y)));

        // parse initialization step
        let init = if let Some((type_, name)) = type_name{
            expect_or_put_error!(
                setup_walker.expect_exact_operator("=")
                | error_stream << ERROR_PARSING_FOR_EXPECT_VARDEF
            )?;
            let set_to = Expression::parse(&mut setup_walker, error_stream)?;
            expect_or_put_error!(
                setup_walker.expect_exact_operator(";")
                | error_stream << ERROR_PARSING_FOR_EXPECT_SEMICOLON
            )?;
            ForLoopInitialization::VariableDefinition(VariableDefinition{
                variable: Variable{
                    type_,   
                    name,
                },
                set_to
            })
        }else{
            let expr = Expression::parse(&mut setup_walker, error_stream)?;
            expect_or_put_error!(
                walker.expect_exact_operator(";")
                | error_stream << ERROR_PARSING_FOR_EXPECT_SEMICOLON
            )?;
            ForLoopInitialization::Expression(expr)
        };

        // parse condition
        let cond = Expression::parse(&mut setup_walker, error_stream)?;
        expect_or_put_error!(
            setup_walker.expect_exact_operator(";")
            | error_stream << ERROR_PARSING_FOR_EXPECT_SEMICOLON
        )?;

        // parse step
        let step = Expression::parse(&mut setup_walker, error_stream)?;
        expect_or_put_error!(
            setup_walker.expect_empty()
            | error_stream << ERROR_PARSING_FOR_EXPECT_CLOSE
        )?;

        let body = Block::parse_inner_block(walker, error_stream)?;
        Some(ForLoop{
            init,
            condition: cond,
            step,
            body
        })
    }
}


impl<'a> Parseable<'a> for Expression<'a>{
fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
    // shunting yurd algorithm
    let mut operand_stack = vec![];
    let mut operator_stack = vec![];
    let first_operand = Self::parse_simple(walker, error_stream)?;
    operand_stack.push(first_operand);
    while let Some(operator) = walker.expect_operator_checked(
        Self::can_be_binary
    ){
        loop{
            if let Some(head) = operator_stack.pop(){
                if Self::is_precede(head, operator){
                    // cannot panic, since amount of operands always greater then amount of
                    // operators.
                    let right = operand_stack.pop().unwrap();
                    let left = operand_stack.pop().unwrap();
                    let expr = Expression(Box::new(ExpressionData::BinaryOperator(
                        BinaryOperator{
                            left,
                            right,
                            operator: head
                        }
                    )));
                    operand_stack.push(expr);
                }else{
                    operator_stack.push(head);
                    break;
                }
            }else{
                break;
            }
        }
        operator_stack.push(operator);
        let next_operand = Self::parse_simple(walker, error_stream)?;
        operand_stack.push(next_operand);
    }
    while let Some(operator) = operator_stack.pop(){
        let right = operand_stack.pop().unwrap();
        let left = operand_stack.pop().unwrap();
        let expr = Expression(Box::new(ExpressionData::BinaryOperator(
            BinaryOperator{
                left,
                right,
                operator
            }
        )));
        operand_stack.push(expr);
    }
    operand_stack.pop()
}
}


lazy_static!{ 
static ref OPERATORS_LEVELS: Vec<Vec<&'static str>> = vec![
    vec![".", "->"],
    vec!["*", "/", "%"],
    vec!["+", "-"],
    vec!["<<", ">>"],
    vec!["<", "<=", ">", ">="],
    vec!["==", "!="],
    vec!["&"],
    vec!["^"],
    vec!["|"],
    vec!["&&"],
    vec!["||"],
    vec!["=", "+=", "-=", "*=", "/=", "%=", "<<=", ">>=", "&=", "^=", "|="],
];
}

impl<'a> Expression<'a>{
	fn get_level(op: &'a TokenData<'a>)->usize{
	    OPERATORS_LEVELS.iter().position(|ref r| r.contains(&op.token_str())).unwrap()
	}

	fn can_be_binary(op: &str)->bool{
	    OPERATORS_LEVELS.iter().position(|ref r| r.contains(&op)).is_some()
	}


	pub fn is_precede(one: &'a TokenData<'a>, other: &'a TokenData<'a>)->bool{
	    let one_level = Self::get_level(one);
	    let other_level = Self::get_level(other);
	    other_level >= one_level
	}




	fn parse_simple(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
	    let prefix_operators = ["+", "-", "*", "!", "~", "++", "--", "&"];
	    if let Some(prefix) = walker.expect_operator_checked(|x| prefix_operators.contains(&x)){
	        let inner = Expression::parse_simple(walker, error_stream)?;
	        Some(Expression(Box::new(ExpressionData::PrefixOperator(
	            PrefixOperator{
	                operator: prefix,
	                right: inner
	            }
	        ))))
	    }else{
	        let syffix_operators = ["++", "--"];
	        let mut expr = Self::parse_base(walker, error_stream)?;
	        loop{
	            if let Some(syffix) = walker.expect_operator_checked(|x| syffix_operators.contains(&x)){
	                expr = Expression(Box::new(
	                    ExpressionData::SyffixOperator(
	                        SyffixOperator{
	                            operator: syffix,
	                            left: expr
	                        }
	                    )
	                ));
	            }else if let Some(mut index_walker) = walker.expect_layer("["){
	                let index = Expression::parse(&mut index_walker, error_stream)?;
	                println!("{:?}; {:?}", index, index_walker.get_pos());
	                expect_or_put_error!(
	                    index_walker.expect_empty()
	                    | error_stream << ERROR_PARSING_EXPR_EXPECTED_SBRACKET
	                )?;

	                expr = Expression(Box::new(
	                    ExpressionData::Index(Index{
	                        expr,
	                        index
	                    })
	                ));
	            }else{
	                break;
	            }
	        }
	        Some(expr)
	    }
	}

	fn parse_base(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Self>{
	    if let Some(name) = walker.expect_name(){
	        // function or variable
	        if let Some(mut args) = walker.expect_layer("("){
	            // function
	            let arguments = Expression::parse_arguments(&mut args, error_stream)?;
	            Some(Expression(Box::new(
	                ExpressionData::FunctionCall(FunctionCall{
	                    name,
	                    arguments
	                })
	            )))
	        }else{
	            // variable
	            Some(Expression(Box::new(
	                ExpressionData::Variable(name)
	            )))
	        }
	    }else if let Some(mut inner) = walker.expect_layer("("){
	        let expr = Expression::parse(&mut inner, error_stream)?;
	        expect_or_put_error!(
	            inner.expect_empty()
	            | error_stream << ERROR_PARSING_EXPR_EXPECTED_BRACKET
	        )?;
	        Some(expr)
	    }else{
	        // constant
	        let value = expect_or_put_error!(
	            walker.expect_value()
	            | error_stream << ERROR_PARSING_EXPR_EXPECTED_NAME_OR_VALUE
	        )?;
	        Some(Expression(Box::new(ExpressionData::Constant(
	            value
	        ))))
	    }
	}

	fn parse_arguments(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)->Option<Vec<Expression<'a>>>{
	    if walker.is_empty(){
	        return Some(vec![]);
	    }
	    let mut res = vec![];
	    loop{
	        let expr = Expression::parse(walker, error_stream)?;
	        res.push(expr);
	        if walker.is_empty(){
	            return Some(res);
	        }
	        expect_or_put_error!(
	            walker.expect_exact_operator(",")
	            | error_stream << ERROR_PARSING_EXPR_EXPECTED_COMA
	        )?;
	    }
	}
}


#[cfg(test)]
mod tests{
	use super::*;
	use lexer::Preprocessor;
	use bracket_tree::BracketTree;
	
	macro_rules! new_walker{
	    (let mut $walker_name: ident = $code: expr;) => {
	        let _preprocessor = Preprocessor::new($code);
	        let _data = _preprocessor.tokenize().unwrap();
	        let _tree = BracketTree::new(&_data).unwrap();
	        let mut $walker_name = _tree.walker();
	    }
	}
	
	#[test]
	fn test_expression_base_parse(){
	    //== single variable
	    new_walker!{let mut walker = "a";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_base(&mut walker, &mut error_stream).unwrap();
	    assert!(error_stream.is_empty());
	    match expr{
	        Expression(box ExpressionData::Variable(td)) => {
	            assert_eq!(td.token_str(), "a");
	        },
	        _ => {panic!("unexpected result")}
	    }
	    assert_eq!(walker.get_pos(), 1);
	    
	    //== constant
	    new_walker!{let mut walker = "13";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_base(&mut walker, &mut error_stream).unwrap();
	    assert!(error_stream.is_empty());
	    match expr{
	        Expression(box ExpressionData::Constant(td)) => {
	            assert_eq!(td.token_str(), "13");
	        },
	        _ => {panic!("unexpected result")}
	    }
	    assert_eq!(walker.get_pos(), 1);
	    
	    //== function call
	    new_walker!{let mut walker = "a()";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_base(&mut walker, &mut error_stream).unwrap();
	    assert!(error_stream.is_empty());
	    match expr{
	        Expression(box ExpressionData::FunctionCall(fc)) => {
	            assert_eq!(fc.name.token_str(), "a");
	            assert_eq!(fc.arguments.len(), 0);
	        },
	        _ => {panic!("unexpected result")}
	    }
	    assert_eq!(walker.get_pos(), 3);
	
	    //== (something inside)
	    new_walker!{let mut walker = "(a())";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_base(&mut walker, &mut error_stream).unwrap();
	    assert!(error_stream.is_empty());
	    match expr{
	        Expression(box ExpressionData::FunctionCall(fc)) => {
	            assert_eq!(fc.name.token_str(), "a");
	            assert_eq!(fc.arguments.len(), 0);
	        },
	        _ => {panic!("unexpected result")}
	    }
	    assert_eq!(walker.get_pos(), 5);
	
	
	}
	
	
	macro_rules! check_nested_expression{
	    (
	        $expr: expr; 
	        $type: ident as $in: ident; 
	        return $res: expr; 
	        $(check($left: expr, $right: expr);)*
	    ) => {
	        match $expr{
	            Expression(box ExpressionData::$type($in)) => {
	                $(assert_eq!($left, $right);)*
	                $res
	            },
	            _ => {panic!("Expected {}", stringify!($type))}
	        }
	    }
	}
	
	macro_rules! check_function_expr{
	    ($from: expr, $name: expr) => {check_nested_expression!(
	        $from;
	        FunctionCall as func_call;
	        return func_call.arguments;
	        check(func_call.name.token_str(), $name);
	    )};
	}
	
	macro_rules! check_prefix_op_expr{
	    ($from: expr, $name: expr) => {check_nested_expression!(
	        $from;
	        PrefixOperator as op;
	        return op.right;
	        check(op.operator.token_str(), $name);
	    )};
	}
	
	macro_rules! check_syffix_op_expr{
	    ($from: expr, $name: expr) => {check_nested_expression!(
	        $from;
	        SyffixOperator as op;
	        return op.left;
	        check(op.operator.token_str(), $name);
	    )};
	}
	
	macro_rules! check_index_expr{
	    ($from: expr) => {check_nested_expression!(
	        $from;
	        Index as i;
	        return (i.expr, i.index);
	    )};
	}
	
	macro_rules! check_binary_op_expr{
	    ($from: expr, $op: expr) => {check_nested_expression!(
	        $from;
	        BinaryOperator as i;
	        return (i.left, i.right);
	        check(i.operator.token_str(), $op);
	    )}
	}
	
	macro_rules! check_var_expr{
	    ($from: expr, $name: expr) => {check_nested_expression!(
	        $from;
	        Variable as i;
	        return ();
	        check(i.token_str(), $name);
	    )}
	}
	
	macro_rules! check_const_expr{
	    ($from: expr, $name: expr) => {check_nested_expression!(
	        $from;
	        Constant as i;
	        return ();
	        check(i.token_str(), $name);
	    )}
	}
	
	#[test]
	fn test_expression_parse_simple(){
	    //== same as base
	    new_walker!{let mut walker = "a()";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_simple(&mut walker, &mut error_stream).unwrap();
	    assert!(error_stream.is_empty());
	    let args = check_function_expr!(expr, "a");
	    assert_eq!(args.len(), 0);
	    assert_eq!(walker.get_pos(), 3);
	
	    //== prefix operator
	    new_walker!{let mut walker = "++a()";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_simple(&mut walker, &mut error_stream).unwrap();
	    let expr = check_prefix_op_expr!(expr, "++");
	    let args = check_function_expr!(expr, "a");
	    assert_eq!(args.len(), 0);
	    assert!(error_stream.is_empty());
	    assert_eq!(walker.get_pos(), 4);
	
	    //== syffix operator
	    new_walker!{let mut walker = "a()++";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_simple(&mut walker, &mut error_stream).unwrap();
	    let expr = check_syffix_op_expr!(expr, "++");
	    let args = check_function_expr!(expr, "a");
	    assert_eq!(args.len(), 0);
	    assert!(error_stream.is_empty());
	    assert_eq!(walker.get_pos(), 4);
	
	    //== indexing operator
	    new_walker!{let mut walker = "a()[b()]";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_simple(&mut walker, &mut error_stream).unwrap();
	    let (expr, index) = check_index_expr!(expr);
	    let args = check_function_expr!(index, "b");
	    assert_eq!(args.len(), 0);
	    let args = check_function_expr!(expr, "a");
	    assert_eq!(args.len(), 0);
	    assert!(error_stream.is_empty());
	    assert_eq!(walker.get_pos(), 8);
	
	    //== sequence
	    new_walker!{let mut walker = "- *a()[b()]++";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse_simple(&mut walker, &mut error_stream);
	    let expr = expr.unwrap();
	    let expr = check_prefix_op_expr!(expr, "-");
	    let expr = check_prefix_op_expr!(expr, "*");
	    let expr = check_syffix_op_expr!(expr, "++");
	    let (expr, index) = check_index_expr!(expr);
	    let args = check_function_expr!(expr, "a");
	    assert_eq!(args.len(), 0);
	    let args = check_function_expr!(index, "b");
	    assert_eq!(args.len(), 0);
	}
	
	#[test]
	fn test_expression_parse(){
	    // basic
	    new_walker!{let mut walker = "a + b";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
	    let (left, right) = check_binary_op_expr!(expr, "+");
	    check_var_expr!(left, "a");
	    check_var_expr!(right, "b");
	
	    // same precedence
	    new_walker!{let mut walker = "a + b + c";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
	    let (left, right) = check_binary_op_expr!(expr, "+");
	    check_var_expr!(right, "c");
	    let (left, right) = check_binary_op_expr!(left, "+");
	    check_var_expr!(left, "a");
	    check_var_expr!(right, "b");
	
	    // precedence
	    new_walker!{let mut walker = "a*b + c";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
	    let (left, right) = check_binary_op_expr!(expr, "+");
	    check_var_expr!(right, "c");
	    let (left, right) = check_binary_op_expr!(left, "*");
	    check_var_expr!(left, "a");
	    check_var_expr!(right, "b");
	
	    new_walker!{let mut walker = "c + a*b";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
	    let (left, right) = check_binary_op_expr!(expr, "+");
	    check_var_expr!(left, "c");
	    let (left, right) = check_binary_op_expr!(right, "*");
	    check_var_expr!(left, "a");
	    check_var_expr!(right, "b");
	
	    // complex
	    new_walker!{let mut walker = "*a = (b + c) * d + -e";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
	    let (left, right) = check_binary_op_expr!(expr, "=");
	    let expr = check_prefix_op_expr!(left, "*");
	    check_var_expr!(expr, "a");
	    let (left, right) = check_binary_op_expr!(right, "+");
	    let expr = check_prefix_op_expr!(right, "-");
	    check_var_expr!(expr, "e");
	    let (left, right) = check_binary_op_expr!(left, "*");
	    check_var_expr!(right, "d");
	    let (left, right) = check_binary_op_expr!(left, "+");
	    check_var_expr!(left, "b");
	    check_var_expr!(right, "c");
	    assert!(walker.is_empty());
	
	    // not equal
	    new_walker!{let mut walker = "a !=e";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
	    let (left, right) = check_binary_op_expr!(expr, "!=");
	    check_var_expr!(left, "a");
	    check_var_expr!(right, "e");

        // dereference
	    new_walker!{let mut walker = "*a";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
        let expr = check_prefix_op_expr!(expr, "*");
        check_var_expr!(expr, "a");

        // array TODO
	    /*new_walker!{let mut walker = "[a, b, c]";}
	    let mut error_stream = vec![];
	    let expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
        match expr.0.as_ref(){
            &ExpressionData::Array(ref arr) => {
                check_var_expr!(arr[0], "a");
                check_var_expr!(arr[1], "b");
                check_var_expr!(arr[2], "c");
                assert_eq!(arr.len(), 3);
            },
            _ => panic!("Unexpected result")
        }*/
	}

    #[test]
    fn test_statement_parse(){
        // expression ended with semicolon
        new_walker!{let mut walker = "*a = b + c;";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let expr = match statement{
            Statement::Expression(expr) => expr,
            _ => panic!("Expected expression")
        };
        let (left, right) = check_binary_op_expr!(expr, "=");
        let expr = check_prefix_op_expr!(left, "*");
        check_var_expr!(expr, "a");
        let (left, right) = check_binary_op_expr!(right, "+");
        check_var_expr!(left, "b");
        check_var_expr!(right, "c");
        assert!(walker.is_empty());

        // variable definition
        new_walker!{let mut walker = "int* x = malloc(a, b, c);";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let (type_, name, expr) = match statement{
            Statement::VariableDefinition(v) => (v.variable.type_, v.variable.name, v.set_to),
            _ => panic!("Expected variable definition")
        };
        assert_eq!(type_.base.token_str(), "int");
        assert_eq!(type_.pointer_count, 1);
        assert_eq!(name.token_str(), "x");
        let args = check_function_expr!(expr, "malloc");
        match args.as_slice(){
            &[ref a, ref b, ref c] => {
                check_var_expr!(a, "a");
                check_var_expr!(b, "b");
                check_var_expr!(c, "c");
            },
            _ => panic!("Exprected (a, b, c) arguments")
        }
        assert!(walker.is_empty());

        // if(){}
        new_walker!{let mut walker = "if(a){b;}";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let (cond, block, else_block) = match statement{
            Statement::Condition(v) => (v.condition, v.then, v.else_),
            _ => panic!("Expected condition")
        };
        check_var_expr!(cond, "a");
        let expr = match block.statements.as_slice(){
            [Statement::Expression(v)] => v,
            _ => panic!("Expected expression")
        };
        check_var_expr!(expr, "b");
        assert!(else_block.is_none());
        assert!(walker.is_empty());

        // if(){}else{}
        new_walker!{let mut walker = "if(a){b;}else c;";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let (cond, block, else_block) = match statement{
            Statement::Condition(v) => (v.condition, v.then, v.else_),
            _ => panic!("Expected condition")
        };
        check_var_expr!(cond, "a");
        let expr = match block.statements.as_slice(){
            [Statement::Expression(v)] => v,
            _ => panic!("Expected expression")
        };
        check_var_expr!(expr, "b");
        let else_block = else_block.unwrap();
        let expr = match else_block.statements.as_slice(){
            [Statement::Expression(v)] => v,
            _ => panic!("Expected expression")
        };
        check_var_expr!(expr, "c");

        // for(varder){}
        new_walker!{let mut walker = "for(int i=0; i<10; ++i)a;";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let (init, condition, step, block) = match statement{
            Statement::ForLoop(v) => (v.init, v.condition, v.step, v.body),
            _ => panic!("Expected for loop")
        };
        let (type_, name, set_to) = match init{
            ForLoopInitialization::VariableDefinition(x) => (x.variable.type_, x.variable.name, x.set_to),
            _ => panic!("Expected variable definition")
        };
        assert_eq!(type_.base.token_str(), "int");
        assert_eq!(type_.pointer_count, 0);
        assert_eq!(name.token_str(), "i");
        check_const_expr!(set_to, "0");

        let (left, right) = check_binary_op_expr!(condition, "<");
        check_var_expr!(left, "i");
        check_const_expr!(right, "10");

        let expr = check_prefix_op_expr!(step, "++");
        check_var_expr!(expr, "i");

        let expr = match block.statements.as_slice(){
            [Statement::Expression(v)] => v,
            _ => panic!("Expected expression")
        };
        check_var_expr!(expr, "a");
        assert!(walker.is_empty());
    }

    #[test]
    fn test_parse_block(){
        new_walker!{let mut walker = "for(int i=0; i<10; ++i)a;b;c;";}
        let mut error_stream = vec![];
        let mut block = Block::parse(&mut walker, &mut error_stream).unwrap();

        println!("{:?}", block);

        let statement = block.statements.pop().unwrap();
        let expr = match statement{
            Statement::Expression(v) => v,
            _ => panic!("Expected expression")
        };
        check_var_expr!(expr, "c");

        let statement = block.statements.pop().unwrap();
        let expr = match statement{
            Statement::Expression(v) => v,
            _ => panic!("Expected expression")
        };
        check_var_expr!(expr, "b");

        let statement = block.statements.pop().unwrap();
        let (init, condition, step, inner_block) = match statement{
            Statement::ForLoop(v) => (v.init, v.condition, v.step, v.body),
            _ => panic!("Expected for loop")
        };
        let (type_, name, set_to) = match init{
            ForLoopInitialization::VariableDefinition(x) => (x.variable.type_, x.variable.name, x.set_to),
            _ => panic!("Expected variable definition")
        };
        assert_eq!(type_.base.token_str(), "int");
        assert_eq!(type_.pointer_count, 0);
        assert_eq!(name.token_str(), "i");
        check_const_expr!(set_to, "0");

        let (left, right) = check_binary_op_expr!(condition, "<");
        check_var_expr!(left, "i");
        check_const_expr!(right, "10");

        let expr = check_prefix_op_expr!(step, "++");
        check_var_expr!(expr, "i");

        let expr = match inner_block.statements.as_slice(){
            [Statement::Expression(v)] => v,
            _ => panic!("Expected expression")
        };
        check_var_expr!(expr, "a");

        assert!(walker.is_empty());
        assert!(block.statements.is_empty());
    }

    #[test]
    fn test_root_parse(){
        // variable definition
        new_walker!{let mut walker = "int x=0; int x(){}";}
        let mut error_stream = vec![];
        let r1 = Root::parse(&mut walker, &mut error_stream).unwrap();
        let var_def = match r1{
            Root::VariableDefinition(v) => v,
            _ => panic!("Expected variable definition")
        };
        let type_ = &var_def.variable.type_;
        assert_eq!(type_.base.token_str(), "int");
        assert_eq!(type_.pointer_count, 0);
        let name = &var_def.variable.name;
        assert_eq!(name.token_str(), "x");
        check_const_expr!(var_def.set_to, "0");

        let r2 = Root::parse(&mut walker, &mut error_stream).unwrap();
        let fn_def = match r2{
            Root::FunctionDefinition(v) => v,
            _ => panic!("Expected function definition")
        };
        let type_ = &fn_def.return_type;
        assert_eq!(type_.base.token_str(), "int");
        assert_eq!(type_.pointer_count, 0);
        let name = &fn_def.name;
        assert_eq!(name.token_str(), "x");
        let args = &fn_def.arguments;
        assert!(args.is_empty());
        assert!(fn_def.body.statements.is_empty());

        assert!(walker.is_empty());
    }


    use test::Bencher;
    #[bench]
    fn bench_parse(bench: &mut Bencher){
        new_walker!{let mut walker = "int x=0; int x(int a){return a + 4; for(int i=0; i<a; ++i) a += 3 * 4;}";}
        let mut error_stream = vec![];
        bench.iter(||{
            let mut walker = walker.clone();
            let _r1 = Root::parse(&mut walker, &mut error_stream).unwrap();
        });
    }
}
