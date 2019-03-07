use bracket_tree::BracketTreeWalker;
use error::syntax_tree_parser::*;
use error::Error;
use lexer::{TokenData, TokenType};

#[derive(Debug)]
pub enum Root<'a> {
    FunctionDefinition(FunctionDefinition<'a>),
    FunctionDeclaration(FunctionDeclaration<'a>),
    VariableDefinition(VariableDefinition<'a>),
}

#[derive(Debug)]
pub struct FunctionDefinition<'a> {
    pub decl: FunctionDeclaration<'a>,
    pub body: Block<'a>,
}

#[derive(Debug)]
pub struct FunctionDeclaration<'a> {
    pub ret_name: Variable<'a>,
    pub arguments: Vec<Variable<'a>>,
}

#[derive(Debug)]
pub struct Variable<'a> {
    pub type_: Type<'a>,
    pub name: &'a TokenData<'a>,
}

#[derive(Debug)]
pub struct Type<'a> {
    pub base: TypeBase<'a>,
    pub pointer_count: usize,
}

#[derive(Debug)]
pub struct FunctionType<'a> {
    pub ret: Type<'a>,
    pub args: Vec<Type<'a>>,
}

#[derive(Debug)]
pub enum TypeBase<'a> {
    Base(&'a TokenData<'a>),
    Function(Box<FunctionType<'a>>),
}

#[derive(Debug)]
pub struct Block<'a> {
    pub statements: Vec<StatementA<'a>>,
}

#[derive(Debug)]
pub struct StatementA<'a>{
    pub statement: Statement<'a>,
    pub attrs: Vec<String>
}

#[derive(Debug)]
pub enum Statement<'a> {
    Expression(Expression<'a>),
    VariableDefinition(VariableDefinition<'a>),
    Condition(Condition<'a>),
    ForLoop(ForLoop<'a>),
    Return(Return<'a>),
}

#[derive(Debug)]
pub struct Return<'a> {
    pub expression: Expression<'a>,
    pub first_token: &'a TokenData<'a>,
}

impl<'a> TreeItem<'a> for Return<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        self.first_token
    }
}

impl<'a> TreeItem<'a> for Type<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        match &self.base {
            TypeBase::Base(t) => t,
            TypeBase::Function(f) => f.ret.first_token(),
        }
    }
}

#[derive(Debug)]
pub struct VariableDefinition<'a> {
    pub variable: Variable<'a>,
    pub set_to: Expression<'a>,
}

impl<'a> VariableDefinition<'a> {
    pub fn name(&self) -> &TokenData<'a> {
        &self.variable.name
    }
}

#[derive(Debug)]
pub struct Expression<'a>(pub Box<ExpressionData<'a>>);

#[derive(Debug)]
pub enum ExpressionData<'a> {
    FunctionCall(FunctionCall<'a>),
    BinaryOperator(BinaryOperator<'a>),
    PrefixOperator(PrefixOperator<'a>),
    SyffixOperator(SyffixOperator<'a>),
    Index(Index<'a>),
    Variable(&'a TokenData<'a>),
    Constant(&'a TokenData<'a>),
}

#[derive(Debug)]
pub struct Condition<'a> {
    pub condition: Expression<'a>,
    pub then: Block<'a>,
    pub else_: Option<Block<'a>>,
    pub first_token: &'a TokenData<'a>,
}

#[derive(Debug)]
pub struct ForLoop<'a> {
    pub init: ForLoopInitialization<'a>,
    pub step: Expression<'a>,
    pub condition: Expression<'a>,
    pub body: Block<'a>,
    pub first_token: &'a TokenData<'a>,
}

#[derive(Debug)]
pub enum ForLoopInitialization<'a> {
    VariableDefinition(VariableDefinition<'a>),
    Expression(Expression<'a>),
}

#[derive(Debug)]
pub struct FunctionCall<'a> {
    pub func: Expression<'a>,
    pub arguments: Vec<Expression<'a>>,
}

impl<'a> TreeItem<'a> for FunctionCall<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        self.func.first_token()
    }
}

#[derive(Debug)]
pub struct Index<'a> {
    pub expr: Expression<'a>,
    pub index: Expression<'a>,
}

#[derive(Debug)]
pub struct BinaryOperator<'a> {
    pub operator: &'a TokenData<'a>,
    pub left: Expression<'a>,
    pub right: Expression<'a>,
}

#[derive(Debug)]
pub struct PrefixOperator<'a> {
    pub operator: &'a TokenData<'a>,
    pub right: Expression<'a>,
}

#[derive(Debug)]
pub struct SyffixOperator<'a> {
    pub operator: &'a TokenData<'a>,
    pub left: Expression<'a>,
}

pub trait Parseable<'a>: Sized {
    fn parse(walker: &mut BracketTreeWalker<'a>, error_stream: &mut Vec<Error<'a>>)
        -> Option<Self>;

    fn parse_silent(walker: &mut BracketTreeWalker<'a>) -> Option<Self> {
        let mut v = vec![];
        Self::parse(walker, &mut v)
    }
}

pub trait TreeItem<'a> {
    fn first_token(&self) -> &'a TokenData<'a>;
}

macro_rules! expect_or_put_error{
    ($walker: ident . $function: ident ($($args: expr),*) | $error_stream: ident << $error_code: expr) => {
        $walker.$function($($args),*).or_else(||{
            $error_stream.push(Error::new(
                $walker.code(),
                $error_code,
                $walker.get_text_pos()
            ));
            None
        })
    }
}

pub fn parse_program<'a>(
    walker: &mut BracketTreeWalker<'a>,
    error_stream: &mut Vec<Error<'a>>,
) -> Option<Vec<Root<'a>>> {
    let mut res = vec![];
    while !walker.is_empty() {
        res.push(Root::parse(walker, error_stream)?);
    }
    Some(res)
}

/// Root is either
/// type name = *expr*;
/// type name(*args*){*body*}
impl<'a> Parseable<'a> for Root<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let var = Variable::parse(walker, error_stream)?;
        // if it's operator(=) go into parsing variable
        if walker.expect_exact_operator("=").is_some() {
            let expr = Expression::parse(walker, error_stream)?;
            expect_or_put_error!(
                walker.expect_exact_operator(";")
                    | error_stream << ERROR_PARSING_ROOT_EXPECT_SEMICOLON
            )?;
            Some(Root::VariableDefinition(VariableDefinition {
                variable: var,
                set_to: expr,
            }))
        } else {
            let mut arg_walker = expect_or_put_error!(
                walker.expect_layer("(")
                    | error_stream << ERROR_PARSING_ROOT_EXPECT_VARIABLE_OR_FUNCTION_DEFINITION
            )?;
            let args = FunctionDefinition::parse_arguments(&mut arg_walker, error_stream)?;
            let decl = FunctionDeclaration {
                ret_name: var,
                arguments: args,
            };
            if walker.expect_exact_operator(";").is_some() {
                Some(Root::FunctionDeclaration(decl))
            } else if let Some(mut body_walker) = walker.expect_layer("{") {
                let body = Block::parse(&mut body_walker, error_stream)?;
                Some(Root::FunctionDefinition(FunctionDefinition { decl, body }))
            } else {
                None
            }
        }
    }
}

impl<'a> TreeItem<'a> for Root<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        match self {
            Root::FunctionDefinition(x) => x.first_token(),
            Root::FunctionDeclaration(x) => x.first_token(),
            Root::VariableDefinition(x) => x.first_token(),
        }
    }
}

impl<'a> FunctionDefinition<'a> {
    fn parse_arguments(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Vec<Variable<'a>>> {
        if walker.is_empty() {
            return Some(vec![]);
        }
        let mut res = vec![];
        loop {
            let type_ = Type::parse(walker, error_stream)?;
            let name = expect_or_put_error!(
                walker.expect_name() | error_stream << ERROR_PARSING_FUNC_ARGS_EXPECT_NAME
            )?;
            res.push(Variable { type_, name });

            if walker.is_empty() {
                return Some(res);
            }

            expect_or_put_error!(
                walker.expect_exact_operator(",")
                    | error_stream << ERROR_PARSING_FUNC_ARGS_EXPECT_COMA
            )?;
        }
    }

    pub fn _start() -> &'static FunctionDefinition<'static>{
        lazy_static!{
            static ref NAME_TOKEN: TokenData<'static> = TokenData::new("_start", (0, 6), TokenType::Name);
            static ref TYPE_TOKEN: TokenData<'static> = TokenData::new("void", (0, 4), TokenType::Name);
            static ref CALL_TOKEN: TokenData<'static> = TokenData::new("main", (0, 4), TokenType::Name);

            static ref _START: FunctionDefinition<'static> = {
                let decl = FunctionDeclaration{
                    ret_name: Variable{
                        type_: Type{
                            base: TypeBase::Base(&TYPE_TOKEN),
                            pointer_count: 0
                        },
                        name: &NAME_TOKEN,
                    },
                    arguments: vec![],
                };
                let call = FunctionCall{
                    func: Expression(Box::new(ExpressionData::Variable(&CALL_TOKEN))),
                    arguments: vec![]
                };
                let call = Expression(Box::new(ExpressionData::FunctionCall(call)));
                let block = Block{
                    statements: vec![StatementA{statement: Statement::Expression(call), attrs: vec![]}]
                };
                FunctionDefinition{
                    decl,
                    body: block
                }
            };
        }
        &_START
    }
}

impl<'a> TreeItem<'a> for FunctionDefinition<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        self.decl.first_token()
    }
}
impl<'a> TreeItem<'a> for FunctionDeclaration<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        self.ret_name.first_token()
    }
}
impl<'a> TreeItem<'a> for Variable<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        self.name
    }
}

impl<'a> Parseable<'a> for Type<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        _error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let base = walker.expect_name()?;
        let mut pointer_count = 0;
        while let Some(op) = walker.expect_operator_checked(|op| op.chars().all(|c| c == '*')) {
            pointer_count += op.token_str().len();
        }
        Some(Type {
            base: TypeBase::Base(base),
            pointer_count,
        })
    }
}

impl<'a> Parseable<'a> for Block<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let mut statements = vec![];
        while !walker.is_empty() {
            let statement = Statement::parse(walker, error_stream)?;
            let mut attrs = vec![];
            std::mem::swap(&mut attrs, &mut walker.attrs);

            statements.push(StatementA{
                statement,
                attrs
            });
        }
        Some(Block { statements })
    }
}

impl<'a> Block<'a> {
    fn parse_inner_block(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        if let Some(mut body_walker) = walker.expect_layer("{") {
            Some(Block::parse(&mut body_walker, error_stream)?)
        } else {
            let statement = Statement::parse(walker, error_stream)?;
            let mut attrs = vec![];
            std::mem::swap(&mut attrs, &mut walker.attrs);
            Some(Block {
                statements: vec![StatementA{statement, attrs}],
            })
        }
    }
}

impl<'a> Parseable<'a> for Statement<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let walker_start = walker.clone();
        if let Some(key) = walker.expect_name() {
            match key.token_str() {
                "if" => {
                    walker.put_back();
                    Condition::parse(walker, error_stream).map(Statement::Condition)
                }
                "for" => {
                    walker.put_back();
                    ForLoop::parse(walker, error_stream).map(Statement::ForLoop)
                }
                "return" => {
                    let expr = Expression::parse(walker, error_stream)?;
                    expect_or_put_error!(
                        walker.expect_exact_operator(";")
                            | error_stream << ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON
                    )?;
                    let ret = Return {
                        first_token: key,
                        expression: expr,
                    };
                    Some(Statement::Return(ret))
                }
                _ => {
                    let errors_count = error_stream.len();
                    *walker = walker_start.clone();
                    if let Some(vardef) = VariableDefinition::parse(walker, error_stream) {
                        Some(Statement::VariableDefinition(vardef))
                    } else {
                        *walker = walker_start;
                        let expr = Expression::parse(walker, error_stream)?;
                        expect_or_put_error!(
                            walker.expect_exact_operator(";")
                                | error_stream << ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON
                        )?;
                        error_stream.truncate(errors_count);
                        Some(Statement::Expression(expr))
                    }
                }
            }
        } else {
            let expr = Expression::parse(walker, error_stream)?;
            expect_or_put_error!(
                walker.expect_exact_operator(";")
                    | error_stream << ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON
            )?;
            Some(Statement::Expression(expr))
        }
    }
}

impl<'a> TreeItem<'a> for Statement<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        match self {
            Statement::VariableDefinition(x) => x.first_token(),
            Statement::Expression(x) => x.first_token(),
            Statement::Return(x) => x.first_token(),
            Statement::Condition(x) => x.first_token(),
            Statement::ForLoop(x) => x.first_token(),
        }
    }
}

impl<'a> Parseable<'a> for VariableDefinition<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let variable = Variable::parse(walker, error_stream)?;
        walker.expect_exact_operator("=")?;
        let expr = Expression::parse(walker, error_stream)?;
        expect_or_put_error!(
            walker.expect_exact_operator(";")
                | error_stream << ERROR_PARSING_VARDEF_EXPECT_SEMICOLON
        )?;
        Some(VariableDefinition {
            variable,
            set_to: expr,
        })
    }
}

impl<'a> Parseable<'a> for Variable<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let type_ = Type::parse(walker, error_stream)?;
        if let Some(mut inner) = walker.expect_layer("(") {
            expect_or_put_error!(
                inner.expect_exact_operator("*")
                    | error_stream << ERROR_PARSING_TYPE_EXPECT_ASTERIX
            )?;
            let name = inner.expect_name()?;
            expect_or_put_error!(
                inner.expect_empty() | error_stream << ERROR_PARSING_TYPE_EXPECT_CLOSING_BRACKET
            )?;

            let mut args = vec![];
            let mut inner = expect_or_put_error!(
                walker.expect_layer("(") | error_stream << ERROR_PARSING_TYPE_EXPECT_BRACKET
            )?;
            if !inner.is_empty() {
                loop {
                    args.push(Type::parse(&mut inner, error_stream)?);
                    if inner.is_empty() {
                        break;
                    }
                    expect_or_put_error!(
                        walker.expect_exact_operator(",")
                            | error_stream << ERROR_PARSING_FUNC_ARGS_EXPECT_COMA
                    )?;
                }
            }
            let type_ = Type {
                base: TypeBase::Function(Box::new(FunctionType { ret: type_, args })),
                pointer_count: 0,
            };
            Some(Variable { type_, name })
        } else {
            let name = walker.expect_name()?;
            Some(Variable { type_, name })
        }
    }
}

impl<'a> TreeItem<'a> for VariableDefinition<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        self.variable.type_.first_token()
    }
}

impl<'a> Parseable<'a> for Condition<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let keyword = walker.expect_name()?;
        let mut cond_walker = expect_or_put_error!(
            walker.expect_layer("(") | error_stream << ERROR_PARSING_COND_EXPECT_CONDITION
        )?;
        let condition = Expression::parse(&mut cond_walker, error_stream)?;
        expect_or_put_error!(
            cond_walker.expect_empty() | error_stream << ERROR_PARSING_COND_EXPECT_CONDITION_END
        )?;
        let block = Block::parse_inner_block(walker, error_stream)?;
        let else_ = walker
            .expect_exact_name("else")
            .and_then(|_| Block::parse_inner_block(walker, error_stream));
        Some(Condition {
            condition,
            then: block,
            else_,
            first_token: keyword,
        })
    }
}

impl<'a> TreeItem<'a> for Condition<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        self.first_token
    }
}

impl<'a> Parseable<'a> for ForLoop<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let keyword = walker.expect_name()?;
        let mut setup_walker = expect_or_put_error!(
            walker.expect_layer("(") | error_stream << ERROR_PARSING_FOR_EXPECT_INIT
        )?;
        let type_name = Type::parse(&mut setup_walker, error_stream)
            .and_then(|x| setup_walker.expect_name().map(|y| (x, y)));

        // parse initialization step
        let init = if let Some((type_, name)) = type_name {
            expect_or_put_error!(
                setup_walker.expect_exact_operator("=")
                    | error_stream << ERROR_PARSING_FOR_EXPECT_VARDEF
            )?;
            let set_to = Expression::parse(&mut setup_walker, error_stream)?;
            expect_or_put_error!(
                setup_walker.expect_exact_operator(";")
                    | error_stream << ERROR_PARSING_FOR_EXPECT_SEMICOLON
            )?;
            ForLoopInitialization::VariableDefinition(VariableDefinition {
                variable: Variable { type_, name },
                set_to,
            })
        } else {
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
            setup_walker.expect_empty() | error_stream << ERROR_PARSING_FOR_EXPECT_CLOSE
        )?;

        let body = Block::parse_inner_block(walker, error_stream)?;
        Some(ForLoop {
            init,
            condition: cond,
            step,
            body,
            first_token: keyword,
        })
    }
}

impl<'a> TreeItem<'a> for ForLoop<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        self.first_token
    }
}

impl<'a> Parseable<'a> for Expression<'a> {
    fn parse(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        // shunting yurd algorithm
        let mut operand_stack = vec![];
        let mut operator_stack = vec![];
        let first_operand = Self::parse_simple(walker, error_stream)?;
        operand_stack.push(first_operand);
        while let Some(operator) = walker.expect_operator_checked(Self::can_be_binary) {
            while let Some(head) = operator_stack.pop() {
                if Self::is_precede(head, operator) {
                    // cannot panic, since amount of operands always greater then amount of
                    // operators.
                    let right = operand_stack.pop().unwrap();
                    let left = operand_stack.pop().unwrap();
                    let expr =
                        Expression(Box::new(ExpressionData::BinaryOperator(BinaryOperator {
                            left,
                            right,
                            operator: head,
                        })));
                    operand_stack.push(expr);
                } else {
                    operator_stack.push(head);
                    break;
                }
            }
            operator_stack.push(operator);
            let next_operand = Self::parse_simple(walker, error_stream)?;
            operand_stack.push(next_operand);
        }
        while let Some(operator) = operator_stack.pop() {
            let right = operand_stack.pop().unwrap();
            let left = operand_stack.pop().unwrap();
            let expr = Expression(Box::new(ExpressionData::BinaryOperator(BinaryOperator {
                left,
                right,
                operator,
            })));
            operand_stack.push(expr);
        }
        operand_stack.pop()
    }
}

impl<'a> TreeItem<'a> for Expression<'a> {
    fn first_token(&self) -> &'a TokenData<'a> {
        match self.0.as_ref() {
            ExpressionData::SyffixOperator(x) => x.left.first_token(),
            ExpressionData::PrefixOperator(x) => x.operator,
            ExpressionData::BinaryOperator(x) => x.left.first_token(),
            ExpressionData::FunctionCall(x) => x.func.first_token(),
            ExpressionData::Constant(x) => x,
            ExpressionData::Variable(x) => x,
            ExpressionData::Index(x) => x.expr.first_token(),
        }
    }
}

lazy_static! {
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

impl<'a> Expression<'a> {
    fn get_level(op: &'a TokenData<'a>) -> usize {
        OPERATORS_LEVELS
            .iter()
            .position(|ref r| r.contains(&op.token_str()))
            .unwrap()
    }

    fn can_be_binary(op: &str) -> bool {
        OPERATORS_LEVELS.iter().any(|ref r| r.contains(&op))
    }

    pub fn is_precede(one: &'a TokenData<'a>, other: &'a TokenData<'a>) -> bool {
        let one_level = Self::get_level(one);
        let other_level = Self::get_level(other);
        other_level >= one_level
    }

    pub fn token(&self) -> &TokenData<'a> {
        match self.0.as_ref() {
            ExpressionData::Variable(v) => v,
            ExpressionData::Constant(c) => c,
            ExpressionData::BinaryOperator(op) => op.operator,
            ExpressionData::SyffixOperator(op) => op.operator,
            ExpressionData::PrefixOperator(op) => op.operator,
            ExpressionData::Index(i) => i.expr.token(),
            ExpressionData::FunctionCall(f) => f.func.token(),
        }
    }

    fn parse_simple(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        let prefix_operators = ["+", "-", "*", "!", "~", "++", "--", "&"];
        if let Some(prefix) = walker.expect_operator_checked(|x| prefix_operators.contains(&x)) {
            let inner = Expression::parse_simple(walker, error_stream)?;
            Some(Expression(Box::new(ExpressionData::PrefixOperator(
                PrefixOperator {
                    operator: prefix,
                    right: inner,
                },
            ))))
        } else {
            let syffix_operators = ["++", "--"];
            let mut expr = Self::parse_base(walker, error_stream)?;
            loop {
                if let Some(syffix) =
                    walker.expect_operator_checked(|x| syffix_operators.contains(&x))
                {
                    expr = Expression(Box::new(ExpressionData::SyffixOperator(SyffixOperator {
                        operator: syffix,
                        left: expr,
                    })));
                } else if let Some(mut index_walker) = walker.expect_layer("[") {
                    let index = Expression::parse(&mut index_walker, error_stream)?;
                    expect_or_put_error!(
                        index_walker.expect_empty()
                            | error_stream << ERROR_PARSING_EXPR_EXPECTED_SBRACKET
                    )?;

                    expr = Expression(Box::new(ExpressionData::Index(Index { expr, index })));
                } else {
                    break;
                }
            }
            Some(expr)
        }
    }

    fn parse_base(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Self> {
        if let Some(name) = walker.expect_name() {
            // function or variable
            if let Some(mut args) = walker.expect_layer("(") {
                // function
                let arguments = Expression::parse_arguments(&mut args, error_stream)?;
                Some(Expression(Box::new(ExpressionData::FunctionCall(
                    FunctionCall {
                        func: Expression(Box::new(ExpressionData::Variable(name))),
                        arguments,
                    },
                ))))
            } else {
                // variable
                Some(Expression(Box::new(ExpressionData::Variable(name))))
            }
        } else if let Some(mut inner) = walker.expect_layer("(") {
            let expr = Expression::parse(&mut inner, error_stream)?;
            expect_or_put_error!(
                inner.expect_empty() | error_stream << ERROR_PARSING_EXPR_EXPECTED_BRACKET
            )?;
            Some(expr)
        } else {
            // constant
            let value = expect_or_put_error!(
                walker.expect_value() | error_stream << ERROR_PARSING_EXPR_EXPECTED_NAME_OR_VALUE
            )?;
            Some(Expression(Box::new(ExpressionData::Constant(value))))
        }
    }

    fn parse_arguments(
        walker: &mut BracketTreeWalker<'a>,
        error_stream: &mut Vec<Error<'a>>,
    ) -> Option<Vec<Expression<'a>>> {
        if walker.is_empty() {
            return Some(vec![]);
        }
        let mut res = vec![];
        loop {
            let expr = Expression::parse(walker, error_stream)?;
            res.push(expr);
            if walker.is_empty() {
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
mod tests {
    use super::*;
    use bracket_tree::BracketTree;
    use lexer::Preprocessor;

    macro_rules! new_walker {
        (let mut $walker_name: ident = $code: expr;) => {
            let _preprocessor = Preprocessor::new($code);
            let _data = _preprocessor.tokenize().unwrap();
            let _tree = BracketTree::new(&_data).unwrap();
            let mut $walker_name = _tree.walker();
        };
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
	        check(check_var_expr!(func_call.func, $name), ());
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
    fn test_expression_base_parse() {
        //== single variable
        new_walker!{let mut walker = "a";}
        let mut error_stream = vec![];
        let expr = Expression::parse_base(&mut walker, &mut error_stream).unwrap();
        assert!(error_stream.is_empty());
        match expr {
            Expression(box ExpressionData::Variable(td)) => {
                assert_eq!(td.token_str(), "a");
            }
            _ => panic!("unexpected result"),
        }
        assert_eq!(walker.get_pos(), 1);

        //== constant
        new_walker!{let mut walker = "13";}
        let mut error_stream = vec![];
        let expr = if let Some(e) = Expression::parse_base(&mut walker, &mut error_stream) {
            e
        } else {
            println!("{:?}", error_stream);
            panic!();
        };
        assert!(error_stream.is_empty());
        match expr {
            Expression(box ExpressionData::Constant(td)) => {
                assert_eq!(td.token_str(), "13");
            }
            _ => panic!("unexpected result"),
        }
        assert_eq!(walker.get_pos(), 1);

        //== function call
        println!("func call");
        new_walker!{let mut walker = "a()";}
        let mut error_stream = vec![];
        let expr = Expression::parse_base(&mut walker, &mut error_stream).unwrap();
        assert!(error_stream.is_empty());
        match expr {
            Expression(box ExpressionData::FunctionCall(fc)) => {
                check_var_expr!(fc.func, "a");
                assert_eq!(fc.arguments.len(), 0);
            }
            _ => panic!("unexpected result"),
        }
        assert_eq!(walker.get_pos(), 3);

        //== (something inside)
        new_walker!{let mut walker = "(a())";}
        let mut error_stream = vec![];
        let expr = Expression::parse_base(&mut walker, &mut error_stream).unwrap();
        assert!(error_stream.is_empty());
        match expr {
            Expression(box ExpressionData::FunctionCall(fc)) => {
                check_var_expr!(fc.func, "a");
                assert_eq!(fc.arguments.len(), 0);
            }
            _ => panic!("unexpected result"),
        }
        assert_eq!(walker.get_pos(), 5);
    }
    #[test]
    fn test_expression_parse_simple() {
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
    fn test_expression_parse() {
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
         mut error_stream = vec![];
         expr = Expression::parse(&mut walker, &mut error_stream).unwrap();
        match expr.0.as_ref(){
            &ExpressionData::Array(ref arr) => {
                check_var_expr!(arr[0], "a");
                check_var_expr!(arr[1], "b");
                check_var_expr!(arr[2], "c");
                assert_eq!(arr.len(), 3);
            },
            _ => panic!("Unexpected result")
        }*/    }

    #[test]
    fn test_statement_parse() {
        // expression ended with semicolon
        new_walker!{let mut walker = "*a = b + c;";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let expr = match statement {
            Statement::Expression(expr) => expr,
            _ => panic!("Expected expression"),
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
        let (type_, name, expr) = match statement {
            Statement::VariableDefinition(v) => (v.variable.type_, v.variable.name, v.set_to),
            _ => panic!("Expected variable definition"),
        };
        assert_eq!(type_to_str(&type_.base), Some("int"));
        assert_eq!(type_.pointer_count, 1);
        assert_eq!(name.token_str(), "x");
        let args = check_function_expr!(expr, "malloc");
        match args.as_slice() {
            &[ref a, ref b, ref c] => {
                check_var_expr!(a, "a");
                check_var_expr!(b, "b");
                check_var_expr!(c, "c");
            }
            _ => panic!("Exprected (a, b, c) arguments"),
        }
        assert!(walker.is_empty());

        // if(){}
        new_walker!{let mut walker = "if(a){b;}";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let (cond, block, else_block) = match statement {
            Statement::Condition(v) => (v.condition, v.then, v.else_),
            _ => panic!("Expected condition"),
        };
        check_var_expr!(cond, "a");
        let expr = match block.statements.as_slice() {
            [StatementA{statement: Statement::Expression(v), ..}] => v,
            _ => panic!("Expected expression"),
        };
        check_var_expr!(expr, "b");
        assert!(else_block.is_none());
        assert!(walker.is_empty());

        // if(){}else{}
        new_walker!{let mut walker = "if(a){b;}else c;";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let (cond, block, else_block) = match statement {
            Statement::Condition(v) => (v.condition, v.then, v.else_),
            _ => panic!("Expected condition"),
        };
        check_var_expr!(cond, "a");
        let expr = match block.statements.as_slice() {
            [StatementA{statement: Statement::Expression(v), ..}] => v,
            _ => panic!("Expected expression"),
        };
        check_var_expr!(expr, "b");
        let else_block = else_block.unwrap();
        let expr = match else_block.statements.as_slice() {
            [StatementA{statement:Statement::Expression(v), ..}] => v,
            _ => panic!("Expected expression"),
        };
        check_var_expr!(expr, "c");

        // for(varder){}
        new_walker!{let mut walker = "for(int i=0; i<10; ++i)a;";}
        let mut error_stream = vec![];
        let statement = Statement::parse(&mut walker, &mut error_stream).unwrap();
        let (init, condition, step, block) = match statement {
            Statement::ForLoop(v) => (v.init, v.condition, v.step, v.body),
            _ => panic!("Expected for loop"),
        };
        let (type_, name, set_to) = match init {
            ForLoopInitialization::VariableDefinition(x) => {
                (x.variable.type_, x.variable.name, x.set_to)
            }
            _ => panic!("Expected variable definition"),
        };
        assert_eq!(type_to_str(&type_.base), Some("int"));
        assert_eq!(type_.pointer_count, 0);
        assert_eq!(name.token_str(), "i");
        check_const_expr!(set_to, "0");

        let (left, right) = check_binary_op_expr!(condition, "<");
        check_var_expr!(left, "i");
        check_const_expr!(right, "10");

        let expr = check_prefix_op_expr!(step, "++");
        check_var_expr!(expr, "i");

        let expr = match block.statements.as_slice() {
            [StatementA{statement: Statement::Expression(v), ..}] => v,
            _ => panic!("Expected expression"),
        };
        check_var_expr!(expr, "a");
        assert!(walker.is_empty());
    }

    #[test]
    fn test_parse_block() {
        new_walker!{let mut walker = "for(int i=0; i<10; ++i)a;b;c;";}
        let mut error_stream = vec![];
        let mut block = Block::parse(&mut walker, &mut error_stream).unwrap();

        println!("{:?}", block);

        let statement = block.statements.pop().unwrap().statement;
        let expr = match statement {
            Statement::Expression(v) => v,
            _ => panic!("Expected expression"),
        };
        check_var_expr!(expr, "c");

        let statement = block.statements.pop().unwrap().statement;
        let expr = match statement {
            Statement::Expression(v) => v,
            _ => panic!("Expected expression"),
        };
        check_var_expr!(expr, "b");

        let statement = block.statements.pop().unwrap().statement;
        let (init, condition, step, inner_block) = match statement {
            Statement::ForLoop(v) => (v.init, v.condition, v.step, v.body),
            _ => panic!("Expected for loop"),
        };
        let (type_, name, set_to) = match init {
            ForLoopInitialization::VariableDefinition(x) => {
                (x.variable.type_, x.variable.name, x.set_to)
            }
            _ => panic!("Expected variable definition"),
        };
        assert_eq!(type_to_str(&type_.base), Some("int"));
        assert_eq!(type_.pointer_count, 0);
        assert_eq!(name.token_str(), "i");
        check_const_expr!(set_to, "0");

        let (left, right) = check_binary_op_expr!(condition, "<");
        check_var_expr!(left, "i");
        check_const_expr!(right, "10");

        let expr = check_prefix_op_expr!(step, "++");
        check_var_expr!(expr, "i");

        let expr = match inner_block.statements.as_slice() {
            [StatementA{statement:Statement::Expression(v), ..}] => v,
            _ => panic!("Expected expression"),
        };
        check_var_expr!(expr, "a");

        assert!(walker.is_empty());
        assert!(block.statements.is_empty());
    }

    #[test]
    fn test_root_parse() {
        // variable definition
        new_walker!{let mut walker = "int x=0; int x(){}";}
        let mut error_stream = vec![];
        let r1 = Root::parse(&mut walker, &mut error_stream).unwrap();
        let var_def = match r1 {
            Root::VariableDefinition(v) => v,
            _ => panic!("Expected variable definition"),
        };
        let type_ = &var_def.variable.type_;
        assert_eq!(type_to_str(&type_.base), Some("int"));
        assert_eq!(type_.pointer_count, 0);
        let name = &var_def.variable.name;
        assert_eq!(name.token_str(), "x");
        check_const_expr!(var_def.set_to, "0");

        let r2 = if let Some(x) = Root::parse(&mut walker, &mut error_stream) {
            x
        } else {
            panic!("{:?}", error_stream);
        };
        let fn_def = match r2 {
            Root::FunctionDefinition(v) => v,
            _ => panic!("Expected function definition"),
        };
        let _type = &fn_def.decl.ret_name.type_;
        let type_ = &var_def.variable.type_;
        assert_eq!(type_to_str(&type_.base), Some("int"));
        assert_eq!(type_.pointer_count, 0);
        let name = &fn_def.decl.ret_name.name;
        assert_eq!(name.token_str(), "x");
        let args = &fn_def.decl.arguments;
        assert!(args.is_empty());
        assert!(fn_def.body.statements.is_empty());

        assert!(walker.is_empty());
    }

    fn type_to_str<'a>(t: &'a TypeBase<'a>) -> Option<&'a str> {
        match t {
            TypeBase::Base(t) => Some(t.token_str()),
            _ => None,
        }
    }

    #[test]
    fn test_function_pointer_variable_parse() {
        new_walker!{let mut walker = "int* (*x)(typename)";}
        let mut error_stream = vec![];
        let var = Variable::parse(&mut walker, &mut error_stream).unwrap();
        match var.type_.base {
            TypeBase::Base(_) => panic!("Expected function pointer type"),
            TypeBase::Function(f) => {
                assert_eq!(f.ret.pointer_count, 1);
                assert_eq!(type_to_str(&f.ret.base), Some("int"));
                assert_eq!(f.args.len(), 1);
                assert_eq!(f.args[0].pointer_count, 0);
                assert_eq!(type_to_str(&f.args[0].base), Some("typename"));
            }
        }
        assert_eq!(var.name.token_str(), "x");

        // negative case
        new_walker!{let mut walker = "int(*x)";}
        let mut error_stream = vec![];
        assert!(Variable::parse(&mut walker, &mut error_stream).is_none());
        assert_eq!(error_stream.len(), 1);
        assert_eq!(error_stream[0].code(), ERROR_PARSING_TYPE_EXPECT_BRACKET);

        new_walker!{let mut walker = "int(x)()";}
        let mut error_stream = vec![];
        assert!(Variable::parse(&mut walker, &mut error_stream).is_none());
        assert_eq!(error_stream.len(), 1);
        assert_eq!(error_stream[0].code(), ERROR_PARSING_TYPE_EXPECT_ASTERIX);

        new_walker!{let mut walker = "int(*x, y)()";}
        let mut error_stream = vec![];
        assert!(Variable::parse(&mut walker, &mut error_stream).is_none());
        assert_eq!(error_stream.len(), 1);
        assert_eq!(
            error_stream[0].code(),
            ERROR_PARSING_TYPE_EXPECT_CLOSING_BRACKET
        );
    }

    use test::Bencher;
    #[bench]
    fn bench_parse(bench: &mut Bencher) {
        new_walker!{let mut walker = "int x=0; int x(int a){return a + 4; for(int i=0; i<a; ++i) a += 3 * 4;}";}
        let mut error_stream = vec![];
        bench.iter(|| {
            let mut walker = walker.clone();
            let _r1 = Root::parse(&mut walker, &mut error_stream).unwrap();
        });
    }
}
