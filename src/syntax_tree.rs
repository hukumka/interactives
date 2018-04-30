use lexer::TokenData;


pub enum TopLevel<'a>{
    FunctionDefinition(FunctionDefinition<'a>),
    VariableDefinition(VariableDefinition<'a>)
}


pub struct FunctionDefinition<'a>{
    return_type: Type<'a>,
    arguments: Vec<Variable<'a>>,
    body: Block<'a>
}


pub struct Variable<'a>{
    type_: Type<'a>,
    name: &'a TokenData<'a>
}


pub struct Type<'a>{
    base: TokenData<'a>,
    pointer_count: usize
}


pub struct Block<'a>{
    statements: Vec<Statement<'a>>
}


pub enum Statement<'a>{
    VariableDefinition(VariableDefinition<'a>),
    Expression(Expression<'a>),
    Condition(Condition<'a>),
    ForLoop(Condition<'a>),
    Return(Expression<'a>),
}


pub struct VariableDefinition<'a>{
    variable: Variable<'a>,
    set_to: Expression<'a>
}


pub struct Expression<'a>(pub Box<ExpressionData<'a>>);

pub enum ExpressionData<'a>{
    FunctionCall(FunctionCall<'a>),
    BinaryOperator(BinaryOperator<'a>),
    PrefixOperator(PrefixOperator<'a>),
    SyffixOperator(SyffixOperator<'a>),
    Variable(&'a TokenData<'a>),
    Constant(&'a TokenData<'a>)
}


pub struct Condition<'a>{
    condition: Expression<'a>,
    then: Block<'a>,
    else_: Option<Block<'a>>
}


pub struct ForLoop<'a>{
    init: ForLoopInitialization<'a>,
    step: Expression<'a>,
    condition: Expression<'a>,
    body: Block<'a>
}


pub enum ForLoopInitialization<'a>{
    VariableDefinition(VariableDefinition<'a>),
    Expression(Expression<'a>)
}


pub struct FunctionCall<'a>{
    name: &'a TokenData<'a>,
    arguments: Vec<Expression<'a>>
}


pub struct BinaryOperator<'a>{
    operator: &'a TokenData<'a>,
    left: Expression<'a>,
    right: Expression<'a>
}


pub struct PrefixOperator<'a>{
    operator: &'a TokenData<'a>,
    right: Expression<'a>
}


pub struct SyffixOperator<'a>{
    operator: &'a TokenData<'a>,
    left: Expression<'a>
}
