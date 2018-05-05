/// Struct used 
#[derive(Debug)]
pub struct Error<'a>{
    text: &'a str,
    code_: usize,
    position: usize
}

impl<'a> Error<'a>{
    pub fn new(text: &'a str, code: usize, position: usize)->Self{
        Self{text, code_: code, position}
    }

    pub fn error_where(&self)->&'static str{
        match self.code_{
            ERROR_UNKNOWN_TOKEN_TYPE => "lexer::Preprocessor::eat_token",
            ERROR_UNCLOSED_STRING => "lexer::Preprocessor::eat_string",

            ERROR_UNCLOSED_BRACKET => "bracket_tree::BracketTree::new",
            ERROR_BRACKET_DONT_MATCH => "bracket_tree::BracketTree::new",
            ERROR_TO_MANY_CLOSING_BRACKETS => "bracket_tree::BracketTree::new",

            ERROR_PARSING_ROOT_EXPECT_ROOT => "syntax_tree::<Root as Parseable>::parse",
            ERROR_PARSING_ROOT_EXPECT_NAME => "syntax_tree::<Root as Parseable>::parse",
            ERROR_PARSING_TYPE_EXPECT_NAME => "syntax_tree::<Type as Parseable>::parse",
            ERROR_PARSING_ROOT_EXPECT_SEMICOLON => "syntax_tree::<Root as Parseable>::parse",
            ERROR_PARSING_FUNC_EXPECT_BODY => "syntax_tree::<Root as Parseable>::parse",
            ERROR_PARSING_FUNC_ARGS_EXPECT_NAME => "syntax_tree::FunctionDefinition::parse_arguments",
            ERROR_PARSING_FUNC_ARGS_EXPECT_COMA => "syntax_tree::FunctionDefinition::parse_arguments",
            ERROR_PARSING_COND_EXPECT_CONDITION => "syntax_tree::<Condition as Parseable>::parse",
            ERROR_PARSING_COND_EXPECT_CONDITION_END => "syntax_tree::<Condition as Parseable>::parse",
            ERROR_PARSING_FOR_EXPECT_INIT => "syntax_tree::<ForLoop as Parseable>::parse",
            ERROR_PARSING_FOR_EXPECT_VARDEF => "syntax_tree::<ForLoop as Parseable>::parse",
            ERROR_PARSING_FOR_EXPECT_SEMICOLON => "syntax_tree::<ForLoop as Parseable>::parse",
            ERROR_PARSING_FOR_EXPECT_CLOSE => "syntax_tree::<ForLoop as Parseable>::parse",
            ERROR_PARSING_EXPR_EXPECTED_COMA => "syntax_tree::<Expression::parse_arguments",
            ERROR_PARSING_EXPR_EXPECTED_NAME_OR_VALUE => "syntax_tree::Expression::parse_base",
            ERROR_PARSING_EXPR_EXPECTED_SBRACKET => "syntax_tree::Expression::parse_simple",
            ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON => "syntax_tree::<Statement as Parseable>::parse",
            ERROR_PARSING_VARDEF_EXPECT_SEMICOLON => "syntax_tree::<VariableDefinition as Parseable>::parse",

            _ => panic!("Unknown error! Indicates bug in product")
        }
    }

    pub fn code(&self)->usize{
        self.code_
    }
}

pub const ERROR_UNKNOWN_TOKEN_TYPE: usize = 0x0000;
pub const ERROR_UNCLOSED_STRING: usize = 0x0001;

pub const ERROR_UNCLOSED_BRACKET: usize = 0x0100;
pub const ERROR_BRACKET_DONT_MATCH: usize = 0x0101;
pub const ERROR_TO_MANY_CLOSING_BRACKETS: usize = 0x0102;

pub const ERROR_PARSING_ROOT_EXPECT_ROOT: usize = 0x0200;
pub const ERROR_PARSING_ROOT_EXPECT_NAME: usize = 0x0201;
pub const ERROR_PARSING_TYPE_EXPECT_NAME: usize = 0x0202;
pub const ERROR_PARSING_ROOT_EXPECT_SEMICOLON: usize = 0x0203;
pub const ERROR_PARSING_FUNC_EXPECT_BODY: usize = 0x0204;
pub const ERROR_PARSING_FUNC_ARGS_EXPECT_NAME: usize = 0x0205;
pub const ERROR_PARSING_FUNC_ARGS_EXPECT_COMA: usize = 0x0206;
pub const ERROR_PARSING_COND_EXPECT_CONDITION: usize = 0x0207;
pub const ERROR_PARSING_COND_EXPECT_CONDITION_END: usize = 0x0208;
pub const ERROR_PARSING_FOR_EXPECT_INIT: usize = 0x0209;
pub const ERROR_PARSING_FOR_EXPECT_VARDEF: usize = 0x020A;
pub const ERROR_PARSING_FOR_EXPECT_SEMICOLON: usize = 0x020B;
pub const ERROR_PARSING_FOR_EXPECT_CLOSE: usize = 0x020C;
pub const ERROR_PARSING_EXPR_EXPECTED_COMA: usize = 0x020D;
pub const ERROR_PARSING_EXPR_EXPECTED_NAME_OR_VALUE: usize = 0x020E;
pub const ERROR_PARSING_EXPR_EXPECTED_SBRACKET: usize = 0x020F;
pub const ERROR_PARSING_EXPR_EXPECTED_BRACKET: usize = 0x0210;
pub const ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON: usize = 0x0211;
pub const ERROR_PARSING_VARDEF_EXPECT_SEMICOLON: usize = 0x0212;
