pub mod lexer{
    pub const ERROR_UNKNOWN_TOKEN_TYPE: usize = 0x0000;
    pub const ERROR_UNCLOSED_STRING: usize = 0x0001;
}

pub mod bracket_tree_parser{
    pub const ERROR_UNCLOSED_BRACKET: usize = 0x0100;
    pub const ERROR_BRACKET_DONT_MATCH: usize = 0x0101;
    pub const ERROR_TO_MANY_CLOSING_BRACKETS: usize = 0x0102;
}

pub mod syntax_tree_parser{
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
}

pub mod precompiler{
    pub const ERROR_PRECMP_VARIABLE_REDEFINITION: usize = 0x0300;
    pub const ERROR_PRECMP_UNKNOWN_CONSTANT_TYPE: usize = 0x0301;
    pub const ERROR_PRECMP_UNDEFINED_VARIABLE: usize = 0x0302;

    pub const ERROR_PRECMP_DEREFERENCE_OF_NOT_POINTER: usize = 0x0304;
    pub const ERROR_PRECMP_NO_FITTING_FUNCTION: usize = 0x0305;
    pub const ERROR_PRECMP_TRYING_TO_GET_VOID_VALUE: usize = 0x0306;
    pub const ERROR_PRECMP_OPERATOR_NOT_IMPLEMENTED: usize = 0x0307;
    pub const ERROR_PRECMP_OPERATOR_TYPE_MISMATCH: usize = 0x0308;
    pub const ERROR_PRECMP_CANNOT_ASSIGN_LVALUE: usize = 0x0309;
}


/// Struct used
#[derive(Debug)]
pub struct Error<'a>{
    text: &'a str,
    code_: usize,
    position: usize
}

pub struct RichError<'a>{
    err: Error<'a>,
    extra_positions: Vec<usize>
}

impl<'a> Error<'a>{
    pub fn new(text: &'a str, code: usize, position: usize)->Self{
        Self{text, code_: code, position}
    }

    pub fn code(&self)->usize{
        self.code_
    }
}
