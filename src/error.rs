/// Struct used 
#[derive(Debug)]
pub struct Error<'a>{
    text: &'a str,
    code_: usize,
    interval: (usize, usize)
}

impl<'a> Error<'a>{
    pub fn new(text: &'a str, code: usize, interval: (usize, usize))->Self{
        Self{text, code_: code, interval}
    }

    pub fn error_where(&self)->&'static str{
        match self.code_{
            ERROR_UNKNOWN_TOKEN_TYPE => "lexer::Preprocessor::eat_token",
            ERROR_UNCLOSED_STRING => "lexer::Preprocessor::eat_string",

            ERROR_UNCLOSED_BRACKET => "parser::BracketTree::new",
            ERROR_BRACKET_DONT_MATCH => "parser::BracketTree::new",
            ERROR_TO_MANY_CLOSING_BRACKETS => "parser::BracketTree::new",

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
