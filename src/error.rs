use std::collections::HashMap;

use lexer::TokenData;

macro_rules! error_code{
    (
        $(pub mod $mod_name: ident{
            $($code_name: ident = $code: expr;)*
        })*
    ) => {
        lazy_static!{
            static ref ERROR_MAP: HashMap<usize, (&'static str, &'static str)> = {
                let mut m = HashMap::new();
                $(
                    let mod_name = stringify!($mod_name);
                    $(
                        m.insert($code, (mod_name, stringify!($code_name)));
                    )*
                )*
                m
            };
        }

        $(pub mod $mod_name{
            $(pub const $code_name: usize = $code;)*
        })*
    }
}

error_code!{
	pub mod lexer{
	    ERROR_UNKNOWN_TOKEN_TYPE = 0x0000;
	    ERROR_UNCLOSED_STRING = 0x0001;
	}
	pub mod bracket_tree_parser{
	    ERROR_UNCLOSED_BRACKET = 0x0100;
	    ERROR_BRACKET_DONT_MATCH = 0x0101;
	    ERROR_TO_MANY_CLOSING_BRACKETS = 0x0102;
	}

	pub mod syntax_tree_parser{
		ERROR_PARSING_ROOT_EXPECT_ROOT = 0x0200;
		ERROR_PARSING_ROOT_EXPECT_NAME = 0x0201;
		ERROR_PARSING_TYPE_EXPECT_NAME = 0x0202;
		ERROR_PARSING_ROOT_EXPECT_SEMICOLON = 0x0203;
		ERROR_PARSING_FUNC_EXPECT_BODY = 0x0204;
		ERROR_PARSING_FUNC_ARGS_EXPECT_NAME = 0x0205;
		ERROR_PARSING_FUNC_ARGS_EXPECT_COMA = 0x0206;
		ERROR_PARSING_COND_EXPECT_CONDITION = 0x0207;
		ERROR_PARSING_COND_EXPECT_CONDITION_END = 0x0208;
		ERROR_PARSING_FOR_EXPECT_INIT = 0x0209;
		ERROR_PARSING_FOR_EXPECT_VARDEF = 0x020A;
		ERROR_PARSING_FOR_EXPECT_SEMICOLON = 0x020B;
		ERROR_PARSING_FOR_EXPECT_CLOSE = 0x020C;
		ERROR_PARSING_EXPR_EXPECTED_COMA = 0x020D;
		ERROR_PARSING_EXPR_EXPECTED_NAME_OR_VALUE = 0x020E;
		ERROR_PARSING_EXPR_EXPECTED_SBRACKET = 0x020F;
		ERROR_PARSING_EXPR_EXPECTED_BRACKET = 0x0210;
		ERROR_PARSING_STATEMENT_EXPECT_SEMICOLON = 0x0211;
		ERROR_PARSING_VARDEF_EXPECT_SEMICOLON = 0x0212;
	}

	pub mod compiler{
	    ERROR_COMPILER_VARIABLE_REDEFINED = 0x0300;
	    ERROR_COMPILER_FUNCTION_REDEFINED = 0x0301;
	    ERROR_COMPILER_UNSUPPORTED_TYPE = 0x0301;
	    ERROR_COMPILER_MISMATCHED_TYPES = 0x0302;
	    ERROR_COMPILER_UNDEFINED_VARIABLE = 0x0303;
	    ERROR_COMPILER_INVALID_CONSTANT = 0x0304;
	    ERROR_COMPILER_UNDEFINED_FUNCTION = 0x0305;
	    ERROR_COMPILER_MISMATCHED_ARGUMENTS_COUNT = 0x0306;
	    ERROR_COMPILER_UNSUPPORTED_OPERATOR = 0x0307;
	}
}


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

    pub fn from_token(code: usize, token: &'a TokenData<'a>)->Self{
        Self{text: token.code(), code_: code, position: token.get_pos()}
    }

    pub fn code(&self)->usize{
        self.code_
    }

    pub fn err_print_message(&self, line_starts: &[usize]){
        let line = match line_starts.binary_search(&self.position){
            Ok(x) => x,
            Err(x) => x - 1
        };
        println!("{} {} {}", self.position, line_starts[line], line);
        let offset = self.position - line_starts[line];
        let from = line_starts[line];
        eprintln!("Error {}::{}({:x}) at line {}, offset {}",
                  self.error_module(),
                  self.error_code_alias(),
                  self.code_,
                  line + 1,
                  offset + 1
        );
        eprintln!("{}v", "=".repeat(offset));
        let to = if line+1 < line_starts.len() {
            line_starts[line+1]
        }else{
            self.text.len()
        };
        eprintln!("{}", &self.text[from..to]);
    }

    fn error_module(&self)->&'static str{
        ERROR_MAP.get(&self.code_).map(|(mod_, _)| mod_).unwrap_or(&"unknown-mod")
    }
    fn error_code_alias(&self)->&'static str{
        ERROR_MAP.get(&self.code_).map(|(_, code_)| code_).unwrap_or(&"unknown-error-code")
    }

}
