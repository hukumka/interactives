//! This is lexing module
//! 
//! 
//! # Examples
//! 
//! ```
//! let code = "int main(){return 0;}";
//! let preprocessor = Preprocessor::new(code);
//! let tokens = preprocessor.tokenize().unwrap();
//! assert_eq!(tokens.len(), 9);
//! 
//! assert_eq!(tokens[0].get_type(), TokenType::Name);
//! assert_eq!(tokens[0].token_str(), "int");
//! 
//! assert_eq!(tokens[1].get_type(), TokenType::Name);
//! assert_eq!(tokens[1].token_str(), "main");
//! 
//! assert_eq!(tokens[2].get_type(), TokenType::Bracket);
//! assert_eq!(tokens[2].token_str(), "(");
//! 
//! assert_eq!(tokens[3].get_type(), TokenType::Bracket);
//! assert_eq!(tokens[3].token_str(), ")");
//! 
//! assert_eq!(tokens[4].get_type(), TokenType::Bracket);
//! assert_eq!(tokens[4].token_str(), "{");
//! 
//! assert_eq!(tokens[5].get_type(), TokenType::Name);
//! assert_eq!(tokens[5].token_str(), "return");
//! 
//! assert_eq!(tokens[6].get_type(), TokenType::Value);
//! assert_eq!(tokens[6].token_str(), "0");
//! 
//! assert_eq!(tokens[7].get_type(), TokenType::Operator);
//! assert_eq!(tokens[7].token_str(), ";");
//! 
//! assert_eq!(tokens[8].get_type(), TokenType::Bracket);
//! assert_eq!(tokens[8].token_str(), "}");
//! ```


use std::fmt;
use std::iter::Peekable;
use std::str::CharIndices;

use error::Error;
use error::lexer::*;


/// Struct used to hold information about token type
#[derive(Debug, Eq, PartialEq, Clone, Copy)]
pub enum TokenType{
    Name,
    Operator,
    Bracket,
    Value
}

impl TokenType{
    /// Returns `TokenType` name as string
    fn as_str(&self)->&'static str{
        match self{
            TokenType::Name => "Name",
            TokenType::Operator => "Operator",
            TokenType::Bracket => "Bracket",
            TokenType::Value => "Value"
        }
    }
}

/// Structure used to hold information about token
/// its type, position in code, captured string
pub struct TokenData<'a>{
    text: &'a str,
    interval: (usize, usize),
    type_: TokenType
}

impl<'a> TokenData<'a>{
    /// Returns `TokenData` instance
    fn new(text: &'a str, interval: (usize, usize), type_: TokenType)->Self{
        Self{text, interval, type_}
    }

    /// Returns string part captured iside token
    pub fn token_str(&self)->&str{
        &self.text[self.interval.0..self.interval.1]
    }

    /// returns reference to source code
    pub fn code(&self)->&str{
        &self.text
    }

    pub fn get_type(&self)->TokenType{
        self.type_
    }

    pub fn get_interval(&self)->(usize, usize){
        self.interval
    }

    pub fn get_pos(&self)->usize{
        self.interval.0
    }
}

impl<'a> fmt::Debug for TokenData<'a>{
    fn fmt(&self, f: &mut fmt::Formatter)->Result<(), fmt::Error>{
        write!(f, "TokenData(Type: {}, text: \"{}\", interval: {:?})", self.type_.as_str(), self.token_str(), self.interval)
    }
}

impl<'a> fmt::Display for TokenData<'a>{
    fn fmt(&self, f: &mut fmt::Formatter)->Result<(), fmt::Error>{
        write!(f, "{}(\"{}\")", self.type_.as_str(), self.token_str())
    }
}


type CharsIter<'a> = Peekable<CharIndices<'a>>;

/// Struct used to tokenize code text
pub struct Preprocessor<'a>{
    code_text: &'a str,
}


impl<'a> Preprocessor<'a>{
    /// Returns new instance of `Preprocessor`
    pub fn new(code_text: &'a str)->Self{
        Self{
            code_text,
        }
    }

    /// Returns `Vec` of `TokenData` corresponding to provided code
    pub fn tokenize(&'a self)->Result<Vec<TokenData<'a>>, Error<'a>>{
        let mut tokens = vec![];
        let mut char_iter = self.get_char_iter();
        while let Some(token) = self.eat_token(&mut char_iter){
            match token{
                Ok(t) => {
                    tokens.push(t)
                },
                Err(e) => {
                    return Err(e);
                }
            }
        }
        Ok(tokens)
    }

    fn get_char_iter(&self)->CharsIter<'a>{
        self.code_text
            .char_indices()
            .peekable()
    }

    /// Returns `TokenData` for next token if possible. Return `None` if provided iterator is empty.
    /// Shifts iterator to position of character atfer token, or to the end if no such.
    fn eat_token(&self, char_iter: &mut CharsIter<'a>)->Option<Result<TokenData<'a>, Error<'a>>>{
        loop{
            let first = char_iter.peek().map(|&x| x);
            if let Some((pos, f)) = first{ // guarantees what iterator has at least 1 character, so non of functions will panic
                return Some(
                    if f.is_alphabetic() || f == '_' { 
                        self.eat_name(char_iter) 
                    }else if f.is_digit(10) || f == '.'{
                        self.eat_numeric(char_iter)
                    }else if f == '"'{
                        self.eat_string(char_iter)
                    }else if f == ' ' || f == '\n' || f == '\r'{
                        char_iter.next().unwrap();
                        continue;
                    }else if is_bracket(f){
                        self.eat_bracket(char_iter)
                    }else if is_operator(f){
                        self.eat_operator(char_iter)  
                    }else{
                        Err(self.error(ERROR_UNKNOWN_TOKEN_TYPE, pos))
                    }
                );
            }else{
                return None;
            }
        }
    }

    /// Returns `TokenData` corresponding to upcoming name. Shift iterator character to symbol after it (or to end)
    /// 
    /// `char_iter` must be finite.
    /// 
    /// # Panics
    /// panic if `char_iter` is empty
    fn eat_name(&self, char_iter: &mut CharsIter<'a>)->Result<TokenData<'a>, Error<'a>>{
        Ok(TokenData::new(
            self.code_text,
            self.get_matching_range(char_iter, |x| x.is_alphabetic() || x.is_digit(10) || x == '_'),
            TokenType::Name
        ))
    }

    /// Returns bracket `TokenData`
    /// 
    /// # Panics
    /// panic if `char_iter` is empty
    fn eat_bracket(&self, char_iter: &mut CharsIter<'a>)->Result<TokenData<'a>, Error<'a>>{
        let (pos, char_) = char_iter.next().unwrap();
        Ok(TokenData::new(self.code_text, (pos, pos+char_.len_utf8()), TokenType::Bracket))
    }

    /// Returns operator `TokenData`
    /// 
    /// # Panics
    /// panic if `char_iter` iterator is empty
    fn eat_operator(&self, char_iter: &mut CharsIter<'a>)->Result<TokenData<'a>, Error<'a>>{
        Ok(TokenData::new(
            self.code_text, 
            self.get_matching_range(char_iter, |x| is_operator(x)),
            TokenType::Operator
        ))
    }

    /// Returns value `TokenData` or '.' operator `TokenData`
    /// 
    /// # Panics
    /// panic if `char_iter` iterator is empty
    fn eat_numeric(&self, char_iter: &mut CharsIter<'a>)->Result<TokenData<'a>, Error<'a>>{
        let int_range = self.get_matching_range(char_iter, |x| x.is_digit(10));
        let _dot_range = if let Some(_) = char_iter.peek(){
            self.get_matching_range(char_iter, |x| x == '.')
        }else{
            int_range   
        };
        let _float_range = if let Some(_) = char_iter.peek(){
            self.get_matching_range(char_iter, |x| x.is_digit(10))
        }else{
            _dot_range  
        };
        let _exp = if let Some(_) = char_iter.peek(){
            self.get_matching_range(char_iter, |x| x == 'e')
        }else{
            _float_range  
        };
        let _minus = if let Some(_) = char_iter.peek(){
            self.get_matching_range(char_iter, |x| x == '-')
        }else{
            _exp  
        };
        let exp_value = if let Some(_) = char_iter.peek(){
            self.get_matching_range(char_iter, |x| x.is_digit(10))
        }else{
            _minus
        };
        Ok(TokenData::new(self.code_text, (int_range.0, exp_value.1), TokenType::Value))
    }


    fn eat_string(&self, char_iter: &mut CharsIter<'a>)->Result<TokenData<'a>, Error<'a>>{
        let (start_pos, _) = char_iter.next().unwrap(); // eat '"' symbol
        while let Some((pos, char_)) = char_iter.next(){
            if char_ == '\\'{
                let _ = char_iter.next(); // just need to skip next character.
            }else if char_ == '"'{
                return Ok(TokenData::new(self.code_text, (start_pos, pos + char_.len_utf8()), TokenType::Value));
            }
        }
        Err(self.error(ERROR_UNCLOSED_STRING, start_pos))
    }

    /// Returns largest interval from start, on which every character match filter, and shift
    /// iterator to first characted after (Or to the end if no such)
    /// 
    /// # Panics
    /// panic if `char_iter` iterator is empty
    fn get_matching_range<Func: Fn(char)->bool>(&self, char_iter: &mut CharsIter<'a>, filter: Func)->(usize, usize){
        let (start_pos, char_) = char_iter.peek().unwrap().clone();
        let mut end_pos = start_pos;
        let mut prev_char = char_;
        loop{
            let peek = char_iter.peek().map(|&x| x);
            match peek{
                Some((pos, c)) if filter(c) => {
                    char_iter.next().unwrap(); // cannot panic, since existance of next character checked above.
                    end_pos = pos;
                    prev_char = c;
                },
                Some((pos, _)) => {
                    return (start_pos, pos);
                },
                None => {
                    return (start_pos, end_pos + prev_char.len_utf8());
                }
            }
        }
    }

    /// Returns error of give error_code and its position in text
    fn error(&self, code: usize, where_: usize)->Error<'a>{
        Error::new(self.code_text, code, where_)
    }
}


fn is_bracket(c: char)->bool{
    "[](){}".contains(c)
}

fn is_operator(c: char)->bool{
    "=+-*/%&|!^<>,.;".contains(c)
}


#[cfg(test)]
mod tests{
    use super::*;

    
    #[test]
    fn test_lexing_name(){
        let lexer = Preprocessor::new("name something");
        let mut char_iter = lexer.get_char_iter();
        
        let t = lexer.eat_token(&mut char_iter).unwrap().ok().unwrap();
        assert_eq!(t.type_.as_str(), "Name");
        assert_eq!(t.token_str(), "name");
        assert_eq!(t.interval, (0, 4));

        let t = lexer.eat_token(&mut char_iter).unwrap().ok().unwrap();
        assert_eq!(t.type_.as_str(), "Name");
        assert_eq!(t.token_str(), "something");
        assert_eq!(t.interval, (5, 14));
    }

    #[test]
    fn test_lexing_operator(){
        let lexer = Preprocessor::new("+= -");
        let mut char_iter = lexer.get_char_iter();

        let t = lexer.eat_token(&mut char_iter).unwrap().ok().unwrap();
        assert_eq!(t.type_.as_str(), "Operator");
        assert_eq!(t.token_str(), "+=");
        assert_eq!(t.interval, (0, 2));

        let t = lexer.eat_token(&mut char_iter).unwrap().ok().unwrap();
        assert_eq!(t.type_.as_str(), "Operator");
        assert_eq!(t.token_str(), "-");
        assert_eq!(t.interval, (3, 4));
    }

    #[test]
    fn test_lexing_bracket(){
        let lexer = Preprocessor::new("(]");
        let mut char_iter = lexer.get_char_iter();

        let t = lexer.eat_token(&mut char_iter).unwrap().ok().unwrap();
        assert_eq!(t.type_.as_str(), "Bracket");
        assert_eq!(t.token_str(), "(");
        assert_eq!(t.interval, (0, 1));

        let t = lexer.eat_token(&mut char_iter).unwrap().ok().unwrap();
        assert_eq!(t.type_.as_str(), "Bracket");
        assert_eq!(t.token_str(), "]");
        assert_eq!(t.interval, (1, 2));
    }

    #[test]
    fn test_mixed(){
        let lexer = Preprocessor::new("(x + 4) * function(\"qwe\\\" \\n\", b)");
        let data = lexer.tokenize().unwrap();
        let data: Vec<_> = data.iter().map(|td| (td.type_.as_str(), td.token_str(), td.interval)).collect();
        assert_eq!(&data, &[
            ("Bracket", "(", (0, 1)),
            ("Name", "x", (1, 2)),
            ("Operator", "+", (3, 4)),
            ("Value", "4", (5, 6)),
            ("Bracket", ")", (6, 7)),
            ("Operator", "*", (8, 9)),
            ("Name", "function", (10, 18)),
            ("Bracket", "(", (18, 19)),
            ("Value", "\"qwe\\\" \\n\"", (19, 29)),
            ("Operator", ",", (29, 30)),
            ("Name", "b", (31, 32)),
            ("Bracket", ")", (32, 33)),
        ]);
    }

    use test::Bencher;
    #[bench]
    fn bench_tokenize(bench: &mut Bencher){
        let preprocessor = Preprocessor::new("int x=0; int x(int a){return a + 4; for(int i=0; i<a; ++i) a += 3 * 4;}");
        bench.iter(||{
            let _data = preprocessor.tokenize();
        });
    }
}
