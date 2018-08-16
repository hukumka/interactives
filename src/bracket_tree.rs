//! ```
//! let code = "int main(int x){int b=x+3; return b;}";
//! let preprocessor = Preprocessor::new(code);
//! let tokens = preprocessor.tokenize().unwrap();
//!
//! // Creating bracket tree from token array
//! let bracket_tree = BracketTree::new(&tokens).unwrap();
//! 
//! // Create walker
//! let mut walker = bracket_tree.walker();
//! // take first two names (int and main)
//! let int_tok = walker.expect_name().unwrap();
//! let main_tok = walker.expect_name().unwrap();
//!
//! // take layer of arguments
//! let mut args_walker = walker.expect_layer("(").unwrap();
//! let int2_tok = args.walker.expect_name().unwrap();
//! let x_tok = args_walker.expect_name().unwrap();
//! args_walker.expect_empty().unwrap();
//!
//! // if token expectations do not satisfied walker dont move
//! assert!(walker.expect_name().is_none());
//! // and one can keep using it as nothing happened
//! let mut body_walker = walker.expect_layer("{").unwrap();
//! // Also, it's not nessesary to exhaust inner layer before
//! // continuing use of parent. This also can allow some async
//! // parsing.
//! walker.expect_empty();
//!
//! body_walker.expect_name().unwrap();
//! body_walker.expect_name().unwrap();
//! body_walker.expect_exact_operator("=").unwrap();
//! body_walker.expect_name().unwrap();
//! body_walker.expect_operator().unwrap();
//! body_walker.expect_value().unwrap();
//! body_walker.expect_exact_operator(";").unwrap();
//! /// ...
//! 
//! ```

use std::fmt;

use lexer::{TokenData, TokenType};
use error::Error;
use error::bracket_tree_parser::*;


pub struct BracketTree<'a>{
	/// tokens
	data: &'a [TokenData<'a>],
	/// depth of current bracket layer in tree; 
	/// 
	/// brackets lay on same layer as everything outside
	/// them, everything inside them lay one layer deeper
	depth_marker: Vec<usize>,
	/// position of closing bracket for current layer in tree
	bracket_end: Vec<usize>,
}

impl<'a> BracketTree<'a>{
    /// Creates new instance of BracketTree.
    /// If brackets ('()' '{}' '[]') do not match, returns ```Error```
	pub fn new(data: &'a [TokenData<'a>])->Result<Self, Error>{
	    let mut depth_marker = vec![0; data.len()];
	    let mut bracket_end = vec![0; data.len()];
	
	    let mut current_depth = 0; // holding depth of current layer
	    let mut current_bracket_end = data.len(); // holding position of closing bracket for current layer, or data.len() for root layer
	
	    // going in reverse to get closing bracket for each position easier.
	    for (i, token) in data.iter().enumerate().rev(){
	        match token.get_type(){
	            TokenType::Bracket if is_close_bracket(token.token_str()) => {
	                // marking bracket
	                depth_marker[i] = current_depth;
	                bracket_end[i] = current_bracket_end;
	                // entering new layer ending with found bracket
	                current_depth+=1;
	                current_bracket_end = i;
	            },
	            TokenType::Bracket if is_open_bracket(token.token_str()) => {
	                // leaving bracket pair
	                if current_depth == 0 {
	                    return Err(Error::new(token.code(), ERROR_UNCLOSED_BRACKET, token.get_interval().0));
	                }
	                if !are_brackets_match(token.token_str(), data[current_bracket_end].token_str()){
	                    return Err(Error::new(token.code(), ERROR_BRACKET_DONT_MATCH, token.get_interval().0));
	                }
	                // leaving layer
	                current_depth-=1;
	                current_bracket_end = if current_bracket_end + 1 == data.len(){
	                    data.len()
	                }else{
	                    bracket_end[current_bracket_end]
	                };
	                // marking opening bracket
	                depth_marker[i] = current_depth;
	                bracket_end[i] = current_bracket_end;
	            },
	            _ => {
	                // marking non-bracket element
	                depth_marker[i] = current_depth;
	                bracket_end[i] = current_bracket_end;
	            }
	        }
	    }
	    if current_bracket_end < data.len(){
	        Err(Error::new(
	            data[current_bracket_end].code(), 
	            ERROR_TO_MANY_CLOSING_BRACKETS, 
	            data[current_bracket_end].get_interval().0
	        ))
	    }else{
	        Ok(Self{data, depth_marker, bracket_end})
	    }
	}
	
    /// Create new instance of ```BracketTreeWalker```.
    /// Used to access tokens and layers
	pub fn walker(&'a self)->BracketTreeWalker<'a>{
	    BracketTreeWalker{pos: 0, end: self.data.len(), tree: self}
	}
}

#[derive(Clone)]
/// Iterator-like structure designated to navigate over raw array of tokens.
pub struct BracketTreeWalker<'a>{
	pos: usize,
	end: usize,
	tree: &'a BracketTree<'a>
}

impl<'a> fmt::Debug for BracketTreeWalker<'a>{
    fn fmt(&self, formatter: &mut fmt::Formatter)->fmt::Result{
        write!(formatter, "BracketTreeWalker{{pos: {}, end: {}}}", self.pos, self.end)
    }
}

impl<'a> BracketTreeWalker<'a>{
    /// If current token is an opening bracket create new ```BracketTreeWalker``` limited
    /// to sub-tree between it and its matching closing bracket, and then move to token
    /// after closing bracket. returns created walker.
    ///
    /// Otherwise returns None
    pub fn expect_layer(&mut self, opening_bracket: &str)->Option<BracketTreeWalker<'a>>{
        assert!(is_open_bracket(opening_bracket));
        if self.pos == self.end{
            return None;
        }
        let pos = self.pos;
        let token = &self.tree.data[pos];
        if token.get_type() == TokenType::Bracket && token.token_str() == opening_bracket{
            let end = if self.tree.depth_marker[pos+1] > self.tree.depth_marker[pos]{
                self.tree.bracket_end[pos+1]
            }else{
                // corresponds to empty brackets case, just skip them
                pos + 1
            };
            self.pos = end + 1;
            // cloned not tree, but reference to it
            Some(BracketTreeWalker{pos: pos+1, end: end, tree: self.tree})
        }else{
            None
        }
    }

    /// walk back over previous token
    /// panic if tries to put back a layer
    pub fn put_back(&mut self){
        self.pos-=1;
        if self.tree.data[self.pos].get_type() == TokenType::Bracket{
            unimplemented!("Cannot put_back a layer!"); // TODO implement this
        }
    }

    /// If current token is a ```TokenType::Name``` returns it and move current to next token.
    /// Otherwise returns None.
    pub fn expect_name(&mut self)->Option<&'a TokenData<'a>>{
        self.expect_token_of_type_checked(TokenType::Name, |_| true)
    }

    /// If current token is a ```TokenType::Name``` and its ```.token_str()``` equal to ```name``
    /// returns it and move current to next token.
    /// Otherwise returns None.
    pub fn expect_exact_name(&mut self, name: &str)->Option<&'a TokenData<'a>>{
        self.expect_token_of_type_checked(TokenType::Name, |x| x == name)
    }

    /// If current token is a ```TokenType::Value``` returns it and move current to next token.
    /// Otherwise returns None.
    pub fn expect_value(&mut self)->Option<&'a TokenData<'a>>{
        self.expect_token_of_type_checked(TokenType::Value, |_| true)
    }

    /// If current token is a ```TokenType::Operator``` and its ```.token_str()``` equal to ```op``
    /// returns it and move current to next token.
    /// Otherwise returns None.
    pub fn expect_exact_operator(&mut self, op: &str)->Option<&'a TokenData<'a>>{
        self.expect_operator_checked(|x| x == op)
    }

    /// If current token is a ```TokenType::Operator```
    /// returns it and move current to next token.
    /// Otherwise returns None.
    pub fn expect_operator(&mut self)->Option<&'a TokenData<'a>>{
        self.expect_operator_checked(|_| true)
    }

    /// If current token is a ```TokenType::Operator``` and its ```.token_str()``` match ```checker``
    /// returns it and move current to next token.
    /// Otherwise returns None.
    pub fn expect_operator_checked<T: FnOnce(&str)->bool>(&mut self, checker: T)->Option<&'a TokenData<'a>>{
        self.expect_token_of_type_checked(TokenType::Operator, checker)
    }

    /// If current token has type ```type_``` and its ```.token_str()``` match ```checker```
    /// return it and move current to next token
    /// Otherwise returns None.
    fn expect_token_of_type_checked<T: FnOnce(&str)->bool>(&mut self, type_: TokenType, checker: T)->Option<&'a TokenData<'a>>{
        if self.pos == self.end{
            return None;
        }
        let token = &self.tree.data[self.pos];
        if token.get_type() == type_ && checker(token.token_str()){
            self.pos += 1;
            Some(token)
        }else{
            None
        }
    }

    /// Check if walker reached its end
    pub fn is_empty(&self)->bool{
        self.pos >= self.end
    }

    /// Get code on which tokens were build
    pub fn code(&self)->&'a str{
        if self.pos < self.tree.data.len(){
            self.tree.data[self.pos].code()
        }else{
            self.tree.data.last().unwrap().code()
        }
    }

    /// Get current position
    pub fn get_pos(&self)->usize{
        self.pos
    }

    /// Get current position in code
    pub fn get_text_pos(&self)->usize{
        if self.pos < self.tree.data.len(){
            self.tree.data[self.pos].get_interval().0
        }else{
            self.tree.data.last().unwrap().get_interval().1
        }
    }

    /// Return Some if walker is empty, otherwise return None
    pub fn expect_empty(&self)->Option<()>{
        if self.is_empty(){
            Some(())
        }else{
            None
        }
    }
}


/// Check if ```s``` is opening bracket
fn is_open_bracket(s: &str)->bool{
    "([{".contains(s)
}

/// Check if ```s``` is closing bracket
fn is_close_bracket(s: &str)->bool{
    ")]}".contains(s)
}

/// Check if ```s1``` and ```s2``` are matching opening and closing bracket correspondedly
fn are_brackets_match(s1: &str, s2: &str)->bool{
    let i1 = "([{".find(s1);
    let i2 = ")]}".find(s2);

    match (i1, i2){
        (Some(x), Some(y)) if x == y => true,
        _ => false
    }
}


#[cfg(test)]
mod tests{
    use super::*;
    use lexer::Preprocessor;

    #[test]
    fn test_new_bracket_tree(){
        let lexer = Preprocessor::new("function(a, b){a(b) + b}");
        let tokens = lexer.tokenize().unwrap();
        let tree = BracketTree::new(&tokens).unwrap();
        assert_eq!(
            tree.depth_marker, 
            //function (  a  ,  b  )  {  a  (  b  )  +  b  }
            &[0,       0, 1, 1, 1, 0, 0, 1, 1, 2, 1, 1, 1, 0]
        );
        assert_eq!(
            tree.bracket_end,
            // 0       1   2  3  4  5   6   7   8   9   10  11  12  13; 14
            //function (   a  ,  b  )   {   a   (   b   )   +   b   }
            &[14,      14, 5, 5, 5, 14, 14, 13, 13, 10, 13, 13, 13, 14]
        );

        let lexer = Preprocessor::new("function(a, ");
        let tokens = lexer.tokenize().unwrap();
        let tree = BracketTree::new(&tokens);
        match tree{
            Err(e) => {
                assert_eq!(e.code(), ERROR_UNCLOSED_BRACKET);
            },
            _ => panic!("Expected error 0x100")
        }

        let lexer = Preprocessor::new("function(a, c}");
        let tokens = lexer.tokenize().unwrap();
        let tree = BracketTree::new(&tokens);
        match tree{
            Err(e) => {
                assert_eq!(e.code(), ERROR_BRACKET_DONT_MATCH);
            },
            _ => panic!("Expected error 0x101")
        }

        let lexer = Preprocessor::new("function a, c}");
        let tokens = lexer.tokenize().unwrap();
        let tree = BracketTree::new(&tokens);
        match tree{
            Err(e) => {
                assert_eq!(e.code(), ERROR_TO_MANY_CLOSING_BRACKETS);
            },
            _ => panic!("Expected error 0x102")
        }

        // 
        let lexer = Preprocessor::new("()[()]");
        let tokens = lexer.tokenize().unwrap();
        let _tree = BracketTree::new(&tokens).unwrap();
    }

    macro_rules! walker_expect_only{
        (#expect_name $walker: expr) => {
            {
                assert!($walker.expect_layer("(").is_none());
                assert!($walker.expect_operator().is_none());
                assert!($walker.expect_value().is_none());
                let res = $walker.expect_name();
                assert!(res.is_some());
                res.unwrap()
            }
        };
        (#expect_value $walker: expr) => {
            {
                assert!($walker.expect_layer("(").is_none());
                assert!($walker.expect_operator().is_none());
                assert!($walker.expect_name().is_none());
                let res = $walker.expect_value();
                assert!(res.is_some());
                res.unwrap()
            }
        };
        (#expect_operator $walker: expr) => {
            {
                assert!($walker.expect_layer("(").is_none());
                assert!($walker.expect_value().is_none());
                assert!($walker.expect_name().is_none());
                let res = $walker.expect_operator();
                assert!(res.is_some());
                res.unwrap()
            }
        };
        (#expect_layer $walker: expr, $bracket: expr) => {
            {
                assert!($walker.expect_operator().is_none());
                assert!($walker.expect_value().is_none());
                assert!($walker.expect_name().is_none());
                let res = $walker.expect_layer($bracket);
                assert!(res.is_some());
                res.unwrap()
            }
        }
    }

    #[test]
    fn test_walker(){
        let lexer = Preprocessor::new("function(a, b){a, b, 1}");
        let tokens = lexer.tokenize().unwrap();
        let tree = BracketTree::new(&tokens).unwrap();
        let mut walker = tree.walker();

        let function = walker_expect_only!(#expect_name walker);
        assert_eq!(function.token_str(), "function");
        let mut args_walker = walker_expect_only!(#expect_layer walker, "(");
        {
            let a = walker_expect_only!(#expect_name args_walker);
            assert_eq!(a.token_str(), "a");
            let coma = walker_expect_only!(#expect_operator args_walker);
            assert_eq!(coma.token_str(), ",");
            let b = walker_expect_only!(#expect_name args_walker);
            assert_eq!(b.token_str(), "b");
            assert!(args_walker.is_empty());
        }
        let mut body_walker = walker_expect_only!(#expect_layer walker, "{");
        {
            let a = walker_expect_only!(#expect_name body_walker);
            assert_eq!(a.token_str(), "a");
            let coma = walker_expect_only!(#expect_operator body_walker);
            assert_eq!(coma.token_str(), ",");
            let b = walker_expect_only!(#expect_name body_walker);
            assert_eq!(b.token_str(), "b");
            let coma = walker_expect_only!(#expect_operator body_walker);
            assert_eq!(coma.token_str(), ",");
            let one = walker_expect_only!(#expect_value body_walker);
            assert_eq!(one.token_str(), "1");
            assert!(body_walker.is_empty());
        }
        assert!(walker.is_empty());
    }

    use test::Bencher;
    #[bench]
    fn bench_bracket_tree(bench: &mut Bencher){
        let preprocessor = Preprocessor::new("int x=0; int x(int a){return a + 4; for(int i=0; i<a; ++i) a += 3 * 4;}");
        let data = preprocessor.tokenize().unwrap();
        bench.iter(||{
            let _tree = BracketTree::new(&data).unwrap();
        });
    }

}
