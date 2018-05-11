use std::collections::HashMap;

use syntax_tree::{
    VariableDefinition,
    Root
};

use error::Error;
use error::precompiler::*;


struct NamespaceStack<'a>{
    /// holds info about global variables present in code
    globals: Namespace<'a>,
    /// holds info about local variables in current scope
    locals: Vec<Namespace<'a>>,
}

impl<'a> NamespaceStack<'a>{
    /// creates new instance of `NamespaceStack`
    fn new(globals: Namespace<'a>)->Self{
        Self{globals, locals: vec![], function_requests: HashMap::new()}
    }

    /// get data about variable and its id for execution
    fn get_id(&self, name: &'a str)->Option<usize>{
        for (i, namespace) in self.locals
            .iter().enumerate()
            .reversed()
        {
            if let Some(id) = namespace.get(name){
                let id = id + self.globals.len()
                    + self.locals[..i].iter().map(|x| x.len()).sum();
                Some(id)
            }
        }

        self.globals.get(name)
    }

    /// get `VariableDefinition` corresponding to variable id for current namespace
    fn get_tree_by_id(&self, mut id: usize)->Option<&'a VariableDefinition>{
        if id < self.globals.len(){
            self.globals.get_tree(id)
        }else{
            id -= self.globals.len();
            for n in self.locals{
                if id < n.len(){
                    return n.get_tree(id);
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
        self.locals.pop()
    }
}


struct Namespace<'a>{
    /// holds info about variables on namespace level and its id on level
    map: HashMap<&'a str, usize>,
    data: Vec<&'a VariableDefinition<'a>>
}

impl<'a> Namespace<'a>{
    fn new()->Self{
        Self{map: HashMap::new()}
    }

    fn register(&mut self, var_def: &'a VariableDefinition<'a>, error_stream: &mut Vec<Error<'a>>)->Option<()>{
        let name_token = var_def.name();
        let name = name_token.token_str();
        if self.map.get(name).is_none(){
            let id = self.data.len();
            self.map.insert(name, id);
            self.data.push(var_def);
            Some(())
        }else{
            let error = Error::new(
                ERROR_PRECMP_VARIABLE_REDEFINITION,
                name_token
            );
            error_stream.push(error);
            None
        }
    }

    fn len(&self)->usize{
        self.data.len()
    }

    fn get(&self, name: &'a str)->Option<usize>{
        self.map.get(name)
    }

    fn get_tree(&self, id: usize)->Option<&'a VariableDefinition<'a>>{
        self.data[id]
    }

}