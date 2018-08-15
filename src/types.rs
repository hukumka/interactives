use syntax_tree::{
    Type as TypeIm,
    FunctionType as FunctionTypeIm,
    TypeBase as TypeBaseIm,
    FunctionDefinition,
    FunctionDeclaration
};


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct FunctionType{
    pub ret: Type,
    pub args: Vec<Type>
}

impl FunctionType{
    pub fn from_definition<'a>(f: &'a FunctionDefinition<'a>)->Result<Self, &'a TypeIm<'a>>{
        Self::from_declaration(&f.decl)
    }
    pub fn from_declaration<'a>(f: &'a FunctionDeclaration<'a>)->Result<Self, &'a TypeIm<'a>>{
        let ret = Type::from_type(&f.ret_name.type_)?;
        let args: Result<Vec<_>, &'a TypeIm<'a>> = f.arguments.iter().map(|v|{
            Type::from_type(&v.type_)
        }).collect();
        let args = args?;
        Ok(Self{ret, args})
    }
    pub fn from_func_type<'a>(f: &'a FunctionTypeIm<'a>)->Result<Self, &'a TypeIm<'a>>{
        let ret = Type::from_type(&f.ret)?;
        let args: Result<Vec<_>, &'a TypeIm<'a>> = f.args.iter().map(|v|{
            Type::from_type(v)
        }).collect();
        let args = args?;
        Ok(Self{ret, args})
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type{
    pub base: BaseType,
    pub pointer_count: u8,
    pub final_: bool,
}

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum BaseType{
    Int,
    Float,
    Void,
    Function(Box<FunctionType>),
}


impl Type{
    pub fn pointer(&self)->Type{
        Self{base: self.base.clone(), pointer_count: self.pointer_count+1, final_: false}
    }

    pub fn function_pointer(type_: FunctionType)->Type{
        Self{
            base: BaseType::Function(Box::new(type_)), 
            pointer_count: 0,
            final_: true,
        }
    }

    pub fn dereference(&self)->Option<Type>{
        if self.pointer_count > 0 {
            Some(Self { base: self.base.clone(), pointer_count: self.pointer_count - 1, final_: false})
        }else{
            None
        }
    }

    // base type constructors
    pub fn int()->Self{
        Self{base: BaseType::Int, pointer_count: 0, final_: false}
    }

    pub fn float()->Self{
        Self{base: BaseType::Float, pointer_count: 0, final_: false}
    }
    
    pub fn void()->Self{
        Self{base: BaseType::Void, pointer_count: 0, final_: false}
    }

    pub fn from_type<'a>(t: &'a TypeIm<'a>)->Result<Self, &'a TypeIm<'a>>{
        Ok(Self{base: Self::from_base(t)?, pointer_count: t.pointer_count as u8, final_: false})
    }

    fn from_base<'a>(t: &'a TypeIm<'a>)->Result<BaseType, &'a TypeIm<'a>>{
        match t.base{
            TypeBaseIm::Base(t) if t.token_str() == "int" => Ok(BaseType::Int),
            TypeBaseIm::Base(t) if t.token_str() == "void" => Ok(BaseType::Void),
            TypeBaseIm::Base(t) if t.token_str() == "float" => Ok(BaseType::Float),
            TypeBaseIm::Function(box ref  func) => {
                let func = FunctionType::from_func_type(&func)?;
                Ok(BaseType::Function(Box::new(func)))
            },
            _ => Err(t)
        }
    }

    pub fn autocast(&self, into: &Type)->bool{
        (self.base == into.base && self.pointer_count == into.pointer_count)
            || (self == &Type::int() && into == &Type::float())
    }

    
}


impl BaseType{
    
}
