use syntax_tree::{
    Type as TypeIm,
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
        let ret = Type::from_type(&f.return_type)?;
        let args: Result<Vec<_>, &'a TypeIm<'a>> = f.arguments.iter().map(|v|{
            Type::from_type(&v.type_)
        }).collect();
        let args = args?;
        Ok(Self{ret, args})
    }
    pub fn from_declaration<'a>(f: &'a FunctionDeclaration<'a>)->Result<Self, &'a TypeIm<'a>>{
        let ret = Type::from_type(&f.return_type)?;
        let args: Result<Vec<_>, &'a TypeIm<'a>> = f.arguments.iter().map(|v|{
            Type::from_type(&v.type_)
        }).collect();
        let args = args?;
        Ok(Self{ret, args})
    }
}


#[derive(Debug, Clone, Eq, PartialEq)]
pub struct Type{
    pub base: BaseType,
    pub pointer_count: u8,
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
        Self{base: self.base.clone(), pointer_count: self.pointer_count+1}
    }

    pub fn function_pointer(type_: FunctionType)->Type{
        Self{
            base: BaseType::Function(Box::new(type_)), 
            pointer_count: 0
        }
    }

    pub fn dereference(&self)->Option<Type>{
        if self.pointer_count > 0 {
            Some(Self { base: self.base.clone(), pointer_count: self.pointer_count - 1})
        }else{
            None
        }
    }

    // base type constructors
    pub fn int()->Self{
        Self{base: BaseType::Int, pointer_count: 0}
    }

    pub fn float()->Self{
        Self{base: BaseType::Float, pointer_count: 0}
    }
    
    pub fn void()->Self{
        Self{base: BaseType::Void, pointer_count: 0}
    }

    pub fn from_type<'b, 'a>(t: &'b TypeIm<'a>)->Result<Self, &'b TypeIm<'a>>{
        if let Some(base) = BaseType::from_str(t.base.token_str()){
            Ok(Self{base, pointer_count: t.pointer_count as u8})
        }else{
            Err(t)
        }
    }

    pub fn autocast(&self, into: &Type)->bool{
        (self.base == into.base && self.pointer_count == into.pointer_count)
            || (self == &Type::int() && into == &Type::float())
    }
}


impl BaseType{
    fn from_str(s: &str)->Option<Self>{
        match s{
            "int" => Some(BaseType::Int),
            "void" => Some(BaseType::Void),
            "float" => Some(BaseType::Float),
            _ => None
        }
    }
}
