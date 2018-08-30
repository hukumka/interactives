//! VirtualMachine supported types

use std::fmt::Debug;
use std::ops::{Index, IndexMut};


/// Trait for data structure represention arbitrary data
/// used in virtual machine
///
/// Currently supported types:
/// + int(i32)
/// + float(f32)
///
/// for every type supported by machine ```ArbitraryData```
/// provides following functions:
/// 
/// + fn as_$type_name(&self)->Result<$type, Self::Error>;
/// + fn from_$type_name($type)->Self;
///
/// where
/// + $type_name - name for type in C (eg int, float)
/// + $type - type used in rust (eg i32, f32)
pub trait ArbitraryData: Debug + Clone + Default{
    /// type of error returned then trying access value 
    /// with sertain type and failing
    type Error: Debug;

    fn as_int(&self)->Result<i32, Self::Error>;
    fn from_int(value: i32)->Self;

    fn as_float(&self)->Result<f32, Self::Error>;
    fn from_float(value: f32)->Self;

    fn access_error()->Self::Error;
}


/// Type safe (checking) implementation of ```ArbitraryData```
#[derive(Clone, Debug)]
pub enum ArbitraryDataEnum{
    Int(i32),
    Float(f32),
    Void,
}


impl Default for ArbitraryDataEnum{
    fn default()->Self{
        ArbitraryDataEnum::Void
    }
}


impl ArbitraryData for ArbitraryDataEnum{
    type Error = ();

    fn as_int(&self)->Result<i32, Self::Error>{
        match self{
            ArbitraryDataEnum::Int(x) => Ok(*x),
            _ => Err(())
        }
    }
    fn from_int(value: i32)->Self{
        ArbitraryDataEnum::Int(value)
    }


    fn as_float(&self)->Result<f32, Self::Error>{
        match self{
            ArbitraryDataEnum::Float(x) => Ok(*x),
            _ => Err(())
        }
    }
    fn from_float(value: f32)->Self{
        ArbitraryDataEnum::Float(value)
    }

    fn access_error()->Self::Error{
        ()
    }
}


/// references local variable or temporary value
#[derive(Copy, Clone, Debug)]
pub struct LocalDataPointer(pub usize);


/// Stack allocated data storage
pub struct DataStack<Data>{
    /// data
    stack: Vec<Data>,
    /// stack offset
    stack_pointer: LocalDataPointer,

    /// void value, reference to which returned in case of 
    /// out of bound indexing (to avoid panicing)
    void: Data,
}


impl<Data: ArbitraryData> Index<&LocalDataPointer> for DataStack<Data>{
    type Output = Data;
    /// interface to access data from stack
    fn index(&self, pointer: &LocalDataPointer)->&Data{
        let id = self.stack_pointer.0 + pointer.0;
        if id < self.stack.len(){
            &self.stack[id]
        }else{
            &self.void
        }
    }

}

impl<Data: ArbitraryData> IndexMut<&LocalDataPointer> for DataStack<Data>{
    /// interface to mutate data in stack
    ///
    /// will automaticly reallocate if needed
    fn index_mut<'a>(&mut self, pointer: &LocalDataPointer)->&mut Data{
        let id = self.stack_pointer.0 + pointer.0;
        if id >= self.stack.len(){
            const OVERHEAD: usize = 10;
            self.stack.resize_default(id + OVERHEAD);
        }
        &mut self.stack[id]
    }
}


impl<Data: ArbitraryData> DataStack<Data>{
    /// crete new instance of DataStack
    pub fn new()->Self{
        Self{
            stack: vec![],
            stack_pointer: LocalDataPointer(0),
            void: Data::default(),
        }
    }

    pub fn stack_pointer_add(&mut self, value: i32){
        if value < 0{
            self.stack_pointer.0 -= (-value) as usize;
        }else{
            self.stack_pointer.0 += value as usize;
        }
    }

    pub fn global_pointer(&self, pointer: i32)->Result<&Data, Data::Error>{
        let pointer = pointer as usize;
        if pointer < self.stack.len(){
            Ok(&self.stack[pointer as usize])
        }else{
            Err(Data::access_error())
        }
    }

    pub fn global_pointer_mut(&mut self, pointer: i32)->Result<&mut Data, Data::Error>{
        let pointer = pointer as usize;
        if pointer < self.stack.len(){
            Ok(&mut self.stack[pointer as usize])
        }else{
            Err(Data::access_error())
        }
    }
}
