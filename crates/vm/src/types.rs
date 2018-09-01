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
/// + ```fn as_$type_name(&self)->Result<$type, Self::Error>;```
/// + ```fn from_$type_name($type)->Self;```
///
/// Expected invariant:
///
/// + ```assert_eq!(Self::from_$type_name(x).as_$type_name(), Ok(x));```
///
/// where
/// + ```$type_name``` - name for type in C (eg int, float)
/// + ```$type``` - type used in rust (eg i32, f32)
///
pub trait ArbitraryData: Debug + Clone + Default{
    /// type of error returned then trying access value 
    /// with sertain type and failing
    type Error: Debug;

    /// take integer from cell
    fn as_int(&self)->Result<i32, Self::Error>;
    /// create cell with int inside
    fn from_int(value: i32)->Self;

    /// take float from cell
    fn as_float(&self)->Result<f32, Self::Error>;
    /// create cell with float inside
    fn from_float(value: f32)->Self;

    /// generate data access error
    fn access_error(pointer: &LocalDataPointer)->Self::Error;
}


/// Type safe (checking) implementation of ```ArbitraryData```
#[derive(Clone, Debug, PartialEq)]
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

    fn access_error(_pointer: &LocalDataPointer)->Self::Error{
        ()
    }
}


/// references local variable or temporary value
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct LocalDataPointer(pub usize);


/// Stack allocated data storage
#[derive(Debug)]
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

    /// Increase or decrease stack_pointer (moving current stack frame)
    /// by specified value
    pub fn stack_pointer_add(&mut self, value: i32){
        if value < 0{
            self.stack_pointer.0 -= (-value) as usize;
        }else{
            self.stack_pointer.0 += value as usize;
        }
    }

    /// Trying to access value value at position ```pointer```
    pub fn global_pointer(&self, pointer: i32)->Result<&Data, Data::Error>{
        if 0 <= pointer && (pointer as usize) < self.stack.len(){
            Ok(&self.stack[pointer as usize])
        }else{
            let pointer = LocalDataPointer(pointer.abs() as usize);
            Err(Data::access_error(&pointer))
        }
    }

    /// Trying to mutably access value value at position ```pointer```
    pub fn global_pointer_mut(&mut self, pointer: i32)->Result<&mut Data, Data::Error>{
        if 0 <= pointer && (pointer as usize) < self.stack.len(){
            Ok(&mut self.stack[pointer as usize])
        }else{
            let pointer = LocalDataPointer(pointer.abs() as usize);
            Err(Data::access_error(&pointer))
        }
    }

    #[cfg(test)]
    pub(crate) fn raw_data(&self)->&[Data]{
        self.stack.as_slice()
    }

    #[cfg(test)]
    pub(crate) fn raw_stack_offset(&self)->&LocalDataPointer{
        &self.stack_pointer
    }
}


#[cfg(test)]
mod tests{
    use super::*;

    #[test]
    fn test_set_local(){
        // check if stack will automaticly 'allocate' extra space if needed
        let mut data = DataStack::<ArbitraryDataEnum>::new();
        assert_eq!(data.stack.as_slice(), []);
        data[&LocalDataPointer(1)] = ArbitraryDataEnum::from_float(3.2);
        assert_eq!(data.stack[0..2], [ArbitraryDataEnum::Void, ArbitraryDataEnum::Float(3.2)]);
        // In current implementation if stack needs to be resized it resized with certain amount of
        // overhead
        for i in data.stack[2..].iter(){
            assert_eq!(i, &ArbitraryDataEnum::Void);
        }

        // usage in regular circumstances
        data[&LocalDataPointer(0)] = ArbitraryData::from_int(-13);
        assert_eq!(data.stack[0..2], [ArbitraryDataEnum::Int(-13), ArbitraryDataEnum::Float(3.2)]);

        // usage withing function (with stack_pointer > 0)
        data.stack_pointer_add(7);
        data[&LocalDataPointer(1)] = ArbitraryData::from_int(0);
        assert_eq!(data.stack[0..9], [
            ArbitraryDataEnum::Int(-13), 
            ArbitraryDataEnum::Float(3.2),
            ArbitraryDataEnum::Void, 
            ArbitraryDataEnum::Void, 
            ArbitraryDataEnum::Void, 
            ArbitraryDataEnum::Void, 
            ArbitraryDataEnum::Void, 
            ArbitraryDataEnum::Void, 
            ArbitraryDataEnum::Int(0), 
        ]);
    }


    #[test]
    fn test_get_local(){
        let mut data = DataStack::<ArbitraryDataEnum>::new();
        data.stack = vec![
            ArbitraryDataEnum::Void, 
            ArbitraryDataEnum::Int(11), 
            ArbitraryDataEnum::Float(2.71), 
            ArbitraryDataEnum::Void, 
            ArbitraryDataEnum::Float(3.14), 
        ];

        assert_eq!(data[&LocalDataPointer(0)], ArbitraryDataEnum::Void);
        assert_eq!(data[&LocalDataPointer(1)], ArbitraryDataEnum::Int(11));
        assert_eq!(data[&LocalDataPointer(2)], ArbitraryDataEnum::Float(2.71));

        data.stack_pointer_add(2);

        assert_eq!(data[&LocalDataPointer(0)], ArbitraryDataEnum::Float(2.71));
        assert_eq!(data[&LocalDataPointer(1)], ArbitraryDataEnum::Void);
        assert_eq!(data[&LocalDataPointer(2)], ArbitraryDataEnum::Float(3.14));

        data.stack_pointer_add(-1);
        assert_eq!(data[&LocalDataPointer(0)], ArbitraryDataEnum::Int(11));
    }
}

