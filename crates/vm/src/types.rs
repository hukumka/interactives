//! VirtualMachine supported types


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
pub trait ArbitraryData{
    /// type of error returned then trying access value 
    /// with sertain type and failing
    type Error;

    fn as_int(&self)->Result<i32, Self::Error>;
    fn from_int(value: i32)->Self;

    fn as_float(&self)->Result<f32, Self::Error>;
    fn from_float(value: f32)->Self;
}


/// Type safe (checking) implementation of ```ArbitraryData```
pub enum ArbitraryDataEnum{
    Int(i32),
    Float(f32)
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
}
