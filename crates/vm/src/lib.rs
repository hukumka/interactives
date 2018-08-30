//!
//! # Virtual Machine descrition & js bindings
//!
//!
//! 
#![feature(
    rust_2018_preview, 
    uniform_paths,
    vec_resize_default
)]

mod types;

use std::collections::HashSet;

use types::{
    ArbitraryData,
    LocalDataPointer,
    DataStack,
};


/// Instruction representation
///
/// Instruction is primitive to describe minimal
/// amount of work ```VirtualMachine``` can do
pub enum Instruction{
    // ---- ----
    // Jumps
    /// Unconditional jump to specified position
    Jump(CodePointer),

    /// If value in cell referenced by ```LocalDataPointer``` is zero
    /// (interpreted as integer)
    ///
    /// then jump to specified position
    ///
    /// otherwise acts like nop
    JumpZ(CodePointer, LocalDataPointer),

    /// If value in cell referenced by ```LocalDataPointer``` is not zero
    /// (interpreted as integer)
    ///
    /// then jump to specified position
    ///
    /// otherwise acts like nop
    JumpNZ(CodePointer, LocalDataPointer),

    // ---- ----
    // Comparison: Int
    /// calculate binary operator ==
    ///
    /// return value stored as int
    /// + 0 if false
    /// - 1 if true
    IntEq(Binary),

    /// calculate binary operator !=
    ///
    /// return value stored as int
    /// + 0 if false
    /// - 1 if true
    IntNotEq(Binary),

    /// calculate binary operator <
    ///
    /// return value stored as int
    /// + 0 if false
    /// - 1 if true
    IntLess(Binary),

    /// calculate binary operator <=
    ///
    /// return value stored as int
    /// + 0 if false
    /// - 1 if true
    IntLessOrEq(Binary),

    // ---- ----
    // Comparison: Float
    /// calculate binary operator ==
    ///
    /// return value stored as int
    /// + 0 if false
    /// - 1 if true
    FloatEq(Binary),

    /// calculate binary operator !=
    ///
    /// return value stored as int
    /// + 0 if false
    /// - 1 if true
    FloatNotEq(Binary),

    /// calculate binary operator <
    ///
    /// return value stored as int
    /// + 0 if false
    /// - 1 if true
    FloatLess(Binary),

    /// calculate binary operator <=
    ///
    /// return value stored as int
    /// + 0 if false
    /// - 1 if true
    FloatLessOrEq(Binary),

    // ---- ----
    // Arithmetic: Int
    /// Calculate binary operator +
    IntSum(Binary),

    /// Calculate binary operator -
    IntDiff(Binary),

    /// Calculate binary operator *
    IntMul(Binary),

    /// Calculate binary operator /
    IntDiv(Binary),

    /// Calculate binary operator %
    IntMod(Binary),

    // ---- ----
    // Arithmetic: FLoat
    /// Calculate binary operator +
    FloatSum(Binary),

    /// Calculate binary operator -
    FloatDiff(Binary),

    /// Calculate binary operator *
    FloatMul(Binary),

    /// Calculate binary operator /
    FloatDiv(Binary),

    // ---- ----
    // Pointers
    /// Copy value from local variable (or temporary value)
    /// into cell with address stored in local variable pointer
    PointerGet(InstructionPointerGet),

    /// Copy value from cell with address stored in local variable pointer
    /// into local variable (or temporary value)
    PointerSet(InstructionPointerSet),

    // ---- ----
    // Function Call
    /// Increase stack pointer by value
    StackPointerAdd(i32),

    /// Call function with pointer stored in local
    /// variable with offset 0.
    Call,

    /// Return from function execution
    Return,

    // ---- ----
    // Constants
    /// Put value into local variable
    IntConst(LocalDataPointer, i32),

    // ---- ----
    /// Put value into local variable
    FloatConst(LocalDataPointer, f32),

    // ---- ----
    // Type convertions
    /// copy and convert int to float
    IntToFloat(InstructionCopy),

    /// copy and convert float to int
    FloatToInt(InstructionCopy),

    // ---- ----
    // Other
    /// Increment local variable
    /// (int and alike only)
    Inc(LocalDataPointer),

    /// Decrement local variable
    /// (int and alike only)
    Dec(LocalDataPointer),

    /// Do nothing
    Nop,

    /// Copy value from one local variable to other
    Copy(InstructionCopy),
}


/// Instruction::PointerGet representation
///
/// Copy value from local variable (or temporary value)
/// into cell with address stored in local variable pointer
pub struct InstructionPointerGet{
    /// pointer to local variable ( or temporary value) there address value stored
    ///
    /// source
    pointer: LocalDataPointer,
    /// destination of copy
    copy_to: LocalDataPointer
}

/// Instruction::PointerSet representation
///
/// Copy value from cell with address stored in local variable pointer
/// into local variable (or temporary value)
pub struct InstructionPointerSet{
    /// pointer to local variable ( or temporary value) there address value stored
    /// 
    /// destination
    pointer: LocalDataPointer,
    /// source of copy
    copy_from: LocalDataPointer
}


/// Instruction::Copy representation
///
/// Copy value from one local variable to other
pub struct InstructionCopy{
    source: LocalDataPointer,
    destination: LocalDataPointer,
}


/// General binary operator instruction representation
pub struct Binary{
    /// result of instruction will be put here
    pub put_into: LocalDataPointer,
    /// left operand
    pub left: LocalDataPointer,
    /// right operand
    pub right: LocalDataPointer
}


/// references position in code
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub struct CodePointer(usize);


pub struct VirtualMachine<Data>{
    /// compiled code storage
    code: Vec<Instruction>,
    /// current position in code
    instruction_pointer: CodePointer,

    /// local variable and temporary values storage
    data_stack: DataStack<Data>,
    
    /// map from function id to function offset in ```code```
    functions: Vec<CodePointer>,

    /// stack containing return points generated after ```Instruction::Call``
    /// to go back with ```Instruction::Return```
    call_stack: Vec<CodePointer>,

    /// points in code at there execution pauses
    breakpoints: HashSet<CodePointer>,
}

impl<Data: ArbitraryData> VirtualMachine<Data>{
    /// instantiates virtual machine
    ///
    /// Arguments:
    /// 1. code: Vec<Instruction> - provided by compiler or via deserialization
    /// 2. functions: Vec<CodePointer> - mapping from function id to function entrance
    pub fn new(code: Vec<Instruction>, functions: Vec<CodePointer>)->Self{
        Self{
            code,
            functions,
            instruction_pointer: CodePointer(0),

            data_stack: DataStack::new(),

            call_stack: vec![],

            breakpoints: HashSet::new(),
        }
    }

    /// Start/continue execution of program
    ///
    /// If program returned value exit with Some(value)
    ///
    /// If program reached breakpoint exit with None
    pub fn run(&mut self)->Result<Option<Data>, Data::Error>{
        let res = loop{
            if let Some(return_value) = self.execute_instruction()?{
                break Some(return_value);
            }else if self.breakpoints.contains(&self.instruction_pointer){
                break None;
            }
        };
        Ok(res)
    }

    /// Execute single instruction on virtual machine
    ///
    /// If it result it stoping execution return Some(value)
    ///
    /// otherwise returns None
    fn execute_instruction(&mut self)->Result<Option<Data>, Data::Error>{
        let instruction = &self.code[self.instruction_pointer.0];
        self.instruction_pointer.0 += 1;
        match instruction{
            // ---- ----
            // Jumps
            Instruction::Jump(to) => {
                self.instruction_pointer = to.clone();
                Ok(None)
            },
            Instruction::JumpZ(to, condition) => {
                let condition = self.data_stack[condition].as_int()?;
                if condition == 0{
                    self.instruction_pointer = to.clone();
                }
                Ok(None)
            },
            Instruction::JumpNZ(to, condition) => {
                let condition = self.data_stack[condition].as_int()?;
                if condition != 0{
                    self.instruction_pointer = to.clone();
                }
                Ok(None)
            },
            // ---- ----
            // Int comparison
            Instruction::IntEq(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? == b.as_int()?;
                    Ok(Data::from_int(res as i32))
                })
            },
            Instruction::IntNotEq(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? != b.as_int()?;
                    Ok(Data::from_int(res as i32))
                })
            },
            Instruction::IntLess(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? < b.as_int()?;
                    Ok(Data::from_int(res as i32))
                })
            },
            Instruction::IntLessOrEq(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? <= b.as_int()?;
                    Ok(Data::from_int(res as i32))
                })
            },
            // ---- ----
            // Float comparison
            Instruction::FloatEq(bin) => {
                // Warning: comparing (Eq) floats without treashold
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_float()? == b.as_float()?;
                    Ok(Data::from_int(res as i32))
                })
            },
            Instruction::FloatNotEq(bin) => {
                // Warning: comparing (!Eq) floats without treashold
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_float()? != b.as_float()?;
                    Ok(Data::from_int(res as i32))
                })
            },
            Instruction::FloatLess(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_float()? < b.as_float()?;
                    Ok(Data::from_int(res as i32))
                })
            },
            Instruction::FloatLessOrEq(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_float()? <= b.as_float()?;
                    Ok(Data::from_int(res as i32))
                })
            },
            // ---- ----
            // Int arithmetics
            Instruction::IntSum(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? + b.as_int()?;
                    Ok(Data::from_int(res))
                })
            },
            Instruction::IntDiff(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? - b.as_int()?;
                    Ok(Data::from_int(res))
                })
            },
            Instruction::IntMul(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? * b.as_int()?;
                    Ok(Data::from_int(res))
                })
            },
            Instruction::IntDiv(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? / b.as_int()?;
                    Ok(Data::from_int(res))
                })
            },
            Instruction::IntMod(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_int()? % b.as_int()?;
                    Ok(Data::from_int(res))
                })
            },
            // ---- ----
            // Float arithmetics
            Instruction::FloatSum(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_float()? + b.as_float()?;
                    Ok(Data::from_float(res))
                })
            },
            Instruction::FloatDiff(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_float()? - b.as_float()?;
                    Ok(Data::from_float(res))
                })
            },
            Instruction::FloatMul(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_float()? * b.as_float()?;
                    Ok(Data::from_float(res))
                })
            },
            Instruction::FloatDiv(bin) => {
                Self::binary_operator(&mut self.data_stack, &bin, |a, b|{
                    let res = a.as_float()? / b.as_float()?;
                    Ok(Data::from_float(res))
                })
            },
            // ---- ----
            // Pointers
            Instruction::PointerGet(get) => {
                let pointer = self.data_stack[&get.pointer].as_int()?;
                let pointer_value = self.data_stack.global_pointer(pointer)?.clone();
                self.data_stack[&get.copy_to] = pointer_value;
                Ok(None)
            },
            Instruction::PointerSet(set) => {
                let value = self.data_stack[&set.copy_from].clone();
                let pointer = self.data_stack[&set.pointer].as_int()?;
                let cell = self.data_stack.global_pointer_mut(pointer)?;
                *cell = value;
                Ok(None)
            },
            // ---- ----
            // Function Call
            Instruction::StackPointerAdd(offset) => {
                self.data_stack.stack_pointer_add(*offset);
                Ok(None)
            },
            Instruction::Call => {
                let function_id = self.data_stack[&LocalDataPointer(0)].as_int()?;
                let function_addr = self.functions[function_id as usize];
                self.call_stack.push(self.instruction_pointer);
                // Due to the fact that instruction pointer was incremented in the beginning
                // of function saved value - pointer to instruction to be executed after 
                // called function returned
                self.instruction_pointer = function_addr;
                Ok(None)
            },
            Instruction::Return => {
                match self.call_stack.pop(){
                    // Get pointer to instruction to be executed in caller
                    // and jump there
                    Some(instruction_pointer) => {
                        self.instruction_pointer = instruction_pointer;
                        Ok(None)
                    },
                    // If no pointer found (already in main function)
                    // then terminate vm execution by returning value
                    None => {
                        let value = self.data_stack[&LocalDataPointer(0)].clone();
                        Ok(Some(value))
                    }
                }
            },
            // ---- ----
            // Constants
            Instruction::IntConst(put_into, value) => {
                self.data_stack[put_into] = Data::from_int(*value);
                Ok(None)
            },
            Instruction::FloatConst(put_into, value) => {
                self.data_stack[put_into] = Data::from_float(*value);
                Ok(None)
            },
            // ---- ----
            // Type convertions
            Instruction::IntToFloat(conv) => {
                let value = self.data_stack[&conv.source].as_int()?;
                let value = value as f32;
                self.data_stack[&conv.destination] = Data::from_float(value);
                Ok(None)
            },
            Instruction::FloatToInt(conv) => {
                let value = self.data_stack[&conv.source].as_float()?;
                let value = value as i32;
                self.data_stack[&conv.destination] = Data::from_int(value);
                Ok(None)
            },
            // ---- ----
            // Other
            Instruction::Inc(pointer) => {
                let value = self.data_stack[pointer].as_int()?;
                self.data_stack[pointer] = Data::from_int(value + 1);
                Ok(None)
            },
            Instruction::Dec(pointer) => {
                let value = self.data_stack[pointer].as_int()?;
                self.data_stack[pointer] = Data::from_int(value - 1);
                Ok(None)
            },
            Instruction::Copy(copy) => {
                let value = self.data_stack[&copy.source].clone();
                self.data_stack[&copy.destination] = value;
                Ok(None)
            }

            Instruction::Nop => {
                Ok(None)
            },
        }
    }

    fn binary_operator<F>(data_stack: &mut DataStack<Data>, bin: &Binary, operator: F)->Result<Option<Data>, Data::Error>
        where F: Fn(&Data, &Data)->Result<Data, Data::Error>
    {
        let res = {
            let left = &data_stack[&bin.left];
            let right = &data_stack[&bin.right];

            operator(left, right)
        };
        data_stack[&bin.put_into] = res?;
        Ok(None)
    }
}


#[cfg(test)]
mod tests {
}
