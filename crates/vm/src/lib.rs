//!
//! # Virtual Machine descrition & js bindings
//!
//!
//! 
#![feature(rust_2018_preview, uniform_paths)]

mod types;

use types::{
    ArbitraryData,
    ArbitraryDataEnum,   
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
    Int_Eq(Binary),

    /// calculate binary operator !=
    Int_NotEq(Binary),

    /// calculate binary operator <
    Int_Less(Binary),

    /// calculate binary operator <=
    Int_LessOrEq(Binary),

    // ---- ----
    // Comparison: Float
    /// calculate binary operator ==
    Float_Eq(Binary),

    /// calculate binary operator !=
    FLoat_NotEq(Binary),

    /// calculate binary operator <
    Float_Less(Binary),

    /// calculate binary operator <=
    FLoat_LessOrEq(Binary),

    // ---- ----
    // Arithmetic: Int
    /// Calculate binary operator +
    Int_Sum(Binary),

    /// Calculate binary operator -
    Int_Diff(Binary),

    /// Calculate binary operator *
    Int_Mul(Binary),

    /// Calculate binary operator /
    Int_Div(Binary),

    /// Calculate binary operator %
    Int_Mod(Binary),

    // ---- ----
    // Arithmetic: FLoat
    /// Calculate binary operator +
    Float_Sum(Binary),

    /// Calculate binary operator -
    Float_Diff(Binary),

    /// Calculate binary operator *
    Float_Mul(Binary),

    /// Calculate binary operator /
    Float_Div(Binary),

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
    Int_Const(LocalDataPointer, i32),

    // ---- ----
    /// Put value into local variable
    Float_Const(LocalDataPointer, i32),

    // ---- ----
    // Type convertions
    /// copy and convert int to float
    IntToFloat(InstructionCopy),

    /// copy and convert float to int
    FloatToInt(InstructionCopy),

    // ---- ----
    // Other
    /// Increment value at address
    /// stored in local variable
    /// (int and alike only)
    Inc(LocalDataPointer),

    /// Decrement value at address
    /// stored in local variable
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
    pub put_into: CodePointer,
    /// left operand
    pub left: CodePointer,
    /// right operand
    pub right: CodePointer
}

/// references position in code
pub struct CodePointer(usize);

/// references local variable or temporary value
pub struct LocalDataPointer(usize);


#[cfg(test)]
mod tests {
}
