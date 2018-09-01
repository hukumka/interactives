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
#[derive(Debug)]
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
#[derive(Debug)]
pub struct InstructionPointerGet{
    /// pointer to local variable ( or temporary value) there address value stored
    ///
    /// source
    source: LocalDataPointer,
    /// destination of copy
    destination: LocalDataPointer
}

/// Instruction::PointerSet representation
///
/// Copy value from cell with address stored in local variable pointer
/// into local variable (or temporary value)
#[derive(Debug)]
pub struct InstructionPointerSet{
    /// pointer to local variable ( or temporary value) there address value stored
    /// 
    /// destination
    destination: LocalDataPointer,
    /// source of copy
    source: LocalDataPointer
}


/// Instruction::Copy representation
///
/// Copy value from one local variable to other
#[derive(Debug)]
pub struct InstructionCopy{
    source: LocalDataPointer,
    destination: LocalDataPointer,
}


/// General binary operator instruction representation
#[derive(Debug)]
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


#[derive(Debug)]
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

    instruction_limit: usize,
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

            instruction_limit: 2000,
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
            }else if self.instruction_limit == 0{
                self.instruction_limit = 2000;
                break None;
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
        self.instruction_limit -= 1;
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
                let pointer = self.data_stack[&get.source].as_int()?;
                let pointer_value = self.data_stack.global_pointer(pointer)?.clone();
                self.data_stack[&get.destination] = pointer_value;
                Ok(None)
            },
            Instruction::PointerSet(set) => {
                let value = self.data_stack[&set.source].clone();
                let pointer = self.data_stack[&set.destination].as_int()?;
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
    use types::ArbitraryDataEnum;

    type Data=ArbitraryDataEnum;

    use super::*;

    /// Struct used to track data side effects of instructions
    struct SideEffects<Data: ArbitraryData>{
        data_changes: Vec<usize>,
        sp_changes: bool,
        _allocates: bool,
        returned: Result<Option<Data>, Data::Error>,
    }

    impl SideEffects<Data>{
        /// execute ```VirtualMachine::execute_instruction``` and
        /// construct ```SideEffects``` data
        fn evaluate_and_check(vm: &mut VirtualMachine<Data>)->Self{
            let stored_data: Vec<Data> = vm.data_stack.raw_data()
                .iter()
                .map(|x| x.clone())
                .collect();
            let old_sp = vm.data_stack.raw_stack_offset().clone();
            
            let returned = vm.execute_instruction();
            let sp_changes = old_sp != *vm.data_stack.raw_stack_offset();

            let new_data = vm.data_stack.raw_data();
            let _allocates = new_data.len() != stored_data.len();
            let mut data_changes = vec![];
            for (i, (old, new)) in stored_data.iter().zip(new_data).enumerate(){
                if old != new{
                    data_changes.push(i)
                }
            }

            Self{
                data_changes,
                _allocates,
                sp_changes,
                returned
            }
        }

        /// check if only data alteration present are allowed
        fn check_allowed_data_changes(&self, allow: &[usize])->Result<(), ()>{
            for x in &self.data_changes{
                if !allow.contains(x){
                    return Err(());
                }
            }
            Ok(())
        }
    }
    

    // ---- ----
    // Single instruction tests

    // ---- ----
    // Jumps
    #[test]
    fn test_jump(){
        let code = vec![
            Instruction::Jump(CodePointer(3)),
            Instruction::Nop,
            Instruction::Nop,
            Instruction::Jump(CodePointer(1)),
        ];

        let mut vm = VirtualMachine::<Data>::new(code, vec![]);
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert_eq!(result.data_changes, vec![]);
        assert_eq!(result.sp_changes, false);
        assert_eq!(result.returned, Ok(None));
        assert_eq!(vm.instruction_pointer, CodePointer(3));

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert_eq!(result.data_changes, vec![]);
        assert_eq!(result.sp_changes, false);
        assert_eq!(result.returned, Ok(None));
        assert_eq!(vm.instruction_pointer, CodePointer(1));
    }

    #[test]
    fn test_jump_zero(){
        let code = vec![
            Instruction::JumpZ(CodePointer(3), LocalDataPointer(0)), // negative
            Instruction::JumpZ(CodePointer(3), LocalDataPointer(0)), // negative
            Instruction::Nop,
            Instruction::Nop,
        ];

        let mut vm = VirtualMachine::<Data>::new(code, vec![]);
        vm.data_stack[&LocalDataPointer(0)] = Data::from_int(1); // not zero, no jump

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert_eq!(result.data_changes, vec![]); // data unchanged
        assert_eq!(result.sp_changes, false); // stack pointer unchanged
        assert_eq!(result.returned, Ok(None)); // no errors/no evaluation end
        assert_eq!(vm.instruction_pointer, CodePointer(1));

        vm.data_stack[&LocalDataPointer(0)] = Data::from_int(0); // zero, jumps
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert_eq!(result.data_changes, vec![]); // data unchanged
        assert_eq!(result.sp_changes, false); // stack pointer unchanged
        assert_eq!(result.returned, Ok(None)); // no errors/no evaluation end
        assert_eq!(vm.instruction_pointer, CodePointer(3));
    }

    #[test]
    fn test_jump_not_zero(){
        let code = vec![
            Instruction::JumpNZ(CodePointer(3), LocalDataPointer(0)), // negative
            Instruction::Nop,
            Instruction::Nop,
            Instruction::JumpNZ(CodePointer(1), LocalDataPointer(0)), // negative
            Instruction::Nop,
        ];

        let mut vm = VirtualMachine::<Data>::new(code, vec![]);
        vm.data_stack[&LocalDataPointer(0)] = Data::from_int(1); // not zero, jump
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert_eq!(result.data_changes, vec![]); // data unchanged
        assert_eq!(result.sp_changes, false); // stack pointer unchanged
        assert_eq!(result.returned, Ok(None)); // no errors/no evaluation end
        assert_eq!(vm.instruction_pointer, CodePointer(3));

        vm.data_stack[&LocalDataPointer(0)] = Data::from_int(0); // zero, no jumps
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert_eq!(result.data_changes, vec![]); // data unchanged
        assert_eq!(result.sp_changes, false); // stack pointer unchanged
        assert_eq!(result.returned, Ok(None)); // no errors/no evaluation end
        assert_eq!(vm.instruction_pointer, CodePointer(4));
    }


    // ---- ----
    // Binary operator tests
    /// generate code with sequence of $len uses of binary operator
    /// used in instruction $instuction with:
    ///
    /// + LocalDataPointer(0) as destination
    /// + LocalDataPointer(1) as left operand
    /// + LocalDataPointer(2) as right operand
    macro_rules! generate_code_bin_op{
        ($instruction: ident, $len: expr) => {
            {
                let mut code = vec![];
                for _ in 0..$len{
                    code.push(Instruction::$instruction(Binary{
                        left: LocalDataPointer(1),
                        right: LocalDataPointer(2),
                        put_into: LocalDataPointer(0),
                    }));
                }
                VirtualMachine::<Data>::new(code, vec![])
            }
        }
    }
    

    /// generate test for binary operator
    /// 
    /// Arguments:
    ///
    /// + $test_name - name for newly generated test
    /// + $type - int | float
    /// + $op - name of operator in enum (eg IntEq, IntSum etc)
    /// + [$($res ;=; $left ;*; $right),*] - coma separated list of triplets:
    /// + + $res - expected result of evaluation
    /// + + $left - left argument
    /// + + $right - left argument
    ///
    /// will assert if ```VirtualMachine::execute_instruction``` executed without errors each step
    /// assert if ```instruction_pointer``` incremented after each instruction
    /// assert if expected ```$res``` is equal to actual result
    ///
    /// Also assert for error with inappropriate type usage
    macro_rules! generate_type_bin_test{
        ($test_name: ident, $type: ident, $op: ident, [$($res: expr ;=; $left: expr ;*; $right: expr,)*]) => {
            #[test]
            fn $test_name(){
                let expected = vec![$($res),*];
                let mut vm = generate_code_bin_op!($op, expected.len());

                let mut _ip = 0;
                $(
                    vm.data_stack[&LocalDataPointer(1)] = $left;
                    vm.data_stack[&LocalDataPointer(2)] = $right;
                    let result = SideEffects::evaluate_and_check(&mut vm);
                    assert!(result.check_allowed_data_changes(&[vm.data_stack.raw_stack_offset().0]).is_ok(), "Iteration {}. Data side effects found", _ip);
                    assert_eq!(result.sp_changes, false, "Iteration {}. Stack pointer altered.", _ip);
                    assert_eq!(result.returned, Ok(None), "Iteration {}. Expected normal execution flow", _ip);
                    assert_eq!(vm.instruction_pointer, CodePointer(_ip+1), "Iteration {}. Unexpected instruction pointer value", _ip);
                    assert_eq!(vm.data_stack[&LocalDataPointer(0)], $res, "Iteration {}. Unexpected operator evaluation result", _ip);
                    _ip += 1;
                )*

                generate_type_bin_test!{#error_with_other_types, vm, $type, $op}
            }
        };

        (#error_with_other_types, $vm:expr, int, $op: ident) => {
            // test for use with float
            $vm.instruction_pointer = CodePointer(0);
            $vm.data_stack[&LocalDataPointer(1)] = Data::from_float(2.0);
            $vm.data_stack[&LocalDataPointer(2)] = Data::from_float(4.0);
            assert_eq!($vm.execute_instruction(), Err(()), "float : float; error expected");
            // test for use with float and int
            $vm.instruction_pointer = CodePointer(0);
            $vm.data_stack[&LocalDataPointer(1)] = Data::from_int(2);
            $vm.data_stack[&LocalDataPointer(2)] = Data::from_float(4.0);
            assert_eq!($vm.execute_instruction(), Err(()), "int : float; error expected");
            // test for use with void
            $vm.instruction_pointer = CodePointer(0);
            $vm.data_stack[&LocalDataPointer(1)] = Data::default();
            $vm.data_stack[&LocalDataPointer(2)] = Data::from_int(4);
            assert_eq!($vm.execute_instruction(), Err(()), "void; error expected");
        };

        (#error_with_other_types, $vm:expr, float, $op: ident) => {
            // test for use with float
            $vm.instruction_pointer = CodePointer(0);
            $vm.data_stack[&LocalDataPointer(1)] = Data::from_int(2);
            $vm.data_stack[&LocalDataPointer(2)] = Data::from_int(4);
            assert_eq!($vm.execute_instruction(), Err(()), "int : int; error expected");
            // test for use with float and int
            $vm.instruction_pointer = CodePointer(0);
            $vm.data_stack[&LocalDataPointer(1)] = Data::from_int(2);
            $vm.data_stack[&LocalDataPointer(2)] = Data::from_float(4.0);
            assert_eq!($vm.execute_instruction(), Err(()), "int : float; error expected");
            // test for use with void
            $vm.instruction_pointer = CodePointer(0);
            $vm.data_stack[&LocalDataPointer(1)] = Data::from_float(2.0);
            $vm.data_stack[&LocalDataPointer(2)] = Data::default();
            assert_eq!($vm.execute_instruction(), Err(()), "void; error expected");
        };
    }


    // ---- ----
    // Int comparison
    generate_type_bin_test!{
        test_int_eq, 
        int, IntEq,
        [
            Data::from_int(1) ;=; Data::from_int(2) ;*; Data::from_int(2),
            Data::from_int(0) ;=; Data::from_int(2) ;*; Data::from_int(4),
        ]
    }

    generate_type_bin_test!{
        test_int_not_eq, 
        int, IntNotEq,
        [
            Data::from_int(0) ;=; Data::from_int(2) ;*; Data::from_int(2),
            Data::from_int(1) ;=; Data::from_int(2) ;*; Data::from_int(4),
        ]
    }

    generate_type_bin_test!{
        test_int_less, 
        int, IntLess,
        [
            Data::from_int(0) ;=; Data::from_int(2) ;*; Data::from_int(2),
            Data::from_int(0) ;=; Data::from_int(3) ;*; Data::from_int(2),
            Data::from_int(1) ;=; Data::from_int(2) ;*; Data::from_int(4),
        ]
    }

    generate_type_bin_test!{
        test_int_less_or_eq, 
        int, IntLessOrEq,
        [
            Data::from_int(1) ;=; Data::from_int(2) ;*; Data::from_int(2),
            Data::from_int(0) ;=; Data::from_int(3) ;*; Data::from_int(2),
            Data::from_int(1) ;=; Data::from_int(2) ;*; Data::from_int(4),
        ]
    }

    // ---- ----
    // Float comparison
    generate_type_bin_test!{
        test_float_eq, 
        float, FloatEq,
        [
            Data::from_int(1) ;=; Data::from_float(2.0) ;*; Data::from_float(2.0),
            Data::from_int(0) ;=; Data::from_float(2.0) ;*; Data::from_float(4.0),
        ]
    }

    generate_type_bin_test!{
        test_float_not_eq, 
        float, FloatNotEq,
        [
            Data::from_int(0) ;=; Data::from_float(2.0) ;*; Data::from_float(2.0),
            Data::from_int(1) ;=; Data::from_float(2.0) ;*; Data::from_float(4.0),
        ]
    }

    generate_type_bin_test!{
        test_float_less, 
        float, FloatLess,
        [
            Data::from_int(0) ;=; Data::from_float(2.0) ;*; Data::from_float(2.0),
            Data::from_int(1) ;=; Data::from_float(2.0) ;*; Data::from_float(4.0),
            Data::from_int(0) ;=; Data::from_float(5.0) ;*; Data::from_float(-4.3),
        ]
    }

    generate_type_bin_test!{
        test_float_less_or_eq, 
        float, FloatLessOrEq,
        [
            Data::from_int(1) ;=; Data::from_float(2.0) ;*; Data::from_float(2.0),
            Data::from_int(1) ;=; Data::from_float(2.0) ;*; Data::from_float(4.0),
            Data::from_int(0) ;=; Data::from_float(5.0) ;*; Data::from_float(-4.3),
        ]
    }


    // ---- ----
    // Int arithmetics
    generate_type_bin_test!{
        test_int_sum, 
        int, IntSum,
        [
            Data::from_int(4) ;=; Data::from_int(1) ;*; Data::from_int(3),
            Data::from_int(-1) ;=; Data::from_int(3) ;*; Data::from_int(-4),
            Data::from_int(9) ;=; Data::from_int(5) ;*; Data::from_int(4),
        ]
    }

    generate_type_bin_test!{
        test_int_diff, 
        int, IntDiff,
        [
            Data::from_int(-2) ;=; Data::from_int(1) ;*; Data::from_int(3),
            Data::from_int(7) ;=; Data::from_int(3) ;*; Data::from_int(-4),
            Data::from_int(1) ;=; Data::from_int(5) ;*; Data::from_int(4),
        ]
    }

    generate_type_bin_test!{
        test_int_mul, 
        int, IntMul,
        [
            Data::from_int(3) ;=; Data::from_int(1) ;*; Data::from_int(3),
            Data::from_int(-12) ;=; Data::from_int(3) ;*; Data::from_int(-4),
            Data::from_int(20) ;=; Data::from_int(5) ;*; Data::from_int(4),
        ]
    }

    generate_type_bin_test!{
        test_int_div, 
        int, IntDiv,
        [
            Data::from_int(0) ;=; Data::from_int(1) ;*; Data::from_int(3),
            Data::from_int(0) ;=; Data::from_int(3) ;*; Data::from_int(-4),
            Data::from_int(-1) ;=; Data::from_int(5) ;*; Data::from_int(-4),
            Data::from_int(-1) ;=; Data::from_int(-5) ;*; Data::from_int(4),
            Data::from_int(1) ;=; Data::from_int(5) ;*; Data::from_int(4),
            Data::from_int(1) ;=; Data::from_int(-5) ;*; Data::from_int(-4),
        ]
    }

    generate_type_bin_test!{
        test_int_mod, 
        int, IntMod,
        [
            Data::from_int(1) ;=; Data::from_int(1) ;*; Data::from_int(3),
            Data::from_int(3) ;=; Data::from_int(3) ;*; Data::from_int(-4),
            Data::from_int(1) ;=; Data::from_int(5) ;*; Data::from_int(-4),
            Data::from_int(-1) ;=; Data::from_int(-5) ;*; Data::from_int(4),
            Data::from_int(1) ;=; Data::from_int(5) ;*; Data::from_int(4),
            Data::from_int(-1) ;=; Data::from_int(-5) ;*; Data::from_int(-4),
        ]
    }

    // ---- ----
    // Float arithmetics
    generate_type_bin_test!{
        test_float_sum, 
        float, FloatSum,
        [
            Data::from_float(3.13) ;=; Data::from_float(3.2) ;*; Data::from_float(-0.07),
        ]
    }

    generate_type_bin_test!{
        test_float_diff, 
        float, FloatDiff,
        [
            Data::from_float(3.27) ;=; Data::from_float(3.2) ;*; Data::from_float(-0.07),
        ]
    }

    generate_type_bin_test!{
        test_float_mul, 
        float, FloatMul,
        [
            Data::from_float(-0.224) ;=; Data::from_float(3.2) ;*; Data::from_float(-0.07),
        ]
    }

    generate_type_bin_test!{
        test_float_div, 
        float, FloatDiv,
        [
            Data::from_float(3.2/-0.07) ;=; Data::from_float(3.2) ;*; Data::from_float(-0.07),
        ]
    }

    // ---- ----
    // Pointers
    #[test]
    fn test_pointer_get(){
        let code = vec![
            Instruction::PointerGet(InstructionPointerGet{
                source: LocalDataPointer(1),
                destination: LocalDataPointer(0),
            }),
            Instruction::PointerGet(InstructionPointerGet{
                source: LocalDataPointer(1),
                destination: LocalDataPointer(0),
            }),
        ];

        let mut vm = VirtualMachine::new(code, vec![]);
        vm.data_stack[&LocalDataPointer(1)] = ArbitraryDataEnum::Int(2);
        vm.data_stack[&LocalDataPointer(2)] = ArbitraryDataEnum::Float(3.14);

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[0]).is_ok());
        assert_eq!(result.sp_changes, false); // stack pointer unchanged
        assert_eq!(result.returned, Ok(None)); // no errors/no evaluation end
        assert_eq!(vm.data_stack[&LocalDataPointer(0)], ArbitraryDataEnum::Float(3.14));

        vm.data_stack.stack_pointer_add(3);
        vm.data_stack[&LocalDataPointer(1)] = ArbitraryDataEnum::Int(1);
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[3]).is_ok());
        assert_eq!(result.sp_changes, false); // stack pointer unchanged
        assert_eq!(result.returned, Ok(None)); // no errors/no evaluation end
        assert_eq!(vm.data_stack[&LocalDataPointer(0)], ArbitraryDataEnum::Int(2));
    }

    #[test]
    fn test_pointer_set(){
        let code = vec![
            Instruction::PointerSet(InstructionPointerSet{
                destination: LocalDataPointer(1),
                source: LocalDataPointer(0),
            }),
            Instruction::PointerSet(InstructionPointerSet{
                destination: LocalDataPointer(1),
                source: LocalDataPointer(0),
            }),
        ];

        let mut vm = VirtualMachine::new(code, vec![]);
        vm.data_stack[&LocalDataPointer(1)] = ArbitraryDataEnum::Int(2);
        vm.data_stack[&LocalDataPointer(0)] = ArbitraryDataEnum::Float(3.14);

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[2]).is_ok());
        assert_eq!(result.sp_changes, false); // stack pointer unchanged
        assert_eq!(result.returned, Ok(None)); // no errors/no evaluation end
        assert_eq!(vm.data_stack[&LocalDataPointer(2)], ArbitraryDataEnum::Float(3.14));

        vm.data_stack.stack_pointer_add(3);
        vm.data_stack[&LocalDataPointer(1)] = ArbitraryDataEnum::Int(2);
        vm.data_stack[&LocalDataPointer(0)] = ArbitraryDataEnum::Float(3.14);
        vm.data_stack[&LocalDataPointer(2)] = ArbitraryDataEnum::Void;
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[5]).is_ok());
        assert_eq!(result.sp_changes, false); // stack pointer unchanged
        assert_eq!(result.returned, Ok(None)); // no errors/no evaluation end
        vm.data_stack.stack_pointer_add(-3);
        assert_eq!(vm.data_stack[&LocalDataPointer(2)], ArbitraryDataEnum::Float(3.14));
    }


    // ---- ----
    // Function Call
    #[test]
    fn test_stack_pointer_add(){
        let code = vec![
            Instruction::StackPointerAdd(4),
            Instruction::StackPointerAdd(-4),
        ];

        let mut vm = VirtualMachine::new(code, vec![]);
        vm.data_stack[&LocalDataPointer(5)] = ArbitraryDataEnum::Void;

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(vm.data_stack.raw_stack_offset(), &LocalDataPointer(4));
        assert_eq!(vm.instruction_pointer, CodePointer(1));

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(vm.data_stack.raw_stack_offset(), &LocalDataPointer(0));
        assert_eq!(vm.instruction_pointer, CodePointer(2));
    }

    #[test]
    fn call_and_return(){
        let code = vec![
            Instruction::Call,
            Instruction::Return,
            Instruction::Return,
            Instruction::Call,
            Instruction::Return
        ];

        let mut vm = VirtualMachine::new(code, vec![
            CodePointer(2),
            CodePointer(3),
        ]);

        // prepare call
        vm.data_stack[&LocalDataPointer(0)] = ArbitraryDataEnum::Int(1);
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(3));

        // second call
        vm.data_stack[&LocalDataPointer(0)] = ArbitraryDataEnum::Int(0);
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(2));

        // return
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(4));

        // second return
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(1));

        // third return - program exit
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[]).is_ok());
        // returns value at cell 0, which was set above
        assert_eq!(result.returned, Ok(Some(ArbitraryDataEnum::Int(0))));
        assert_eq!(result.sp_changes, false);
        // since execution stopped ip does not matter
    }
    
    // ---- ----
    // Constants
    #[test]
    fn test_int_const(){
        let code = vec![
            Instruction::IntConst(LocalDataPointer(0), 5),
        ];

        let mut vm = VirtualMachine::new(code, vec![]);
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[0]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(1));
        assert_eq!(vm.data_stack.raw_data()[0], ArbitraryDataEnum::Int(5));
    }

    #[test]
    fn test_float_const(){
        let code = vec![
            Instruction::FloatConst(LocalDataPointer(0), 3.11),
        ];

        let mut vm = VirtualMachine::new(code, vec![]);
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[0]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(1));
        assert_eq!(vm.data_stack.raw_data()[0], ArbitraryDataEnum::Float(3.11));
    }

    // ---- ----
    // Other
    #[test]
    fn test_inc(){
        let code = vec![
            Instruction::Inc(LocalDataPointer(1)),
            Instruction::Inc(LocalDataPointer(2)),
        ];

        let mut vm = VirtualMachine::new(code, vec![]);

        vm.data_stack[&LocalDataPointer(1)] = ArbitraryDataEnum::Int(1);
        vm.data_stack[&LocalDataPointer(2)] = ArbitraryDataEnum::Int(-1);

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[1]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(1));
        assert_eq!(vm.data_stack.raw_data()[1], ArbitraryDataEnum::Int(2));

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[2]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(2));
        assert_eq!(vm.data_stack.raw_data()[2], ArbitraryDataEnum::Int(0));
    }

    #[test]
    fn test_dec(){
        let code = vec![
            Instruction::Dec(LocalDataPointer(1)),
            Instruction::Dec(LocalDataPointer(2)),
        ];

        let mut vm = VirtualMachine::new(code, vec![]);

        vm.data_stack[&LocalDataPointer(1)] = ArbitraryDataEnum::Int(1);
        vm.data_stack[&LocalDataPointer(2)] = ArbitraryDataEnum::Int(-1);

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[1]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(1));
        assert_eq!(vm.data_stack.raw_data()[1], ArbitraryDataEnum::Int(0));

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[2]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(2));
        assert_eq!(vm.data_stack.raw_data()[2], ArbitraryDataEnum::Int(-2));
    }

    #[test]
    fn test_nop(){
        let code = vec![
            Instruction::Nop
        ];
        let mut vm = VirtualMachine::new(code, vec![]);

        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(1));
    }

    #[test]
    fn copy(){
        let code = vec![
            Instruction::Copy(InstructionCopy{
                source: LocalDataPointer(0),
                destination: LocalDataPointer(2),
            }),
        ];
        let mut vm = VirtualMachine::new(code, vec![]);
        vm.data_stack[&LocalDataPointer(0)] = ArbitraryDataEnum::Int(2);
        let result = SideEffects::evaluate_and_check(&mut vm);
        assert!(result.check_allowed_data_changes(&[2]).is_ok());
        assert_eq!(result.returned, Ok(None));
        assert_eq!(result.sp_changes, false);
        assert_eq!(vm.instruction_pointer, CodePointer(1));
        assert_eq!(vm.data_stack.raw_data()[2], ArbitraryDataEnum::Int(2));
    }
}

