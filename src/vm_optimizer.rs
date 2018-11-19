use std::mem::discriminant;

use compiler::{Code, Operation};

pub struct Optimizer {
    /// all positions in code there jump can occure
    jumps: Vec<usize>,
}

impl Optimizer {
    pub fn new(code: &[Operation]) -> Self {
        let mut res = Self { jumps: vec![] };
        res.register_jumps(code);
        res
    }

    fn register_jumps(&mut self, code: &[Operation]) {
        for o in code {
            match o.code {
                Code::Jump => {
                    self.jumps.push(o.args[0]);
                }
                Code::JumpZ => {
                    self.jumps.push(o.args[0]);
                }
                Code::JumpNZ => {
                    self.jumps.push(o.args[0]);
                }
                _ => {}
            }
        }
    }

    fn has_jump_in_range(&mut self, from: usize, to: usize) -> bool {
        // TODO: improve perfomance (probably using segment tree)
        for &j in &self.jumps {
            if from < j && j <= to {
                return true;
            }
        }
        false
    }

    pub fn optimize(&mut self, code: &mut [Operation]) {
        self.optimize_local_variable_get(code);
    }

    fn optimize_local_variable_get(&mut self, code: &mut [Operation]) {
        for i in 0..(code.len() - 1) {
            if Self::is_local_get([&code[i], &code[i + 1]]) && !self.has_jump_in_range(i, i + 1) {
                let var_id = code[i].args[1];
                let cell_id = code[i + 1].args[0];
                code[i] = Operation {
                    code: Code::Clone,
                    args: vec![cell_id, var_id],
                    value: None,
                };
                code[i + 1] = Operation {
                    code: Code::Nop,
                    args: vec![],
                    value: None,
                }
            }
        }
    }

    fn is_local_get(code: [&Operation; 2]) -> bool {
        discriminant(&code[0].code) == discriminant(&Code::PointerToLocal)
            && discriminant(&code[1].code) == discriminant(&Code::Dereference)
            && code[0].args[0] == code[1].args[1] // guaranties that get related to local variable pointer
            && code[1].args[0] == code[1].args[1] // guaranties that dereference use same cell as pointer
                                                  // (so removal of pointer cannot possibly break it usage somewhere else)
    }
}
