use compiler::{Operation, Code};


struct Optimizer{
    /// all positions in code there jump can occure
    jumps: Vec<usize>;
};


impl Optimizer{
    fn new()->Self{
        Self{}
    }


    fn register_jumps(&mut self, code: &[Operation]){
        for o in code{
            match o.code{
                Code::Jump => {
                    self.jumps.push(o.args[0]);
                },
                Code::JumpZ => {
                    self.jumps.push(o.args[0]);
                },
                Code::JumpNZ => {
                    self.jumps.push(o.args[0]);
                },
                _ => {}
            }
        }
    }

    fn has_jump_in_range(&mut self, from: usize, to: usize)->bool{
        for j in jumps{
            if from < j && j <= to{
                return true;
            }
        }
        true
    }

    fn optimize(&mut self, code: &mut [Operation]){
        self.optimize_local_variable_access(code);
    }


    fn optimize_local_variable_access(&mut self, code: &mut [Operation]){
        //TODO
    }
}
