function VM(code, functions){
    this.sp = 0;
    this.ip = 0;
    this.code = code;
    this.functions = functions;
    this.breakpoints = [];
    this.break_on_return = false;
    this.data = [];
    this.return_stack = [];

    var self = this;

    this.run = function(){
        while(true){
            var command = this.code[this.ip];
            var command_id = command[0];
            var res = this.operations[command_id](command);
            if(res !== undefined){
                this.reset();
                return res;
            }
            if(this.breakpoints.includes(this.ip)){
                break;
            }
        }
    }

    this.step = function(statements){
        var range = this.current_function_range()
        var statements = statements.filter(x => range[0] <= x && x < range[1])
        var depth = this.return_stack.length;
        while(true){
            var command = this.code[this.ip];
            var command_id = command[0];
            var res = this.operations[command_id](command);
            if(res !== undefined){
                this.reset();
                return res;
            }
            if(this.return_stack.length == depth && statements.includes(this.ip)){
                break;
            }
            if(this.return_stack.length < depth){
                break;
            }
            if(this.breakpoints.includes(this.ip)){
                break;
            }
        }
    };

    this.current_function_range = function(){
        for(var i=0; i<this.functions.length-1; ++i){
            if(this.functions[i][1] <= this.ip && this.functions[i+1][1] > this.ip){
                return [this.functions[i][1], this.functions[i+1][1]]
            }
        }
        return [this.functions[this.functions.length-1][1], this.code.length]
    }

    this.get_local_variable = function(id){
        return this.data[this.sp + id]
    }

    this.reset = function(){
        this.ip = 0;
        this.data = [];
    }

    this.operations = [];
    this.operation_names = [];
    // jump
    this.operation_names[0] = "JUMP";
    this.operations[0] = function(args){
        // args[0] is 0
        self.ip = args[1];
    }
    // jump if zero
    this.operation_names[1] = "JUMP_Z";
    this.operations[1] = function(args){
        var jump_to = args[1];
        var cond_offset = args[2];
        if(self.data[self.sp +  cond_offset] == 0){
            self.ip = jump_to;
        }else{
            self.ip++;
        }
    }
    // jump if not zero
    this.operation_names[2] = "JUMP_NZ";
    this.operations[2] = this.op = function(args){
        var jump_to = args[1];
        var cond_offset = args[2];
        if(self.data[self.sp +  cond_offset] != 0){
            self.ip = jump_to;
        }else{
            self.ip++;
        }
    }
    // check if values equal
    this.operation_names[3] = "EQ";
    this.operations[3] = function(args){
        self.data[self.sp + args[1]] = (self.data[self.sp + args[2]] == self.data[self.sp + args[3]]) + 0;
        self.ip++;
    }
    // check if values not equal
    this.operation_names[4] = "NOT_EQ";
    this.operations[4] = function(args){
        self.data[self.sp + args[1]] = (self.data[self.sp + args[2]] != self.data[self.sp + args[3]]) + 0;
        self.ip++;
    }
    // check if left less then right
    this.operation_names[5] = "LESS";
    this.operations[5] = function(args){
        self.data[self.sp + args[1]] = (self.data[self.sp + args[2]] < self.data[self.sp + args[3]]) + 0;
        self.ip++;
    }
    // check if left less or equal to right
    this.operation_names[6] = "LESS_OR_EQ";
    this.operations[6] = function(args){
        self.data[self.sp + args[1]] = (self.data[self.sp + args[2]] <= self.data[self.sp + args[3]]) + 0;
        self.ip++;
    }
    // sum
    this.operation_names[7] = "SUM";
    this.operations[7] = function(args){
        self.data[args[1]] = self.data[args[2]] + self.data[args[3]];
        self.ip++;
    }
    // diff
    this.operation_names[8] = "DIFF";
    this.operations[8] = function(args){
        self.data[self.sp + args[1]] = self.data[self.sp + args[2]] - self.data[self.sp + args[3]];
        self.ip++;
    }
    // mul
    this.operation_names[9] = "MUL";
    this.operations[9] = function(args){
        self.data[self.sp + args[1]] = self.data[self.sp + args[2]] * self.data[self.sp + args[3]];
        self.ip++;
    }
    // div integer
    this.operation_names[10] = "DIV";
    this.operations[10] = function(args){
        self.data[self.sp + args[1]] = Math.floor(self.data[self.sp + args[2]] / self.data[self.sp + args[3]]);
        self.ip++;
    }
    // mod
    this.operation_names[11] = "MOD";
    this.operations[11] = function(args){
        self.data[self.sp + args[1]] = self.data[self.sp + args[2]] % self.data[self.sp + args[3]];
        self.ip++;
    }
    // inc
    this.operation_names[12] = "INC";
    this.operations[12] = function(args){
        var pointer = self.data[self.sp + args[1]];
        self.data[pointer]++;
        self.ip++;
    }
    // dec
    this.operation_names[13] = "DEC";
    this.operations[13] = function(args){
        var pointer = self.data[self.sp + args[1]];
        self.data[pointer]--;
        self.ip++;
    }
    // dereference
    this.operation_names[14] = "PTR_GET";
    this.operations[14] = function(args){
        var pointer = self.data[self.sp + args[2]];
        self.data[self.sp + args[1]] = self.data[pointer];
        self.ip++;
    }
    // pointer set
    this.operation_names[15] = "PTR_SET";
    this.operations[15] = function(args){
        var pointer = self.data[self.sp + args[1]];
        self.data[pointer] = self.data[self.sp + args[2]];
        self.ip++;
    }
    // Nop
    this.operation_names[16] = "NOP";
    this.operations[16] = function(args){
        self.ip++;
    }
    // Clone
    this.operation_names[17] = "MOV";
    this.operations[17] = function(args){
        self.data[self.sp + args[1]] = self.data[self.sp + args[2]];
        self.ip++;
    }
    // ConstInt
    this.operation_names[18] = "CONST_INT";
    this.operations[18] = function(args){
        self.data[self.sp + args[1]] = args[2];
        self.ip++;
    }
    // PointerToLocal
    this.operation_names[19] = "PTR_LOCAL";
    this.operations[19] = function(args){
        self.data[self.sp + args[1]] = self.sp + args[2];
        self.ip++;
    }
    // return
    this.operation_names[20] = "RET";
    this.operations[20] = function(args){
        if(self.return_stack.length > 0){
            self.ip = self.return_stack.pop() + 1;
        }else{
            return self.data[self.sp];
        }
    }
    // call
    this.operation_names[21] = "CALL";
    this.operations[21] = function(args){
        self.return_stack.push(self.ip);
        self.ip = self.functions[args[1]][1];
    }
    // add to stack pointer
    this.operation_names[22] = "SP_ADD";
    this.operations[22] = function(args){
        self.sp += args[1];
        self.ip++;
    }
    // sub from stack pointer
    this.operation_names[23] = "SP_SUB";
    this.operations[23] = function(args){
        self.sp -= args[1];
        self.ip++;
    }
}
