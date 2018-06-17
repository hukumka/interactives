function VM(code, functions){
    this.sp = 0;
    this.ip = 0;
    this.code = code;
    this.functions = functions;
    this.breakpoints = [];
    this.data = [];
    this.return_stack = [];

    this.run = function(){
        while(true){
            if(this.breakpoints.contains(this.ip)){
                break;
            }
            var command = this.code[this.ip];
            var res = this.operations[command[0]](command);
            if(res !== undefined){
                this.reset();
                return res;
            }
        }
    }

    this.reset = function(){
        this.ip = 0;
        this.data = [];
    }

    this.operations = [];
    // jump
    this.operations[0] = function(args){
        // args[0] is 0
        this.ip = args[1];
    }
    // jump if zero
    this.operations[1] = function(args){
        var jump_to = args[1];
        var cond_offset = args[2];
        if(this.data[sp +  cond_offset] == 0){
            this.ip = jump_to;
        }else{
            this.ip++;
        }
    }
    // jump if not zero
    this.operations[2] = function(args){
        var jump_to = args[1];
        var cond_offset = args[2];
        if(this.data[sp +  cond_offset] != 0){
            this.ip = jump_to;
        }else{
            this.ip++;
        }
    }
    // check if values equal
    this.operations[3] = function(args){
        this.data[args[1]] = this.data[args[2]] == this.data[args[3]];
        this.ip++;
    }
    // check if values not equal
    this.operations[4] = function(args){
        this.data[args[1]] = this.data[args[2]] != this.data[args[3]];
        this.ip++;
    }
    // check if left less then right
    this.operations[5] = function(args){
        this.data[args[1]] = this.data[args[2]] < this.data[args[3]];
        this.ip++;
    }
    // check if left less or equal to right
    this.operations[6] = function(args){
        this.data[args[1]] = this.data[args[2]] <= this.data[args[3]];
        this.ip++;
    }
    // sum
    this.operations[7] = function(args){
        this.data[args[1]] = this.data[args[2]] + this.data[args[3]];
        this.ip++;
    }
    // diff
    this.operations[8] = function(args){
        this.data[args[1]] = this.data[args[2]] - this.data[args[3]];
        this.ip++;
    }
    // mul
    this.operations[9] = function(args){
        this.data[args[1]] = this.data[args[2]] * this.data[args[3]];
        this.ip++;
    }
    // div integer
    this.operations[10] = function(args){
        this.data[args[1]] = Math.floor(this.data[args[2]] / this.data[args[3]]);
        this.ip++;
    }
    // mod
    this.operations[11] = function(args){
        this.data[args[1]] = this.data[args[2]] % this.data[args[3]];
        this.ip++;
    }
    // inc
    this.operations[12] = function(args){
        this.data[args[1]]++;
        this.ip++;
    }
    // dec
    this.operations[13] = function(args){
        this.data[args[1]]--;
        this.ip++;
    }
    // dereference
    this.operations[14] = function(args){
        var pointer = this.data[args[2]];
        this.data[args[1]] = this.data[pointer];
        this.ip++;
    }
    // pointer set
    this.operations[14] = function(args){
        var pointer = this.data[args[1]];
        this.data[pointer] = this.data[args[1]];
        this.ip++;
    }
    // Nop
    this.operations[15] = function(args){
        this.ip++;
    }
    // Clone
    this.operations[16] = function(args){
        this.data[args[1]] = this.data[args[2]];
        this.ip++;
    }
    // ConstInt
    this.operations[17] = function(args){
        this.data[args[1]] = args[2];
        this.ip++;
    }
    // PointerToLocal
    this.operations[18] = function(args){
        this.data[args[1]] = this.sp + args[2];
        this.ip++;
    }
    // return
    this.operations[19] = function(args){
        this.ip = this.return_stack.pop() + 1;
    }
    // call
    this.operations[20] = function(args){
        this.return_stack.push(this.ip);
        this.ip = this.functions[args[1]][1];
    }
    // add to stack pointer
    this.operations[21] = function(args){
        this.sp += args[1];
        this.ip++;
    }
    // sub from stack pointer
    this.operations[22] = function(args){
        this.sp -= args[1];
        this.ip++;
    }
}