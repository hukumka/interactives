    function PersistentStack(parent, value){
        this.parent = parent
        this.value = value
        this.push = function(value){
            return new PersistentStack(this, value)
        }
        this.pop = function(){
            if(this.parent !== undefined){
                return this.parent
            }
        }

        this.top = function(){
            return this.value
        }

        this.values = function(){
            var res = [];
            var head = this;
            while(head.parent !== undefined){
                res.push(head.value)
                head = head.parent
            }
            return res
        }
    }


    var vm = new VM(compiled.commands, compiled.function_enters)

    function VariableManager(transactions){
        this.transactions = transactions;

        this.variables_data = []
        var stack = new PersistentStack()
        var transaction_id = 0
        for(var i=0; i<vm.code.length; ++i){
            while(compiled.variable_transactions[transaction_id].pos == i){
                var t = compiled.variable_transactions[transaction_id]
                if(t.add){
                    stack = stack.push(t)
                }else{
                    stack = stack.pop()
                }
                transaction_id += 1;
            }
            this.variables_data.push(stack)
        }

        this.visible_variables = function(ip){
            return variables_manager.variables_data[ip].values().reverse()
        }

        this.is_visible = function(ip, transaction){
            var pair = this.transactions[transaction].pair
            return this.transactions[transaction].pos <= ip && ip < this.transactions[pair].pos
        }
    }

    var variables_manager = new VariableManager(compiled.variable_transactions)


	function on_hover(event){
	    var elem = event.target;
	    if(elem.getAttribute('class') == "variable-name" && hover_image !== undefined){
	        var variable_id = Number(elem.getAttribute('data_var_offset'))
            var transaction_id = Number(elem.getAttribute('data_var_transaction'))

            if(variables_manager.is_visible(vm.ip, transaction_id)){
	            hover_image.innerHTML = elem.innerHTML + " = " + vm.get_local_variable(variable_id)
            }else{
	            hover_image.innerHTML = elem.innerHTML + " is out of bound."
            }
	        hover_image.style.left = (event.clientX + 10) + "px";
	        hover_image.style.top = (event.clientY + 10) + "px";
	        hover_image.style.display = "inline-block"
	    }else{
	        hover_image.innerHTML = ""
	        hover_image.style.display = "none"
	    }
	}

	function on_click(event){
	    var elem = event.target;
	    if(elem.classList.contains("line-number") && elem.getAttribute('data_address') !== null){
	        toggle_breakpoint(elem)
	    }
	}

	function toggle_breakpoint(elem){
	    var i = Number(elem.getAttribute('data_address'))
	    if(vm.breakpoints.includes(i)){
	        // delete breakpoint
	        elem.classList.remove('breakpoint')
            vm.breakpoints.splice(vm.breakpoints.indexOf(i), 1);
        }else{
	        elem.classList.add('breakpoint')
            vm.breakpoints.push(i);
        }
	}


    function display_variables(){
        var vars = variables_manager.visible_variables(vm.ip)
        var text = "<ul>" + vars.map(x => "<li>"+x.name + " = " + vm.get_local_variable(x.id) +"</li>").join("") + "</ul>"
        variables.innerHTML = text
    }

	var hover_image=undefined;
	var variables=undefined;
	window.onload = function(){
	    hover_image=document.getElementById('hover_image');
	    variables=document.getElementById('variables');

        var lines_e = document.getElementsByClassName('line-number');
	    for(var i=0; i<lines_e.length; ++i){
	        var l = lines_e[i];
            var addr = l.getAttribute('data_address');
            if(addr != null){
                lines.push([addr, l])
            }
        }
	    document.getElementById('run').addEventListener('click', run);
	    document.getElementById('step').addEventListener('click', step);
	    document.getElementById('step-in').addEventListener('click', step_in);
	}

    var lines = [];
    var current_line = null;


    function get_current_line(addr){
        for(var i=0; i<lines.length; ++i){
            var line_addr = lines[i][0];
            if(line_addr>addr){
                return lines[i-1][1];
            }
        }
        return lines[lines.length-1][1]
    }

    function run(){
        vm.run();
        set_current();
    }

    function step(){
        vm.step(compiled.statements)
        set_current();
    }

    function step_in(){
        var tmp = vm.breakpoints;
        vm.breakpoints = compiled.statements
        vm.run();
        vm.breakpoints = tmp;
        set_current();
    }

    function get_next_statement_addr(){
        var addr=-1;
        for(var i=0; i<compiled.statements.length; ++i){
            if(compiled.statements[i] > vm.ip){
                addr = compiled.statements[i];
                break;
            }
        }
        var range = vm.current_function_range()
        if(range[0] <= addr < range[1]){
            return addr;
        }else{
            return null;
        }
    }

    function set_current(){
        if(current_line != null){
            current_line.classList.remove('current')
        }
        current_line = get_current_line(vm.ip)
        current_line.classList.add('current')

        display_variables()
    }
	addEventListener('mousemove', on_hover);
	addEventListener('click', on_click);
