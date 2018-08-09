    var vm = new VM(compiled.commands, compiled.function_enters)
    console.log(vm)

	function on_hover(event){
	    var elem = event.target;
	    if(elem.getAttribute('class') == "variable-name" && hover_image !== undefined){
	        var variable_id = Number(elem.getAttribute('data_var_offset'))

	        hover_image.innerHTML = elem.innerHTML + " = " + vm.get_local_variable(variable_id)
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

	var hover_image=undefined;
	window.onload = function(){
	    hover_image=document.getElementById('hover_image');

        var lines_e = document.getElementsByClassName('line-number');
	    for(var i=0; i<lines_e.length; ++i){
	        var l = lines_e[i];
            console.log(l)
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
            console.log("next statement", addr)
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
    }
	addEventListener('mousemove', on_hover);
	addEventListener('click', on_click);
