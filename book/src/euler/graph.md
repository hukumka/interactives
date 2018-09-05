# Graph

This page describes how to enchant page we built in previous step by adding graph display.


## Modify code

First we need some interface to show page what we want do draw. To do so we'll add new function declaration:

```C
void display(float* array, int size);
```

and call of this function each iteration of method.

```C
    ...
    for(int i=0; i<COUNT; ++i){
        data[i] = f;
        f += step * P(f, x);
        x += step;

        display(data, i);
    }
    ...
}
```

## Add plotly script
```
<script src="https://cdn.plot.ly/plotly-latest.min.js"></script>
```

inside head.

## Add element where graph will be contained

by simply extending our ```<div class="interface">``` with new child

```
<div id="graph"></div>
```

## Extracting data

Now only thing we have to do is to add function link to ploty (or other library you might prefer) as in previous page... 

```js
functions['display'] = {{
    arg_count: 1,
    func: function(pointer, size){{
        console.log(pointer, size);
        //...
    }}
}}
```

Whawhawait, we receiving two integers? What would we do with them?

Unfortunatly, after compilation, virtual machine no longer posseses knowlege about variable type. Therefore it is suppling as with what it can,
and it is our duty to interpret it correcty.

But, we still can access our array using method vm.get_array.

```js
var y = vm.get_array(pointer, size);

var x = [];
var STEP = 0.01;
for(var i=0; i<y.length; ++i){{
    x.push(i*STEP);
}}

Plotly.plot(document.getElementById('graph'), [{{x: x, y: y}}]);
```

With this we can rebuild our page, open it, press run and get terrifying experiance.
What is this? Page freezes and after centuries passes and it start responding we get what? Billions graph annotations? What the heck?

## Fix it

Lets take a look what gone wrong (althrough you might have recognised problem just by glancing on code). Each time vm finishes other iteration of loop it calls display.
Each time display is called it orders plotly to create new plot on element with id "graph". And drawing graph is not cheap.

What we can do, is to delay actual drawing until vm pauses. On top of script block:

```js
var graph = null; // div element for graph
var graph_data = {{
    x: null,
    y: null,
}};
```

Then change code of display to simply putting given array into ```graph_data.y```.

```js
func: function(pointer, size){
    graph_data.y = vm.get_array(pointer, size);
}
```

Now, then our page loads we initialize graph variable with its element and draw empty graph on it so we can use redraw plotly function.

```js
window.onload = function(){{
    vm = initialize_vm_interface(functions);
    graph = document.getElementById('graph');
    Plotly.plot(graph, [graph_data], {{margin: {{t: 0}} }});
}}
```

And then we need to use callback vm.on_break to draw graph.

```js
window.onload = function(){{
    vm = initialize_vm_interface(functions);
    graph = document.getElementById('graph');
    Plotly.plot(graph, [graph_data], {{margin: {{t: 0}} }});

    vm.on_break = function(){{
        // if breakpoint reached before and data been supplied.
        if(graph_data.y === null){{
            return;
        }}
        // update grapth
        var x = [];
        var STEP = 0.01;
        for(var i=0; i<graph_data.y.length; ++i){{
            x.push(i*STEP);
        }}
        graph_data.x = x;
        Plotly.redraw(graph);
    }}
}}
```

After rebuilding page whose two problems should dissapear.
