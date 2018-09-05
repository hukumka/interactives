# C code

For our C part of tutorial we will want to write:
* function for predicate P (defined in previous page)
* main loop calculating approximation
* some way to allocate array (see [limitations](#limitations))
* some way to tell web page we want to draw graph using data we calculated

But first you will need to create new file "0.c"

## predicate

Let's go with predicate

\\[
    P(f, x) = 2 sqrt(f)
\\]

corresponding to solution

\\[
    f(x) = x^2
\\]


```C
float P(float f, float x){
    return f;
}
```

## main loop

```C
int main(){
    float START=0.0;
    float END=2.0; 

    int COUNT=100;
    float step = (START-END)/COUNT;

    float x = START;
    float f = 1.0;
    
    float* data = allocate_float(COUNT);

    for(int i=0; i<COUNT; ++i){
        data[i] = f;
        f += step * P(f, x);
        x += step;
    }
}
```

Lets break it step by step:

Here we define range on which we integrate.
```C
    float START=0.0;
    float END=2.0; 
```


Define point net.
```C
    int COUNT=100;
    float step = (START-END)/COUNT;
```

Define calculation starting point.
```C
    float x = START;
    float f = 1.0;
```

Most magical part of tutorial. For now let just assume that it returns pointer to
array newly allocated array of COUNT subsequential floats.
```C
    float* data = allocate_float(COUNT);
```

Evaluation of euler method.
```C
    for(int i=0; i<COUNT; ++i){
        data[i] = f;
        f += step * P(f, x);
        x += step;
    }
```

## Linking

So far our code contains one unanotated function: allocate\_float;

To tell our little compiler about it we will use very familiar syntax

```C
float* allocate_float(int size);
```

This will create a call pointer and request for function with this name in generated page.

## result

*Important*: main must be first defined function due to current misimplementation in compiler. 

```C
float* allocate_float(int size);

int main(){
    float START=0.0;
    float END=2.0; 

    int COUNT=100;
    float step = (START-END)/COUNT;

    float x = START;
    float f = 1.0;
    
    float* data = allocate_float(COUNT);

    for(int i=0; i<COUNT; ++i){
        data[i] = f;
        f += step * P(f, x);
        x += step;
    }
}

float P(float f, float x){
    return f;
}
```

## Limitations

* No structs
* No type casting operators.
* Comments are not allowed
* Not preprocessor support
* Only supported types are ```int```, ```float```, ```void```, function pointers and various pointers to those.
* No const type modifier
* No enums
* ...


Features below aren't planned to ever be implemented
* Asm blocks
* Defining multiple variables as single statement
* Standart library support


If you discovered unimplemented feature not mentioned here please create issue on github page <https://github.com/hukumka/interactives>
