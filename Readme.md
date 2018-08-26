# Interactive c-alike language web debuger.

Interactives is application for generating web pages, which can be used to 
evaluate code step by step with ability to see intermediate results

## Why?

To help building visualisation of certain algorithms for students

## Why c-alike?

This project started as contribution for numerical analysis course
at my university, which uses C++ language.

C is chosen cause it contains significantly less features, and therefore
it is more realistic to achive suitable language support for C then for C++.

But even so is still require almost unreasonable amount of work for standalone 
developer, so many features might be never implemented (Or implemented incorrectly).

## Usage

Example (from catalog):
```
interactives examples/euler/0.cpp -t examples/euler/template.html
```
```
interactives INPUT [-o OUTPUT] [-t TEMPLATE]
```

+ INPUT - path to C file.
+ OUTPUT - path to output web page file. Requiers vm catalog to be copied to same directory. Default value is "out.html"
+ TEMPLATE - path to template used to generate page. Default value is "vm/index.html"

###### Template syntax
Template uses rust formating syntax with provided values
+ {code_js} - Virtual machine code, function links, debug info
+ {code_html} - Html for source code display

See "vm/index.html" as example.

