use std::io::{
    Write,
    Result
};
use std::collections::HashMap;

use lexer::TokenData;
use syntax_tree::{
    Root,
    VariableDefinition,
    Variable,
    FunctionDefinition,
    Block,
    Expression,
    ExpressionData,
    ForLoop,
    ForLoopInitialization,
    Condition,
    Statement,
    Type,

    TreeItem
};
use syntax_tree::Return;
use compiler::DebugInfo;


pub struct Context<'a>{
    debug_info: Option<&'a DebugInfo>,
    variables: Vec<HashMap<&'a str, usize>>,
    size: usize
}

impl<'a> Context<'a>{
    pub fn new()->Self{
        Self{
            debug_info: None,
            variables: vec![HashMap::new()],
            size: 0
        }
    }

    pub fn set_debug_info(&mut self, debug_info: &'a DebugInfo){
        self.debug_info = Some(debug_info);
    }

    fn push(&mut self){
        self.variables.push(HashMap::new());
    }

    fn pop(&mut self){
        let x = self.variables.pop().expect("Context should not be empty!");
        self.size -= x.len();
    }

    fn define_variable(&mut self, var_def: &'a VariableDefinition<'a>){
        self.variables.last_mut()
            .expect("Context should not be empty")
            .insert(var_def.name().token_str(), self.size);
        self.size += 1;
    }

    fn level(&self)->usize{
        self.variables.len() - 1
    }
}


pub trait PageElement<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>;
}
trait PageElementReplacement{
    fn write_page<T: Write>(&self, writer: &mut T, context: &mut Context)->Result<()>;
}
trait PageElementFunc<T: Write>{
    fn write_page(self, writer: &mut T, context: &mut Context)->Result<()>;
}


impl PageElementReplacement for String{
    fn write_page<T: Write>(&self, writer: &mut T, _context: &mut Context)->Result<()>{
        write!(writer, "{}", self)?;
        Ok(())
    }
}
impl<'a> PageElementReplacement for &'a str{
    fn write_page<T: Write>(&self, writer: &mut T, _: &mut Context)->Result<()>{
        write!(writer, "{}", self)?;
        Ok(())
    }
}
impl<T: Write, Func: FnOnce(&mut T, &mut Context)->Result<()>> PageElementFunc<T> for Func{
    fn write_page(self, writer: &mut T, context: &mut Context)->Result<()>{
        self(writer, context)
    }
}


macro_rules! html{
    ($writer: expr, $context: expr, ) => {
    };

    ($writer: expr, $context: expr, {$c: expr} $($t: tt)*) => {
        $c.write_page($writer, $context)?;
        html!{$writer, $context, $($t)*}
    };
    ($writer: expr, $context: expr, tabs{$($c: tt)*} $($t: tt)*) => {
        write!($writer, "<div class=\"tabs\">")?;
        for _ in 0..$context.level(){
            write!($writer, "<span class=\"tab\"></span>")?;
        }
        html!{$writer, $context, $($c)*}
        write!($writer, "</div>")?;
        html!{$writer, $context, $($t)*}
    };
    ($writer: expr, $context: expr, call{$c: expr} $($t: tt)*) => {
        $c($writer, $context)?;
        html!{$writer, $context, $($t)*}
    };
    ($writer: expr, $context: expr, coma{$c: expr} $($t: tt)*) => {
        let mut iter = $c.into_iter();
        if let Some(e) = iter.next(){
            e.write_page($writer, $context)?;
        }
        for e in iter{
            write!($writer, ", ")?;
            e.write_page($writer, $context)?;
        }
        html!{$writer, $context, $($t)*}
    };

    ($writer: expr, $context: expr, seq{$c: expr} $($t: tt)*) => {
        #[allow(unused_mut)]
        let mut iter = $c.into_iter();
        for e in iter{
            e.write_page($writer, $context)?;
        }
        html!{$writer, $context, $($t)*}
    };
    ($writer: expr, $context: expr, $tag: ident ($($i: ident = $value: expr)*) [$($t: tt)*] $($rest: tt)*) => {
        write!($writer, "<{} ", stringify!($tag))?;
        $(
        let opt_value: Option<_> = $value.into_option();
        if let Some(value) = opt_value{
            write!($writer, "{} = \"{}\"", stringify!($i), value)?;
        }
        )*
        write!($writer, ">")?;
        html!{$writer, $context, $($t)*}
        write!($writer, "</{}>", stringify!($tag))?;
        html!{$writer, $context, $($rest)*}
    };
}


trait IntoOption<T>{
    fn into_option(self)->Option<T>;
}


impl IntoOption<String> for String{
    fn into_option(self)->Option<String>{
        Some(self)
    }
}

impl IntoOption<String> for Option<String>{
    fn into_option(self)->Option<String>{
        self
    }
}

impl<'a> IntoOption<&'a str> for &'a str{
    fn into_option(self)->Option<&'a str>{
        Some(self)
    }
}

impl<'a> IntoOption<&'a str> for Option<&'a str>{
    fn into_option(self)->Option<&'a str>{
        self
    }
}


impl<'a> PageElement<'a> for Root<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>{
        match self{
            Root::VariableDefinition(ref vd) => {
                html!{writer, context,
                    {vd}
                    {";"}
                }
                context.define_variable(vd);
                Ok(())
            },
            Root::FunctionDefinition(ref func) => {
                func.write_page(writer, context)
            }
        }
    }
}


impl<'a> PageElement<'a> for VariableDefinition<'a> {
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>) -> Result<()> {
        html!{ writer, context,
            span(class="variable-definition")[
                {self.variable}
                span(class="variable-definition-eq")[{"="}]
                {self.set_to}
            ]
        }
        Ok(())
    }
}

impl<'a> PageElement<'a> for Variable<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>{
        html!{ writer, context,
            span(class="variable")[
                {self.type_}
                {" "}
                call{|w, c| write_variable(self.name, w, c)}
            ]
        };
        Ok(())
    }
}


impl<'a> PageElement<'a> for Type<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>{
        html!{writer, context,
            span(class="c-type")[
                {self.base}
                {"*".repeat(self.pointer_count).as_str()}
            ]
        }
        Ok(())
    }
}


impl<'a> PageElement<'a> for TokenData<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, _context: &mut Context<'a>)->Result<()>{
        write!(writer, "{}", self.token_str())?;
        Ok(())
    }
}


impl<'a> PageElement<'a> for Expression<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>{
        match self.0.as_ref(){
            ExpressionData::BinaryOperator(bin_op) => {
                let brct_l = need_brackets_left(&bin_op.left, bin_op.operator);
                let brct_r = need_brackets_right(bin_op.operator, &bin_op.right);
                let op = str::replace(bin_op.operator.token_str(), "<", "&lt;")
                    .replace(">", "&gt;");
                html!{writer, context,
                    call{|w, c| write_expression_brackets(w, c, &bin_op.left, brct_l)}
                    span(class="operator")[{op}]
                    call{|w, c| write_expression_brackets(w, c, &bin_op.right, brct_r)}
                }
            },
            ExpressionData::PrefixOperator(op) => {
                let brct = need_brackets_prefix(&op.right);
                html!{writer, context,
                    span(class="operator")[{op.operator}]
                    call{|w, c| write_expression_brackets(w, c, &op.right, brct)}
                }
            },
            ExpressionData::SyffixOperator(op) => {
                let brct = need_brackets_syffix(&op.left);
                html!{writer, context,
                    call{|w, c| write_expression_brackets(w, c, &op.left, brct)}
                    span(class="operator")[{op.operator}]
                }
            },
            ExpressionData::Index(index) => {
                html!{writer, context,
                    {index.expr}
                    span(class="operator")[{"["}]
                    {index.index}
                    span(class="operator")[{"]"}]
                }
            },
            ExpressionData::FunctionCall(fc) => {
                html!{writer, context,
                    span(class="function-name")[{fc.name}]
                    span(class="operator")[{"("}]
                    coma{&fc.arguments}
                    span(class="operator")[{")"}]
                }
            },
            ExpressionData::Variable(var) => {
                write_variable(var, writer, context)?;
            },
            ExpressionData::Constant(c) => {
                html! {writer, context,
                    span(class="constant")[
                        {c}
                    ]
                }
            }
        };
        Ok(())
    }
}

pub fn write_variable<'a, T: Write>(var: &'a TokenData<'a>, writer: &mut T, context: &mut Context<'a>)->Result<()>{
    let var_offset = context.debug_info
        .and_then(|x| x.get_variable_offset(var))
        .map(|x| x.to_string());
    if let Some(var_offset) = var_offset {
        html! {writer, context,
            span(class="variable-name" data_var_offset=var_offset)[
                {var}
            ]
        }
    }else {
        html! {writer, context,
            span(class="variable-name")[
                {var}
            ]
        }
    }
    Ok(())
}

pub fn need_brackets_right<'a>(op: &'a TokenData<'a>, expr: &'a Expression<'a>)->bool{
    match expr.0.as_ref(){
        ExpressionData::BinaryOperator(op2) => Expression::is_precede(op, op2.operator),
        _ => false
    }
}
pub fn need_brackets_left<'a>(expr: &'a Expression<'a>, op: &'a TokenData<'a>)->bool{
    match expr.0.as_ref(){
        ExpressionData::BinaryOperator(op2) => Expression::is_precede(op2.operator, op),
        _ => false
    }
}
pub fn need_brackets_prefix<'a>(expr: &'a Expression<'a>)->bool{
    match expr.0.as_ref(){
        ExpressionData::PrefixOperator(_) => false,
        ExpressionData::SyffixOperator(_) => true,
        ExpressionData::BinaryOperator(_) => true,
        ExpressionData::Index(_) => true,
        ExpressionData::FunctionCall(_) => false,
        ExpressionData::Variable(_) => false,
        _ => true
    }
}
pub fn need_brackets_syffix<'a>(expr: &'a Expression<'a>)->bool{
    match expr.0.as_ref(){
        ExpressionData::PrefixOperator(_) => true,
        ExpressionData::SyffixOperator(_) => false,
        ExpressionData::BinaryOperator(_) => true,
        ExpressionData::Index(_) => false,
        ExpressionData::FunctionCall(_) => false,
        ExpressionData::Variable(_) => false,
        _ => true
    }
}



fn write_expression_brackets<'a, T: Write>(writer: &mut T, context: &mut Context<'a>, expr: &'a Expression<'a>, need_brackets: bool)->Result<()>{
    if need_brackets{
        html!{writer, context,
            span(class="expr-bracket")[{"("}]
            {expr}
            span(class="expr-bracket")[{")"}]
        }
    }else{
        html!{writer, context,
            {expr}
        }
    }
    Ok(())
}


impl<'a> PageElement<'a> for FunctionDefinition<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>{
        html!{writer, context,
            div(class="function-definition")[
                div(class="function-head")[
                    {self.return_type}
                    {" "}
                    span(class="function-name")[
                        {self.name}
                    ]
                    span(class="operator")[{"("}]
                    coma{self.arguments.as_slice()}
                    span(class="operator")[{"){"}]
                ]

                div(class="function-body")[
                    {self.body}
                ]

                span(class="operator")[{"}"}]
            ]
        }
        Ok(())
    }
}


impl<'a> PageElement<'a> for Block<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>{
        context.push();
        html!{writer, context,
            seq{self.statements.as_slice()}
        }
        context.pop();
        Ok(())
    }
}


impl<'a> PageElement<'a> for Statement<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>{
        let offset = context.debug_info
            .and_then(|x| x.get_statement_offset(self))
            .map(|x| x.to_string());

        match self{
            Statement::Expression(e) => {html!{writer, context,
                div(class="statement" data_address=offset)[
                    tabs{
                        {e}
                        {";"}
                    }
                ]
            }},
            Statement::VariableDefinition(vd) => {html!{writer, context,
                div(class="statement" data_address=offset)[
                    tabs{
                        {vd}
                        {";"}
                    }
                ]
            }},
            Statement::Return(Return{expression: e, ..}) => {html!{writer, context,
                div(class="statement" data_address=offset)[
                    tabs{
                        span(class="keyword")[
                            {"return "}
                        ]
                        {e}
                        {";"}
                    }
                ]
            }},
            Statement::Condition(cond) => {html!{writer, context,
                {cond}
            }},
            Statement::ForLoop(loop_) => {html! {writer, context,
                {loop_}
            }}
        };
        Ok(())
    }
}


impl<'a> PageElement<'a> for Condition<'a>{
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>)->Result<()>{
        let offset = context.debug_info
            .and_then(|x| x.get_statement_offset_by_pos(self.first_token().get_pos()))
            .map(|x| x.to_string());
        html!{writer, context,
            div(class="statement" data_address=offset)[
                tabs{
                    span(class="keyword")[{"if"}]
                    {"("}
                    {self.condition}
                    {"){"}
                }
            ]
            {self.then}
        }
        let mut else_ = &self.else_;
        // if else is followed by single statement write "else if" block
        while let &[Statement::Condition(ref cond)] = if let Some(Block{statements: x}) = else_{
            x.as_slice()
        }else{
            &[]
        }{
            let offset = context.debug_info
                .and_then(|x| x.get_statement_offset_by_pos(cond.first_token().get_pos()))
                .map(|x| x.to_string());
            html!{writer, context,
                div(class="statement" data_address=offset)[
                    tabs{
                        {"}"}
                        span(class="keyword")[{"else if"}]
                        {"()"}
                        {cond.condition}
                        {"){"}
                    }
                ]
                {cond.then}
            }
            else_ = &cond.else_;
        }
        if let Some(else_) = else_{
            html!{writer, context,
                tabs{
                    {"}"}
                    span(class="keyword")[{"else"}]
                    {"{"}
                }
                {else_}
                tabs{{"}"}}
            }
        }else{
            html!{writer, context,
                tabs{{"}"}}
            }
        }
        Ok(())
    }
}


impl<'a> PageElement<'a> for ForLoopInitialization<'a> {
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>) -> Result<()> {
        match self {
            ForLoopInitialization::VariableDefinition(vd) => vd.write_page(writer, context),
            ForLoopInitialization::Expression(e) => e.write_page(writer, context)
        }
    }
}


impl<'a> PageElement<'a> for ForLoop<'a> {
    fn write_page<T: Write>(&'a self, writer: &mut T, context: &mut Context<'a>) -> Result<()> {
        let offset = context.debug_info
            .and_then(|x| x.get_statement_offset_by_pos(self.first_token().get_pos()))
            .map(|x| x.to_string());
        html! {writer, context,
            div(class="statement" data_address=offset)[
                tabs{
                    span(class="keyword")[{"for"}]
                    {"("}
                    {self.init}
                    {";"}
                    {self.condition}
                    {";"}
                    {self.step}
                    {"){"}
                }
            ]
            {self.body}
            tabs{{"}"}}
        };
        Ok(())
    }
}
