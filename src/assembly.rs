use types::{
    Type
};


enum Code{
    JUMP,
    JUMP_Z,
    JUMP_NZ,

    SUM,
    DIFF,
    MUL,
    DIV,
    MOD,

    EQ,
    NOT_EQ,
    LESS,
    LESS_OR_EQ,

    NOT,

    DEREF,
    PTR,

    SET,
}


struct Operation{
    type_: Type,
    code: Code,
    args: Vec<usize>
}
