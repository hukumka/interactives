//! # pointers
//!
//! Data separated between arrays, and each can be reached using pointer of type DWORD
//! there first two bytes corresponds to position in array and second two to array id.
//! There are two builtin arrays: array '0', which is static data, and '1' which is stack.
//! other arrays can be used as heap data, using `malloc/free`
//!
//! # registers
//!
//! available registers are:
//! %i0-%i16 - common integer registers used in math
//! %f0-%f16 - same for floating point operations
//! %sh - head of stack
//! %sb - register, from which functions count variables
//! %ip - instruction pointer
//! %fl - flags
//!
//! # instructions
//!
//! movi; base for movi is 0x10; additions calculated same for all integer binary instruction
//!
//! `movirr` %r1, %r2 - copy value from integer register %r1 into %r2.
//! represented in two bytes: first is 0x10 signature, second is split into
//! first 4 bits as first register id and second 4 bits as second register id
//!
//! `movicr` x, %r - set value of integer register %r as x, there x is constant
//! represended in 6 bytes: first is 0x11, second is 0x0<<4 + register_id
//! last four bits are value to move
//!
//! `movimr` [%sb + x], %r - set value of integer register %r into value of pointer
//! equal to %sb + x.
//! represented in 4 bytes: first is 0x11, second is 0x1<<4 + register_id
//! last to are x
//!
//! `movirm` %r, [%sb + x] - set value of at [%sb + x] to value in %r.
//! represented in 4 bytes: first is 0x11, second is 0x2<<4 + register_id
//! last to are x
//!
//! `movimr` [x], %r - set value of integer register to value in [x], there x is integer constant
//! represented in 6 bytes: first is 0x11, second is 0x3<<4 + register_id
//! last 4 are x
//!
//! `movirm` %r, [x] - set value of [x] to value of %r, there x is integer constant
//! represented in 6 bytes: first is 0x11, second is 0x4<<4 + register_id
//! last 4 are x
//!
//! `movimr [%r1], %r2 - set value of %r2 to value of [%r1]
//! represented in 2 bytes: 0x12, (r1<<4 + r2)
//!
//! `movirm %r1, [%r2] - set value of [%r2] to value of %r1
//! represented in 2 bytes: 0x12, (r1<<4 + r2)
//!
//! 0x11, [0x50-0xff] - reversed
//!
//! addi; base - 0x14; put sub of arguments into second argument
//! subi; base - 0x18; put difference between second argument and first into second argument
//! muli; base - 0x1B; put multiplication of args into second one
//! divi; base - 0x20; pub division of second onto first into second
//! modi; base - 0x24; put modulo of second onto first into second
//! cmp; base - 0x28; compare two arguments. change flags:
//!     z - set to 1 if values is equal, 0 otherwise
//!     l - set to 1 if second is less then first
//!
//!
//! `jump` %ip - 128 + x - unconditional relative jump
//! represented as 0x30, x
//!
//! `jumpz` %ip - 128 + x - relative jump if z flag is 1
//! 0x31, x
//! `jumpl` %ip - 128 + x - relative jump if l flag is 1
//! 0x32, x
//! `jumpnz` %ip - 128 + x - relative jump if z flag is 0
//! 0x33, x
//! `jumpnl` %ip - 128 + x - relative jump if c flag is 0
//! 0x34, x


use lexer::TokenData;
use syntax_tree::*;



