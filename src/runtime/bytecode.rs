use super::JSValue;
use std::{fmt, mem};

pub struct Bytecode(pub Box<[u8]>);

pub struct BytecodeReader<'a> {
    cur: *const u8,
    last: *const u8,
    code: &'a [u8],
}

impl<'a> BytecodeReader<'a> {
    pub fn new(code: &'a Bytecode) -> Self {
        let cur = code.0.as_ptr();
        let last = unsafe { code.0.as_ptr().add(code.0.len()) };
        BytecodeReader {
            cur,
            last,
            code: &code.0,
        }
    }

    pub fn read_op(&mut self) -> Option<u8> {
        if self.cur == self.last {
            return None;
        }
        unsafe {
            let res = *self.cur;
            self.cur = self.cur.add(mem::size_of::<u8>());
            Some(res)
        }
    }

    pub fn read_i32(&mut self) -> Option<i32> {
        if self.cur as usize + mem::size_of::<i32>() >= self.last as usize {
            return None;
        }
        unsafe {
            let res = (self.cur as *const i32).read_unaligned();
            self.cur = self.cur.add(mem::size_of::<i32>());
            Some(res)
        }
    }

    pub fn read_js_value(&mut self) -> Option<JSValue> {
        if self.cur as usize + mem::size_of::<JSValue>() >= self.last as usize {
            return None;
        }
        unsafe {
            let res = (self.cur as *const JSValue).read_unaligned();
            self.cur = self.cur.add(mem::size_of::<JSValue>());
            Some(res)
        }
    }
}

#[derive(Clone)]
pub struct BytecodeBuilder(Vec<u8>);

impl BytecodeBuilder {
    pub fn new() -> Self {
        BytecodeBuilder(Vec::new())
    }

    pub fn emit(&mut self, op: Op) {
        self.0.push(op as u8);
    }

    pub fn emit_i32(&mut self, val: i32) {
        let len = self.0.len();
        self.0.extend_from_slice(&[0, 0, 0, 0]);
        unsafe { (self.0.as_mut_ptr().add(len) as *mut i32).write_unaligned(val) };
    }

    pub fn emit_js_value(&mut self, val: JSValue) {
        let len = self.0.len();
        self.0.extend_from_slice(&[0, 0, 0, 0, 0, 0, 0, 0]);
        unsafe { (self.0.as_mut_ptr().add(len) as *mut u64).write_unaligned(val.0.bits) };
    }

    pub fn build(self) -> Bytecode {
        Bytecode(self.0.into_boxed_slice())
    }
}

macro_rules! op_code {
    ($($name:ident $( ($($val:ident: $op:ident,)*) )*,)*) => {

        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug)]
        #[repr(u8)]
        pub enum Op{
            $($name,)*
        }

        pub mod op{
            use super::Op;
            $(pub const $name: u8 = Op::$name as u8;)*
        }

        impl BytecodeBuilder {
            $(
                #[allow(non_snake_case)]
                pub fn $name(&mut self$($(,$val: $op)*)*) -> &mut Self{
                self.emit(Op::$name);
                $($(
                    op_code!(@emit self,$val, $op);
                )*)*
                self
            })*
        }

        impl fmt::Display for Bytecode{
            fn fmt(&self,w: &mut fmt::Formatter) -> fmt::Result{
                let mut reader = BytecodeReader::new(self);
                while let Some(o) = reader.read_op(){
                    match o{
                        $(op::$name => {
                            write!(w,stringify!($name))?;
                            $($(
                                    op_code!(@display w,reader,$val, $op);
                            )*)*
                            writeln!(w)?;
                        })*
                        _ => writeln!(w,"invalid OP!")?,
                    }
                }
                Ok(())
            }
        }

    };

    (@display $writer:ident,$reader:ident,$val:ident, u32) => {
        let $val = if let Some(x) = $reader.read_i32(){ x } else { return writeln!($writer,"\nearly end!"); };
        write!($writer,"\t{}",x)?;
    };
    (@display $writer:ident,$reader:ident,$val:ident, i32) => {
        let $val = if let Some(x) = $reader.read_i32(){ x } else { return writeln!($writer,"\nearly end!");};
        write!($writer,"\t{}",$val)?;
    };
    (@display $writer:ident,$reader:ident,$val:ident, f64) => {
        let $val = if let Some(x) = $reader.read_js_value(){ x } else { return writeln!($writer,"\nearly end!");};
        write!($writer,"\t{}",$val)?;
    };
    (@display $writer:ident,$reader:ident,$val:ident, JSValue) => {
        let $val = if let Some(x) = $reader.read_js_value(){ x } else { return writeln!($writer,"\nearly end!");};
        write!($writer,"\t{}",$val)?;
    };
    (@emit $this:expr, $val:ident, u32) => {
        $this.emit_i32($val as i32)
    };
    (@emit $this:expr, $val:ident, i32) => {
        $this.emit_i32($val)
    };
    (@emit $this:expr, $val:ident, f64) => {
        $this.emit_js_value(JSValue::from_float($val as f64))
    };
    (@emit $this:expr, $val:ident, JSValue) => {
        $this.emit_js_value($val)
    };
    //(@emit $this:expr, $val:ident: $op:ty) => {
        //compile_error!(concat!("unkown operand type: ",stringify!($op)))
    //};
}

op_code! {
    LD_INT(val: i32,),
    LD_VAL(val: JSValue,),
    ADD,
    SUB,
    DIV,
    MUL,
    MOD,
    POW,
    SHL,
    SHR,
    SHRU,
    EQ,
    NEQ,
    SEQ,
    SNEQ,
    LT,
    LTE,
    GT,
    GTE,
    IF_TRUE,
    IF_FALSE,
    JMP(target: i32,),
    RET,
}

/*
#[allow(non_camel_case_types)]
#[derive(Clone, Copy, Debug)]
#[repr(u8)]
pub enum Op {
    LD_INT,
    LD_VAL,

    /// Addition
    ADD,
    /// Subtraction
    SUB,
    /// division
    DIV,
    /// multiplication
    MUL,
    /// modulo
    MOD,
    /// power
    POW,
    /// shift left
    SHL,
    /// shift right signed
    SHR,
    /// shift right signed
    SHRU,
    /// equal
    EQ,
    /// not equal
    NEQ,
    /// strict equal
    SEQ,
    /// strict not equal
    SNEQ,
    /// less then
    LT,
    /// less then or equal
    LTE,
    /// greater then
    GT,
    /// greater then or equal
    GTE,

    /// if true execute the next instruction else jump over one
    IF_TRUE,
    /// if false execute the next instruction else jump over one
    IF_FALSE,
    /// jump to instruction at offset
    JMP(i32),

    RET,
}
*/
