macro_rules! op_code{
    (enum Op{
        $(
            $(#[doc = $com:expr])*
            $name:ident($($t:tt)+),
        )*
    }) => {
        #[allow(non_camel_case_types)]
        #[derive(Clone, Copy, Debug)]
        #[repr(u8)]
        pub enum Op{

            $(
                $(#[doc = $com])*
                $name,
            )*
        }

        pub mod op{
            use super::Op;
            $(
                $(#[doc = $com])*
                pub const $name: u8 = Op::$name as u8;
            )*
        }


        fn format_instr(instr: Instruction, f: &mut fmt::Formatter) -> fmt::Result{
            let op = op_op(instr);
            match op {
                $(
                    op::$name => op_code!(@format $name,f,instr,($($t)*)),
                )*
                _ => write!(f,"invalid op!")
            }
        }

        fn get_type(op: u8) -> InstructionType{
            match op{
                $(
                    op::$name => op_code!(@ty ($($t)*)),
                )*
                _ => panic!("invalid instruction op code"),
            }
        }
    };

    (@format $n:ident, $w:expr, $i:expr, ($A:ident, $B:ident, $C:ident)) => {
        write!($w,"{:4}  {:3}:0x{:<2x}  {:>3}:0x{:<2x}  {:>4}:{:#x}",
            stringify!($n),
            stringify!($A),
            ($i & A_MASK) >> 8,
            stringify!($B),
            ($i & B_MASK) >> 16,
            stringify!($C),
            ($i & C_MASK) >> 24,
            )
    };
    (@format $n:ident, $w:expr, $i:expr, ($A:ident, $D:ident)) => {
        write!($w,"{:4}  {:3}:0x{:<2x}    _:_     {:>4}:{:#x}",
            stringify!($n),
            stringify!($A),
            ($i & A_MASK) >> 8,
            stringify!($D),
            ($i & D_MASK) >> 16,
            )
    };

    (@ty ($A:ident, $B:ident, $C:ident)) => {
        InstructionType::A
    };
    (@ty ($A:ident, $D:ident)) => {
        InstructionType::D
    };
}
