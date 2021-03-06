macro_rules! op_code{
    (enum Op{
        $(
            $(#[doc = $com:expr])*
            $name:ident($($t:tt)+),
        )*
    }) => {
        /// All the possible instruction OP codes
        #[derive(Clone, Copy, Debug)]
        #[repr(u8)]
        pub enum Op{

            $(
                $(#[doc = $com])*
                $name,
            )*
        }

        /// Module containing the values of all the instruction OP codes expressed as u8 constants
        pub mod op{
            use super::Op;
            $(
                #[allow(non_upper_case_globals)]
                $(#[doc = $com])*
                pub const $name: u8 = Op::$name as u8;
            )*
        }


        /// Format an instruction as a dissassembled like format
        fn format_instr(instr: Instruction, f: &mut fmt::Formatter) -> fmt::Result{
            let op = op_op(instr);
            match op {
                $(
                    op::$name => op_code!(@format $name,f,instr,($($t)*)),
                )*
                _ => write!(f,"invalid op!")
            }
        }

        /// Returns the instruction type of a given instruction operand
        fn get_type(op: Op) -> InstructionType{
            get_type_byte(op as u8)
        }

        /// Returns the instruction type of a given instruction operand as an u8
        /// # Panic
        /// Panics if the given instruction is not a valid instruction.
        fn get_type_byte(op: u8) -> InstructionType{
            match op{
                $(
                    op::$name => op_code!(@ty ($($t)*)),
                )*
                _ => panic!("invalid instruction op code"),
            }
        }
    };

    (@format $n:ident, $w:expr, $i:expr, ($A:ident, $B:ident, $C:ident)) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}  {:>3}:0x{:<2x}  {:>4}:{:#x}",
            stringify!($n),
            stringify!($A),
            op_a($i),
            stringify!($B),
            op_b($i),
            stringify!($C),
            op_c($i),
            )
    };
    (@format $n:ident, $w:expr, $i:expr, ($A:ident, $D:ident)) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}   __:__    {:>4}:{:#x}",
            stringify!($n),
            stringify!($A),
            op_a($i),
            stringify!($D),
            op_d($i),
            )
    };

    (@ty ($A:ident, $B:ident, $C:ident)) => {
        InstructionType::A
    };
    (@ty ($A:ident, $D:ident)) => {
        InstructionType::D
    };
    (@ty ($t:tt*)) => {
        compile_error!("invalid instruction type")
    };
}
