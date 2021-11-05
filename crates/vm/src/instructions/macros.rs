macro_rules! define_instructions {
    (
        pub enum Instruction { $(
                $(#[doc = $com:expr])*
                $name:ident{$($arg:tt)+},
        )* }
    ) => {
        /// Full instruction representation with operands.
        pub enum Instruction{
            $(
                $(#[doc = $com])*
                $name{ $($arg)+ },
            )*
        }


        #[repr(u8)]
        /// Instruction representated as just there opcode
        pub enum InstructionOpcode{
            $(
                $(#[doc = $com])*
                $name,
            )*
        }

        /// Module containing instruction opcodes as `u8` constants.
        pub mod opcode{
            #![allow(non_upper_case_globals)]

            $(
                $(#[doc = $com])*
                pub const $name:u8 = super::InstructionOpcode::$name as u8;)*
        }

        impl Instruction{
            /// Returns the kind of instruction
            pub fn kind(&self) -> InstructionKind{
                match *self{
                    $(
                        Instruction::$name{..} => define_instructions!( @kind $($arg)+),
                    )*
                }
            }

            /// Returns the opcode of the instruction
            pub fn opcode(&self) -> InstructionOpcode{
                match *self{
                    $(
                        Instruction::$name{..} => InstructionOpcode::$name,
                    )*
                }
            }

            /// Writes the current instruction to a buffer in compact form.
            pub fn write_to_buffer(&self, buffer: &mut Vec<u32>){
                $(
                    define_instructions!(@write self, buffer, $name => $($arg)+);
                )*
            }

            pub fn format_byte_instruction(fmt: &mut std::fmt::Formatter, reader: &mut InstructionReader) -> std::fmt::Result{
                let op = reader.try_read_u8().unwrap();
                write!(fmt,"{:3} ",op)?;
                match op {
                    $(
                    opcode::$name => define_instructions!(@format fmt,reader,$name => $($arg)+),
                    )*
                    _ => panic!("invalid opcode")
                }
}
        }
    };


    (@kind $a:ident :u8, $b:ident :u8,$c:ident :u8) => {
        InstructionKind::A
    };
    (@kind $a:ident :u8, $d:ident :u16) => {
        InstructionKind::D
    };
    (@kind $a:ident :u8, $d:ident :i16) => {
        InstructionKind::D
    };
    (@kind $a:ident :u8, $d:ident :u16, $l:ident :u32) => {
        InstructionKind::L
    };
    (@kind $a:ident :u8, $d:ident :u16, $l:ident :i32) => {
        InstructionKind::L
    };
    (@kind $a:ident :u8, $d:ident :i16, $l:ident :u32) => {
        InstructionKind::L
    };
    (@kind $a:ident :u8, $d:ident :i16, $l:ident :i32) => {
        InstructionKind::L
    };

    (@kind $($arg:tt)*) => {
        compile_error!("invalid instruction variant")
    };


    (@write $this:expr, $buffer:expr, $name:ident => $a:ident :u8, $b:ident :u8,$c:ident :u8) => {
        if let Instruction::$name{ $a, $b, $c } = *$this {
            $buffer.push(u32::from_le_bytes([$this.opcode() as u8, $a, $b,$c]));
            return;
        }
    };
    (@write $this:expr, $buffer:expr, $name:ident => $a:ident :u8, $d:ident :u16) => {
        if let Instruction::$name{ $a, $d } = *$this {
            let [b,c] = $d.to_le_bytes();
            $buffer.push(u32::from_le_bytes([$this.opcode() as u8, $a,b,c]));
            return;
        }
    };
    (@write $this:expr, $buffer:expr, $name:ident => $a:ident :u8, $d:ident :i16) => {
        if let Instruction::$name{ $a, $d } = *$this {
            let [b,c] = $d.to_le_bytes();
            $buffer.push(u32::from_le_bytes([$this.opcode() as u8, $a,b,c]));
            return;
        }
    };
    (@write $this:expr, $buffer:expr, $name:ident => $a:ident :u8, $d:ident :u16, $l:ident :u32) => {
        if let Instruction::$name{ $a, $d,  $l } = *$this {
            let [b,c] = $d.to_le_bytes();
            $buffer.push(u32::from_le_bytes([$this.opcode() as u8, $a,b,c]));
            $buffer.push($l);
            return;
        }
    };
    (@write $this:expr, $buffer:expr, $name:ident => $a:ident :u8, $d:ident :u16, $l:ident :i32) => {
        if let Instruction::$name{ $a, $d, $l } = *$this {
            let [b,c] = $d.to_le_bytes();
            $buffer.push(u32::from_le_bytes([$this.opcode() as u8, $a,b,c]));
            $buffer.push($l as u32);
            return;
        }
    };

    (@write $this:expr, $buffer:expr, $name:ident => $a:ident :u8, $d:ident :i16, $l:ident :u32) => {
        if let Instruction::$name{ $a, $d,  $l } = *$this {
            let [b,c] = $d.to_le_bytes();
            $buffer.push(u32::from_le_bytes([$this.opcode() as u8, $a,b,c]));
            $buffer.push($l);
            return;
        }
    };
    (@write $this:expr, $buffer:expr, $name:ident => $a:ident :u8, $d:ident :i16, $l:ident :i32) => {
        if let Instruction::$name{ $a, $d, $l } = *$this {
            let [b,c] = $d.to_le_bytes();
            $buffer.push(u32::from_le_bytes([$this.opcode() as u8, $a,b,c]));
            $buffer.push($l as u32);
            return;
        }
    };

    (@format $w:expr, $reader:expr, $name:ident => $a:ident :u8, $b:ident :u8, $c:ident :u8) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}  {:>4}:0x{:<2x}  {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $reader.try_read_u8().unwrap(),
            stringify!($b),
            $reader.try_read_u8().unwrap(),
            stringify!($c),
            $reader.try_read_u8().unwrap()
            )
    };
    (@format $w:expr, $reader:expr, $name:ident => $a:ident :u8, $d:ident :u16) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}  ____:____  {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $reader.try_read_u8().unwrap(),
            stringify!($d),
            $reader.try_read_u16().unwrap()
            )
    };
    (@format $w:expr, $reader:expr, $name:ident => $a:ident :u8, $d:ident :i16) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}  ____:____  {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $reader.try_read_u8().unwrap(),
            stringify!($d),
            $reader.try_read_i16().unwrap()
            )
    };
    (@format $w:expr, $reader:expr, $name:ident => $a:ident :u8, $d:ident :u16, $l:ident: u32) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}  ____:____  {:>4}:{:<6x}    {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $reader.try_read_u8().unwrap(),
            stringify!($d),
            $reader.try_read_u16().unwrap(),
            stringify!($l),
            $reader.try_read_u32().unwrap()
            )
    };
    (@format $w:expr, $reader:expr, $name:ident => $a:ident :u8, $d:ident :u16, $l:ident: i32) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}  ____:____  {:>4}:{:<6x}    {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $reader.try_read_u8().unwrap(),
            stringify!($d),
            $reader.try_read_u16().unwrap(),
            stringify!($l),
            $reader.try_read_i32().unwrap()
            )
    };
    (@format $w:expr, $reader:expr, $name:ident => $a:ident :u8, $d:ident :i16, $l:ident: u32) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}  ____:____  {:>4}:{:<6x}    {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $reader.try_read_u8().unwrap(),
            stringify!($d),
            $reader.try_read_i16().unwrap(),
            stringify!($l),
            $reader.try_read_u32().unwrap()
            )
    };
    (@format $w:expr, $reader:expr, $name:ident => $a:ident :u8, $d:ident :i16, $l:ident: i32) => {
        write!($w,"{:15}  {:>4}:0x{:<2x}  ____:____  {:>4}:{:<6x}    {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $reader.try_read_u8().unwrap(),
            stringify!($d),
            $reader.try_read_i16().unwrap(),
            stringify!($l),
            $reader.try_read_i32().unwrap()
            )
    };
}
