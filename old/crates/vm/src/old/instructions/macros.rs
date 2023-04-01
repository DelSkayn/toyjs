macro_rules! define_instructions {
    (
        pub enum Instruction { $(
                $(#[doc = $com:expr])*
                $name:ident{$($arg:tt)+},
        )* }
    ) => {
        /// Full instruction representation with operands.
        #[derive(Clone,Copy,Eq,PartialEq,Debug)]
        pub enum Instruction{
            $(
                $(#[doc = $com])*
                $name{ $($arg)+ },
            )*
        }


        impl std::fmt::Display for Instruction{
            fn fmt(&self, fmt: &mut std::fmt::Formatter) -> std::fmt::Result{
                $(
                    define_instructions!( @format fmt,*self, $name => $($arg)*);
                )*
                Ok(())
            }
        }
    };

    (@format $w:expr,$v:expr, $name:ident => $a:ident :u8, $b:ident :u8, $c:ident :u8) => {
        if let Instruction::$name{ $a, $b,$c} = $v{
            return write!($w,"{:15}  {:>4}:0x{:<2x}  {:>4}:0x{:<2x}  {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $a,
            stringify!($b),
            $b,
            stringify!($c),
            $c,
            )
        }
    };
    (@format $w:expr,$v:expr, $name:ident => $a:ident :u8, $d:ident :u16) => {
        if let Instruction::$name {$a, $d } = $v{
            return write!($w,"{:15}  {:>4}:0x{:<2x}      :      {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $a,
            stringify!($d),
            $d
            )
        }
    };
    (@format $w:expr,$v:expr,  $name:ident => $a:ident :u8, $d:ident :i16) => {
        if let Instruction::$name {$a, $d } = $v{
            return write!($w,"{:15}  {:>4}:0x{:<2x}      :      {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $a,
            stringify!($d),
            $d
            )
        }
    };

    (@format $w:expr,$v:expr,  $name:ident => $a:ident :u8, $d:ident :u8) => {
        if let Instruction::$name {$a, $d } = $v{
            return write!($w,"{:15}  {:>4}:0x{:<2x}  {:>4}:0x{:<2x}      :    ",
            stringify!($name),
            stringify!($a),
            $a,
            stringify!($d),
            $d
            )
        }
    };

    (@format $w:expr,$v:expr,  $name:ident => $a:ident :u8) => {
        if let Instruction::$name {$a} = $v{
            return write!($w,"{:15}  {:>4}:0x{:<2x}      :          :    ",
            stringify!($name),
            stringify!($a),
            $a,
            )
        }
    };
    (@format $w:expr,$v:expr,  $name:ident => $a:ident :i16) => {
        if let Instruction::$name {$a} = $v{
            return write!($w,"{:15}      :          :      {:>4}:{:#x}",
            stringify!($name),
            stringify!($a),
            $a,
            )
        }
    };

    (@format $w:expr,$v:expr,  $name:ident => _ignore:()) => {
        if let Instruction::$name{ _ignore} = $v{
            return write!($w,"{:15}      :          :          :    ",
            stringify!($name),
            )
        }
    }
}
