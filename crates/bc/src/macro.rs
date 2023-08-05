/// Macro used to define instructions
#[macro_export]
macro_rules! instruction {
    ($(

            $(#[$attr:meta])*
            $ir_name:ident$({$($field:ident : $ty:ty),*})?),* $(,)?
    ) => {


        /// Instructions as an enum, used in safe code.
        #[derive(Clone,Copy,Debug)]
        #[repr(u8)]
        pub enum Instruction{
            $(
                $(#[$attr])*
                #[doc = "# Operands\n"]
                $ir_name$({ $($field : $ty,)* })* = opcodes::$ir_name,
                )*
        }

        impl Instruction{
            pub fn read(bc: &mut SafeByteCodeReader) -> Option<Self>{
                let mut bc_clone = *bc;
                let res = match bc_clone.read::<u8>()?{
                    $(opcodes::$ir_name => {
                        Self::$ir_name$({
                            $($field: bc_clone.read::<$ty>()?,)*
                        })*
                    })*
                    _ => return None
                };
                *bc = bc_clone;
                Some(res)
            }

            pub fn write(self,write: &mut Vec<u8>){
                match self{
                    $(
                        Self::$ir_name$({$($field,)*})* => {
                            write.push(opcodes::$ir_name);
                            $($(
                                let data = &bytemuck::cast::<_,[u8; std::mem::size_of::<$ty>()]>($field);
                                write.extend_from_slice(data.as_slice());
                            )*)*
                        }
                    )*
                }
            }
        }

        impl std::fmt::Display for Instruction{
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result{
                match *self{
                    $(
                        Self::$ir_name $({$($field,)*})* => {
                            write!(f,"{:15} ",stringify!($ir_name))?;
                            $($(
                                write!(f,"{:>4}:{:<2} ",stringify!($field),$field)?;
                            )*)*
                            Ok(())
                        }
                    )*
                }
            }
        }

        #[derive(Clone,Copy,Eq,PartialEq,Debug)]
        #[repr(u8)]
        /// Opcodes as an enum
        pub enum OpCode{
            $(
                $(#[$attr])*
                #[doc = "# Operands\n"]
                $($(
                    #[doc = concat!(" - `",stringify!($field), "` : [`", stringify!($ty),"`]\n\n")]
                )*)*
                $ir_name,
                )*
        }

        impl OpCode{
            pub fn from_u8(value: u8) -> Option<Self>{
                match value{
                    $(opcodes::$ir_name => Some(OpCode::$ir_name),)*
                    _ => None
                }
            }
        }

        /// Opcodes as constants
        #[allow(non_upper_case_globals)]
        pub mod opcodes{
            #[allow(unused_imports)]
            use super::*;
            $(
                $(#[$attr])*
                #[doc = "# Operands\n"]
                $($(
                    #[doc = concat!(" - `",stringify!($field), "` : [`", stringify!($ty),"`]\n\n")]
                )*)*
                pub const $ir_name: u8 = super::OpCode::$ir_name as u8;
                )*
        }

    };
}
