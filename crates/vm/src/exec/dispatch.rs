use bc::{ByteCodeReader, Reg};
use dreck::Owner;

use crate::value::Value;

use super::ExecutionFrame;

impl<'gc, 'own> ExecutionFrame<'gc, 'own> {
    pub(crate) unsafe fn dispatch(
        &mut self,
        mut reader: ByteCodeReader,
        _owner: &mut Owner<'own>,
    ) -> Result<Value<'gc, 'own>, Value<'gc, 'own>> {
        loop {
            match reader.read::<u8>() {
                bc::opcodes::Loadi8 => {
                    let dst = reader.read::<Reg>();
                    let v = reader.read::<i8>();
                    self.stack.write(dst, Value::from(v as i32));
                }
                bc::opcodes::Loadi16 => {
                    let dst = reader.read::<Reg>();
                    let v = reader.read::<i16>();
                    self.stack.write(dst, Value::from(v as i32));
                }
                bc::opcodes::Loadi32 => {
                    let dst = reader.read::<Reg>();
                    let v = reader.read::<i32>();
                    self.stack.write(dst, Value::from(v));
                }
                bc::opcodes::Loadf64 => {
                    let dst = reader.read::<Reg>();
                    let v = reader.read::<f64>();
                    self.stack.write(dst, Value::from(v));
                }
                bc::opcodes::Add => {
                    let dst = reader.read::<Reg>();
                    let left = reader.read::<Reg>();
                    let right = reader.read::<Reg>();

                    let left = self.stack.read(left);
                    let right = self.stack.read(right);
                    let left = left
                        .into_int()
                        .map(|x| x as f64)
                        .unwrap_or_else(|| left.into_float().unwrap());
                    let right = right
                        .into_int()
                        .map(|x| x as f64)
                        .unwrap_or_else(|| right.into_float().unwrap());

                    let res = left + right;
                    let v = if (res as i32 as f64).to_bits() == res.to_bits() {
                        Value::from(res as i32)
                    } else {
                        Value::from(res)
                    };
                    self.stack.write(dst, v);
                }
                bc::opcodes::Ret => {
                    let src = reader.read::<Reg>();
                    return Ok(self.stack.read(src));
                }
                x => panic!("invalid opcode: 0x{x:x}"),
            }
        }
    }
}
