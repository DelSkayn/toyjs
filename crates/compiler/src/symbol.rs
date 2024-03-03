use bc::{Instruction, Primitive, Reg};

use crate::{
    expr::{ExprPosition, ExprResult},
    variables::{Kind, SymbolId, SymbolUseOrder},
    Compiler, Error, Limits, Result,
};

#[derive(Clone)]
pub enum SymbolPlacement {
    /// Symbol is a register and allocated in register -value
    Argument(u32),
    /// Symbol is in a register
    Register(Reg),
    /// Symbol is swapped out to a wide register.
    SwappedOut(u32),
    /// Symbol is in a given upvalue.
    Upvalue(u32),
    /// Symbol is in a given upvalue and loaded in a register.
    UpvalueLoaded(u32, Reg),
    /// Symbol is stored on the global object.
    Global,
}

#[derive(Debug)]
pub struct Reservation {
    until: SymbolUseOrder,
    symbol: SymbolId,
}

impl PartialEq for Reservation {
    fn eq(&self, other: &Self) -> bool {
        self.until == other.until
    }
}
impl Eq for Reservation {}

impl PartialOrd for Reservation {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.until.cmp(&other.until))
    }
}
impl Ord for Reservation {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.until.cmp(&other.until)
    }
}

impl Compiler<'_> {
    pub fn is_tmp_register(&self, reg: Reg) -> bool {
        self.tmp_registers.get(reg.0 as u8)
    }

    /// Allocates a temporary register for free use, will live until freed.
    pub fn alloc_tmp_register(&mut self) -> Result<Reg> {
        let reg = self.next_free_register()?;
        self.used_registers.set(reg.0 as u8);
        self.tmp_registers.set(reg.0 as u8);
        Ok(reg)
    }

    /// Free a temporary register
    ///
    /// Will only free actually temporary registers. If the register is not allocated as tempory
    /// this function will do nothing.
    pub fn free_tmp_register(&mut self, reg: Reg) {
        if self.tmp_registers.get(reg.0 as u8) {
            self.tmp_registers.unset(reg.0 as u8);
            self.used_registers.unset(reg.0 as u8)
        }
    }

    /// Returs a free register but doesn't allocate it.
    /// Calling this function twice might return the same value.
    /// Use when you would immediatly free an allocated register.
    pub fn next_free_register(&mut self) -> Result<Reg> {
        self.collect_registers();
        let next_free = self.used_registers.next_free();
        if next_free == 128 {
            self.free_registers()?;
            let next_free = self.used_registers.next_free();
            if next_free == 128 {
                return Err(Error::ExceededLimits(Limits::Registers));
            }
            return Ok(Reg(next_free as i8));
        }
        self.function_stack_size = (next_free as u32).max(self.function_stack_size);
        Ok(Reg(next_free as i8))
    }

    pub fn load_global(&mut self) -> Result<Reg> {
        if let Some(reg) = self.global_register {
            Ok(reg)
        } else {
            let reg = self.alloc_tmp_register()?;
            self.emit(Instruction::LoadGlobal { dst: reg })?;
            self.global_register = Some(reg);
            Ok(reg)
        }
    }

    pub fn store_symbol(&mut self, symbol: SymbolId, expr: ExprResult) -> Result<()> {
        let sym = &self.variables.symbols()[symbol];
        let placement = if let Some(p) = self.inflight_symbols.get(&symbol).cloned() {
            p
        } else {
            let kind = sym.kind;
            let placement = match sym.kind {
                Kind::Unresolved | Kind::Global => SymbolPlacement::Global,
                Kind::Function | Kind::Let | Kind::Const => {
                    let register = self.next_free_register()?;
                    self.used_registers.set(register.0 as u8);
                    let reservation = Reservation {
                        symbol,
                        // TODO: check if this is correct. Not sure when last_use_of doesn't return
                        // a value
                        until: self
                            .variables
                            .last_use_of(symbol)
                            .unwrap_or(SymbolUseOrder::last()),
                    };
                    let place = self.reservations.binary_search(&reservation).unwrap_err();
                    self.reservations.insert(place, reservation);
                    SymbolPlacement::Register(register)
                }
                Kind::Arg => panic!("arguments should already have been loaded"),
            };
            self.inflight_symbols.insert(symbol, placement.clone());
            placement
        };
        match placement {
            SymbolPlacement::Argument(place) => {
                if place <= 128 {
                    return expr.assign_to_reg(self, Reg((-(place as i32)) as i8));
                }
                let reg = expr.into_register(self)?;
                let place = i32::try_from(-(place as i64)).unwrap();
                self.emit(Instruction::MoveLong {
                    dst: place,
                    src: reg.0 as i32,
                })?;
                Ok(())
            }
            SymbolPlacement::Register(x) => expr.assign_to_reg(self, x),
            SymbolPlacement::SwappedOut(x) => {
                let reg = expr.into_register(self)?;
                let place = i32::try_from(x).unwrap();
                self.emit(Instruction::MoveLong {
                    dst: place,
                    src: reg.0 as i32,
                })?;
                Ok(())
            }
            SymbolPlacement::Upvalue(_) => to_do!(),
            SymbolPlacement::UpvalueLoaded(_, _) => to_do!(),
            SymbolPlacement::Global => {
                let src = expr.into_register(self)?;
                let ident = self.variables.symbols()[symbol].ident;
                let instr = self.compile_string(ident)?;
                let key = self.alloc_tmp_register()?;
                let global = self.load_global()?;
                self.free_tmp_register(src);
                self.free_tmp_register(key);
                self.patch_dst(instr, key);
                self.emit(Instruction::IndexStore {
                    obj: global,
                    key,
                    src,
                })?;
                self.inflight_symbols
                    .insert(symbol, SymbolPlacement::Global);
                Ok(())
            }
        }
    }

    pub fn load_symbol(&mut self, symbol: SymbolId) -> Result<ExprResult> {
        let sym = &self.variables.symbols()[symbol];
        let placement = if let Some(p) = self.inflight_symbols.get_mut(&symbol) {
            p
        } else {
            let placement = match sym.kind {
                Kind::Unresolved | Kind::Global => SymbolPlacement::Global,
                Kind::Function | Kind::Let | Kind::Const => {
                    let instr = self.emit(Instruction::LoadPrim {
                        dst: Reg::tmp(),
                        imm: Primitive::undefined(),
                    })?;
                    return Ok(ExprResult::new(ExprPosition::InstrDst(instr)));
                }
                Kind::Arg => panic!("arguments should already have been loaded"),
            };
            self.inflight_symbols.entry(symbol).or_insert(placement)
        };
        match *placement {
            SymbolPlacement::Argument(x) => {
                if x <= 128 {
                    let reg = Reg((-(x as i32)) as i8);
                    Ok(ExprResult::new(ExprPosition::Register(reg)))
                } else {
                    let dst = self.emit(Instruction::MoveLong {
                        dst: Reg::tmp().0 as i32,
                        src: -(x as i32),
                    })?;
                    Ok(ExprResult::new(ExprPosition::InstrDst(dst)))
                }
            }
            SymbolPlacement::Register(reg) => Ok(ExprResult::new(ExprPosition::Register(reg))),
            SymbolPlacement::SwappedOut(place) => {
                let reg = self.next_free_register()?;
                let instr = self.emit(Instruction::MoveLong {
                    dst: reg.0 as i32,
                    src: place as i32,
                })?;
                self.used_registers.set(reg.0 as u8);
                self.inflight_symbols
                    .insert(symbol, SymbolPlacement::Register(reg));
                Ok(ExprResult::new(ExprPosition::Register(reg)))
            }
            SymbolPlacement::Upvalue(_) => todo!(),
            SymbolPlacement::UpvalueLoaded(_, _) => todo!(),
            SymbolPlacement::Global => {
                let instr = self.compile_string(sym.ident)?;
                let key = self.alloc_tmp_register()?;
                let global = self.load_global()?;
                self.free_tmp_register(key);
                self.patch_dst(instr, key);
                let instr = self.emit(Instruction::IndexLoad {
                    dst: Reg::tmp(),
                    obj: global,
                    key,
                })?;
                Ok(ExprResult::new(ExprPosition::InstrDst(instr)))
            }
        }
    }

    pub fn free_registers(&mut self) -> Result<()> {
        if let Some(x) = self.global_register {
            self.free_tmp_register(x);
            return Ok(());
        }
        Err(Error::ExceededLimits(Limits::Registers))
    }

    pub fn collect_registers(&mut self) {
        while let Some(first) = self.reservations.first() {
            if self.current_use_order >= first.until {
                let res = self.reservations.remove(0);
                self.free_symbol(res.symbol);
            } else {
                break;
            }
        }
    }

    fn free_symbol(&mut self, id: SymbolId) {
        match self.inflight_symbols.remove(&id).unwrap() {
            SymbolPlacement::Argument(_)
            | SymbolPlacement::Upvalue(_)
            | SymbolPlacement::UpvalueLoaded(_, _)
            | SymbolPlacement::Global => panic!("tried to free unfreeable symbol"),
            SymbolPlacement::Register(reg) => {
                self.used_registers.unset(reg.0 as u8);
            }
            SymbolPlacement::SwappedOut(_) => {
                // This shouldn't be possible since the last use of symbol must be a read. Last
                // writes ought to be ignored.
                panic!("freed symbol was swapped out")
            }
        }
    }
}
