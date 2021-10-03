use ast::{symbol_table::DeclType, BinaryOperator, Expr, Literal, PrimeExpr, SymbolId};
use runtime::instructions::Instruction;

use crate::{register::Register, Compiler, InstructionId};
use std::alloc::Allocator;

pub struct ExprValue<A: Allocator> {
    pub place: Register,
    true_list: Vec<InstructionId, A>,
    false_list: Vec<InstructionId, A>,
}

impl<A: Allocator + Clone> ExprValue<A> {
    pub fn new_in(register: Register, alloc: A) -> Self {
        ExprValue {
            place: register,
            true_list: Vec::new_in(alloc.clone()),
            false_list: Vec::new_in(alloc),
        }
    }
}

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_expr(
        &mut self,
        placement: Option<Register>,
        expr: &Expr<A>,
    ) -> ExprValue<A> {
        match expr {
            Expr::Prime(x) => self.compile_prime(placement, x),
            Expr::Binary(left, op, right) => {
                let left = self.compile_expr(None, left);
                let right = self.compile_expr(None, right);

                self.registers.free_temp(left.place);
                self.registers.free_temp(right.place);
                let reg_dst = placement.unwrap_or_else(|| self.registers.alloc_temp());

                match op {
                    BinaryOperator::Add => {
                        self.instructions.push(Instruction::Add {
                            dst: reg_dst.0,
                            left: left.place.0,
                            righ: right.place.0,
                        });
                    }
                    BinaryOperator::Subtract => {
                        self.instructions.push(Instruction::Sub {
                            dst: reg_dst.0,
                            left: left.place.0,
                            righ: right.place.0,
                        });
                    }
                    BinaryOperator::Multiply => {
                        self.instructions.push(Instruction::Mul {
                            dst: reg_dst.0,
                            left: left.place.0,
                            righ: right.place.0,
                        });
                    }
                    BinaryOperator::Divide => {
                        self.instructions.push(Instruction::Div {
                            dst: reg_dst.0,
                            left: left.place.0,
                            righ: right.place.0,
                        });
                    }
                    BinaryOperator::Modulo => {
                        self.instructions.push(Instruction::Mod {
                            dst: reg_dst.0,
                            left: left.place.0,
                            righ: right.place.0,
                        });
                    }
                    _ => todo!(),
                }
                ExprValue::new_in(reg_dst, self.alloc.clone())
            }
            _ => todo!(),
        }
    }

    fn compile_prime(&mut self, placement: Option<Register>, expr: &PrimeExpr<A>) -> ExprValue<A> {
        match expr {
            PrimeExpr::Variable(symbol) => ExprValue::new_in(
                self.compile_symbol_use(placement, *symbol),
                self.alloc.clone(),
            ),
            PrimeExpr::Covered(x) => x
                .iter()
                .map(|expr| self.compile_expr(placement, expr))
                .last()
                .expect("covered expression did not contain atleast a single expression"),
            PrimeExpr::Literal(x) => {
                ExprValue::new_in(self.compile_literal(placement, *x), self.alloc.clone())
            }
            _ => todo!(),
        }
    }

    fn compile_symbol_use(&mut self, placement: Option<Register>, symbol_id: SymbolId) -> Register {
        let symbol = &self.symbol_table.symbols()[symbol_id];
        match symbol.decl_type {
            DeclType::Global => {
                // Test if it is a true global
                if symbol.decl_scope == self.symbol_table.global() {
                    let name = self.compile_literal(None, Literal::String(symbol.ident));
                    let global = self.registers.alloc_temp();
                    self.instructions.push(Instruction::LoadGlobal {
                        dst: global.0,
                        null: 0,
                    });
                    self.registers.free_temp(global);
                    if let Some(place) = placement {
                        self.registers.free_temp(name);
                        self.instructions.push(Instruction::Index {
                            dst: place.0,
                            key: name.0,
                            obj: global.0,
                        });
                        return place;
                    } else {
                        // Just reuse name temp instruction.
                        self.instructions.push(Instruction::Index {
                            dst: name.0,
                            key: name.0,
                            obj: global.0,
                        });
                        return name;
                    }
                }
                //TODO crossing variables.
                if let Some(place) = placement {
                    let reg = self.registers.alloc_symbol(symbol_id);
                    if reg != place {
                        self.instructions.push(Instruction::Move {
                            dst: place.0,
                            src: reg.0 as u16,
                        });
                    }
                    return place;
                } else {
                    return self.registers.alloc_symbol(symbol_id);
                }
            }
            DeclType::Local | DeclType::Const => {
                if let Some(place) = placement {
                    let reg = self.registers.alloc_symbol(symbol_id);
                    if reg != place {
                        self.instructions.push(Instruction::Move {
                            dst: place.0,
                            src: reg.0 as u16,
                        });
                    }
                    return place;
                } else {
                    return self.registers.alloc_symbol(symbol_id);
                }
            }
            DeclType::Implicit => {
                let name = self.compile_literal(None, Literal::String(symbol.ident));
                let global = self.registers.alloc_temp();
                self.instructions.push(Instruction::LoadGlobal {
                    dst: global.0,
                    null: 0,
                });
                self.registers.free_temp(global);
                if let Some(place) = placement {
                    self.registers.free_temp(name);
                    self.instructions.push(Instruction::Index {
                        dst: place.0,
                        key: name.0,
                        obj: global.0,
                    });
                    return place;
                } else {
                    // Just reuse name temp instruction.
                    self.instructions.push(Instruction::Index {
                        dst: name.0,
                        key: name.0,
                        obj: global.0,
                    });
                    return name;
                }
            }
            _ => todo!(),
        }
    }

    fn compile_literal(&mut self, placement: Option<Register>, literal: Literal) -> Register {
        let register = placement.unwrap_or_else(|| self.registers.alloc_temp());
        let constant = self.constants.push_constant(literal);
        if constant.0 < u16::MAX as u32 {
            self.instructions.push(Instruction::LoadConst {
                dst: register.0,
                cons: constant.0 as u16,
            });
        } else {
            self.instructions.push(Instruction::LoadConstL {
                dst: register.0,
                null: 0,
                cons: constant.0,
            });
        }
        register
    }
}
