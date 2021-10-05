use ast::{
    symbol_table::{DeclType, Symbol},
    AssignOperator, BinaryOperator, Expr, Literal, PostfixOperator, PrefixOperator, PrimeExpr,
    SymbolId,
};
use common::interner::StringId;
use runtime::instructions::Instruction;

use crate::{lexical_info::SymbolInfo, register::Register, Compiler, InstructionId};
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

enum AssignmentTarget {
    Variable(SymbolId),
    Dot(Register, StringId),
    Index(Register, Register),
}

impl AssignmentTarget {
    /// Create the assignment target from an assignment expression.
    pub fn from_expr<A: Allocator + Clone>(this: &mut Compiler<A>, assign: &Expr<A>) -> Self {
        match assign {
            Expr::Prime(PrimeExpr::Variable(symbol)) => AssignmentTarget::Variable(*symbol),
            Expr::UnaryPostfix(expr, PostfixOperator::Dot(name)) => {
                let reg = this.compile_expr(None, expr).place;
                AssignmentTarget::Dot(reg, *name)
            }
            Expr::UnaryPostfix(expr, PostfixOperator::Index(index)) => {
                let tgt = this.compile_expr(None, expr).place;
                let index_expr = this.compile_expr(None, index).place;
                AssignmentTarget::Index(tgt, index_expr)
            }
            x => panic!("expression is not assignable: {:?}", x),
        }
    }

    /// Returns a register which contains the value of the assignment target.
    pub fn compile_use<A: Allocator + Clone>(&self, this: &mut Compiler<A>) -> Register {
        match self {
            Self::Variable(x) => this.compile_symbol_use(None, *x),
            Self::Dot(register, string) => {
                let reg = this.compile_literal(None, Literal::String(*string));
                this.instructions.push(Instruction::Index {
                    dst: reg.0,
                    obj: register.0,
                    key: reg.0,
                });
                reg
            }
            Self::Index(obj, key) => {
                let dst = this.registers.alloc_temp();
                this.instructions.push(Instruction::Index {
                    dst: dst.0,
                    obj: obj.0,
                    key: key.0,
                });
                dst
            }
        }
    }

    /// Returns an optional placement register if the assignment target is an local register.
    pub fn placment<A: Allocator + Clone>(&self, this: &mut Compiler<A>) -> Option<Register> {
        if let Self::Variable(x) = self {
            if let SymbolInfo::Local = this.lexical_info.symbol_info()[*x] {
                Some(this.registers.alloc_symbol(*x))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Compiles the code for assigning this target its value from the given registers
    /// If the target is a local variable the value should already be in the corresponding
    /// register.
    pub fn compile_assign<A: Allocator + Clone>(&self, this: &mut Compiler<A>, dst: Register) {
        match *self {
            Self::Variable(x) => match this.lexical_info.symbol_info()[x] {
                SymbolInfo::Local => {}
                SymbolInfo::Global => {
                    let global = this.registers.alloc_temp();
                    let name = this.compile_literal(
                        None,
                        Literal::String(this.symbol_table.symbols()[x].ident),
                    );
                    this.instructions.push(Instruction::LoadGlobal {
                        dst: global.0,
                        null: 0,
                    });
                    this.registers.free_temp(global);
                    this.registers.free_temp(name);

                    this.instructions.push(Instruction::IndexAssign {
                        obj: global.0,
                        val: dst.0,
                        key: name.0,
                    });
                }
                SymbolInfo::Captured(_x) => todo!(),
                SymbolInfo::Argument => todo!(),
            },
            Self::Dot(obj, name) => {
                let name = this.compile_literal(None, Literal::String(name));
                this.registers.free_temp(name);
                this.instructions.push(Instruction::IndexAssign {
                    obj: obj.0,
                    val: dst.0,
                    key: name.0,
                });
            }
            Self::Index(obj, index) => {
                this.instructions.push(Instruction::IndexAssign {
                    obj: obj.0,
                    val: dst.0,
                    key: index.0,
                });
            }
        }
    }

    pub fn free_temp<A: Allocator + Clone>(&self, this: &mut Compiler<A>) {
        match *self {
            Self::Variable(_) => {}
            Self::Index(a, b) => {
                this.registers.free_temp(a);
                this.registers.free_temp(b);
            }
            Self::Dot(a, _) => {
                this.registers.free_temp(a);
            }
        }
    }
}

macro_rules! match_binary_instruction{
    (match ($this:expr,$dst:expr,$left:expr,$right:expr,$m:expr) { $($op:ident => $instr:ident,)* }) => {

        match $m{
            $(
                BinaryOperator::$op => {$this.instructions.push(Instruction::$instr{
                    dst: $dst.0,
                    left: $left.place.0,
                    righ: $right.place.0,
                });
                return ExprValue::new_in($dst, $this.alloc.clone())
                }
            )*
                _ => {}
        }
    };
}

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    /// Compile the given expression
    /// Will put result of expression in given placement register if there is one.
    pub(crate) fn compile_expr(
        &mut self,
        placement: Option<Register>,
        expr: &Expr<A>,
    ) -> ExprValue<A> {
        match expr {
            Expr::Prime(x) => self.compile_prime(placement, x),
            Expr::Assign(assign, op, expr) => ExprValue::new_in(
                self.compile_assignment(placement, assign, op, expr),
                self.alloc.clone(),
            ),
            Expr::Binary(left, op, right) => self.compile_binary_expr(placement, left, op, right),
            Expr::UnaryPostfix(expr, op) => match *op {
                PostfixOperator::Dot(name) => {
                    let expr = self.compile_expr(None, expr).place;
                    let key = self.compile_literal(None, Literal::String(name));
                    self.registers.free_temp(expr);
                    self.registers.free_temp(key);
                    let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
                    self.instructions.push(Instruction::Index {
                        dst: dst.0,
                        obj: expr.0,
                        key: key.0,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                _ => todo!(),
            },
            Expr::UnaryPrefix(op, expr) => match op {
                PrefixOperator::Not => {
                    let expr = self.compile_expr(None, expr);
                    let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
                    self.registers.free_temp(expr.place);
                    self.instructions.push(Instruction::Not {
                        dst: dst.0,
                        src: expr.place.0 as u16,
                    });
                    ExprValue {
                        place: dst,
                        true_list: expr.false_list,
                        false_list: expr.true_list,
                    }
                }
                _ => todo!(),
            },
        }
    }

    fn compile_binary_expr(
        &mut self,
        placement: Option<Register>,
        left: &Expr<A>,
        op: &BinaryOperator<A>,
        right: &Expr<A>,
    ) -> ExprValue<A> {
        let left = self.compile_expr(None, left);
        let right = self.compile_expr(None, right);

        self.registers.free_temp(left.place);
        self.registers.free_temp(right.place);
        let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());

        // Simple macro to shorten repetitive code.
        // each entry results in roughly
        // ```
        // self.instructions.push(Instruction::value{
        //  ...
        // });
        // return dst
        match_binary_instruction!(match (self, dst, left, right, op) {
            Add => Add,
            Subtract => Sub,
            Multiply => Mul,
            Divide => Div,
            Modulo => Mod,
            Exponentiate => Pow,
            ShiftLeft => ShiftLeft,
            ShiftRight => ShiftRight,
            ShiftRightUnsigned => ShiftUnsigned,
            BitwiseAnd => BitwiseAnd,
            BitwiseXor => BitwiseOr,
            BitwiseOr => BitwiseXor,
            Less => Less,
            LessEqual => LessEq,
            Greater => Greater,
            GreaterEqual => GreaterEq,
            Equal => Equal,
            StrictEqual => SEqual,
            NotEqual => NotEqual,
            StrictNotEqual => SNotEqual,
            In => In,
            InstanceOf => InstanceOf,
        });

        match op {
            BinaryOperator::And => todo!(),
            BinaryOperator::Or => todo!(),
            BinaryOperator::Index => todo!(),
            BinaryOperator::TenaryNull => todo!(),
            BinaryOperator::NullCoalessing(_) => todo!(),
            BinaryOperator::Ternary(_) => todo!(),
            _ => unreachable!(),
        }
    }

    fn compile_assignment(
        &mut self,
        placement: Option<Register>,
        assign: &Expr<A>,
        op: &AssignOperator,
        value: &Expr<A>,
    ) -> Register {
        let assign_target = AssignmentTarget::from_expr(self, assign);
        let place = assign_target.placment(self);

        if let AssignOperator::Assign = op {
            let expr = self.compile_expr(place, value).place;
            assign_target.compile_assign(self, expr);
            return expr;
        }

        let assign_value = assign_target.compile_use(self);
        let expr = self.compile_expr(None, value).place;
        let dst = place
            .or(placement)
            .unwrap_or_else(|| self.registers.alloc_temp());
        self.registers.free_temp(assign_value);
        self.registers.free_temp(expr);

        match op {
            AssignOperator::Assign => unreachable!(),
            AssignOperator::Add => {
                self.instructions.push(Instruction::Add {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::Subtract => {
                self.instructions.push(Instruction::Sub {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            _ => todo!(),
        }

        assign_target.compile_assign(self, dst);
        assign_target.free_temp(self);

        if let Some(x) = placement {
            if let Some(src) = place {
                self.instructions.push(Instruction::Move {
                    dst: x.0,
                    src: src.0 as u16,
                });
            }
        }
        dst
    }

    /// Compile the given prime expression
    /// Will put result of expression in given placement register if there is one.
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

    /// Compile the use of a symbol
    /// Will put result of expression in given placement register if there is one.
    fn compile_symbol_use(&mut self, placement: Option<Register>, symbol_id: SymbolId) -> Register {
        let symbol = &self.symbol_table.symbols()[symbol_id];
        match self.lexical_info.symbol_info()[symbol_id] {
            SymbolInfo::Global => {
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
            SymbolInfo::Local => {
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
            _ => todo!(),
        }
    }

    /// Compile the use of a literal expression
    /// Will put result of expression in given placement register if there is one.
    pub(crate) fn compile_literal(
        &mut self,
        placement: Option<Register>,
        literal: Literal,
    ) -> Register {
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
