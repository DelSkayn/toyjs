use ast::{
    symbol_table::{DeclType, Symbol},
    AssignOperator, BinaryOperator, Expr, Literal, PostfixOperator, PrefixOperator, PrimeExpr,
    SymbolId,
};
use common::interner::StringId;
use vm::instructions::Instruction;

use crate::{
    lexical_info::{ArgAllocInfo, SymbolInfo},
    register::Register,
    Compiler, InstructionId,
};
use std::alloc::Allocator;

pub struct ExprValue<A: Allocator> {
    pub register: Register,
    pub true_list: Vec<InstructionId, A>,
    pub false_list: Vec<InstructionId, A>,
}

impl<A: Allocator + Clone> ExprValue<A> {
    pub fn new_in(register: Register, alloc: A) -> Self {
        ExprValue {
            register,
            true_list: Vec::new_in(alloc.clone()),
            false_list: Vec::new_in(alloc),
        }
    }

    pub fn eval(self, this: &mut Compiler<A>) -> Register {
        self.true_list
            .into_iter()
            .for_each(|x| this.patch_jump(x, this.next_instruction_id()));
        self.false_list
            .into_iter()
            .for_each(|x| this.patch_jump(x, this.next_instruction_id()));
        self.register
    }
}

enum AssignmentTarget {
    Variable(SymbolId),
    Dot(Register, StringId),
    Index(Register, Register),
}

impl AssignmentTarget {
    /// Create the assignment target from an assignment expression.
    pub fn from_expr<'a, A: Allocator + Clone>(
        this: &mut Compiler<'a, A>,
        assign: &'a Expr<A>,
    ) -> Self {
        match assign {
            Expr::Prime(PrimeExpr::Variable(symbol)) => AssignmentTarget::Variable(*symbol),
            Expr::UnaryPostfix(expr, PostfixOperator::Dot(name)) => {
                let reg = this.compile_expr(None, expr).eval(this);
                AssignmentTarget::Dot(reg, *name)
            }
            Expr::UnaryPostfix(expr, PostfixOperator::Index(index)) => {
                let tgt = this.compile_expr(None, expr).eval(this);
                let index_expr = this.compile_expr(None, index).eval(this);
                AssignmentTarget::Index(tgt, index_expr)
            }
            x => panic!("expression is not assignable: {:?}", x),
        }
    }

    /// Returns a register which contains the value of the assignment target.
    pub fn compile_use<A: Allocator + Clone>(
        &self,
        placement: Option<Register>,
        this: &mut Compiler<A>,
    ) -> Register {
        match self {
            Self::Variable(x) => this.compile_symbol_use(placement, *x),
            Self::Dot(register, string) => {
                let reg = this.compile_literal(placement, Literal::String(*string));
                this.instructions.push(Instruction::Index {
                    dst: reg.0,
                    obj: register.0,
                    key: reg.0,
                });
                reg
            }
            Self::Index(obj, key) => {
                let dst = placement.unwrap_or_else(|| this.registers.alloc_temp());
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
    pub fn placement<A: Allocator + Clone>(&self, this: &mut Compiler<A>) -> Option<Register> {
        if let Self::Variable(x) = self {
            if let SymbolInfo::Local = this.lexical_info.symbol_info[*x] {
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
            Self::Variable(x) => match this.lexical_info.symbol_info[x] {
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
                SymbolInfo::Argument(_) => todo!(),
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
                    left: $left.0,
                    righ: $right.0,
                });
                return ExprValue::new_in($dst, $this.alloc.clone())
                }
            )*
                _ => unreachable!()
        }
    };
}

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_expressions(
        &mut self,
        placment: Option<Register>,
        expr: &'a Vec<Expr<A>, A>,
    ) -> ExprValue<A> {
        for e in expr[..expr.len() - 1].iter() {
            let expr = self.compile_expr(None, e).eval(self);
            self.registers.free_temp(expr);
        }
        self.compile_expr(
            placment,
            expr.last()
                .expect("expression node did not have atleast a single expression"),
        )
    }

    /// Compile the given expression
    /// Will put result of expression in given placement register if there is one.
    pub(crate) fn compile_expr(
        &mut self,
        placement: Option<Register>,
        expr: &'a Expr<A>,
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
                    let expr = self.compile_expr(None, expr).eval(self);
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
                PostfixOperator::Call(ref args) => ExprValue::new_in(
                    self.compile_function_call(placement, expr, args),
                    self.alloc.clone(),
                ),
                PostfixOperator::Index(ref key) => {
                    let expr = self.compile_expr(None, expr).eval(self);
                    let key = self.compile_expr(None, key).eval(self);
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
                PostfixOperator::AddOne => {
                    let ass = AssignmentTarget::from_expr(self, expr);
                    let one = self.compile_literal(None, Literal::Integer(1));
                    let mut value = ass.compile_use(placement, self);
                    if let Some(x) = ass.placement(self) {
                        if x == value {
                            let new_value = self.registers.alloc_temp();
                            self.instructions.push(Instruction::Move {
                                src: value.0 as u16,
                                dst: new_value.0,
                            });
                            value = new_value;
                        }
                    }
                    self.registers.free_temp(one);
                    let dst = ass
                        .placement(self)
                        .unwrap_or_else(|| self.registers.alloc_temp());

                    self.instructions.push(Instruction::Add {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    ass.compile_assign(self, dst);
                    self.registers.free_temp(dst);
                    ExprValue::new_in(value, self.alloc.clone())
                }
                PostfixOperator::SubtractOne => {
                    let ass = AssignmentTarget::from_expr(self, expr);
                    let one = self.compile_literal(None, Literal::Integer(1));
                    let mut value = ass.compile_use(placement, self);
                    if let Some(x) = ass.placement(self) {
                        if x == value {
                            let new_value = self.registers.alloc_temp();
                            self.instructions.push(Instruction::Move {
                                src: value.0 as u16,
                                dst: new_value.0,
                            });
                            value = new_value;
                        }
                    }
                    self.registers.free_temp(one);
                    let dst = ass
                        .placement(self)
                        .unwrap_or_else(|| self.registers.alloc_temp());

                    self.instructions.push(Instruction::Sub {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    ass.compile_assign(self, dst);
                    self.registers.free_temp(dst);
                    ExprValue::new_in(value, self.alloc.clone())
                }
            },
            Expr::UnaryPrefix(op, expr) => match op {
                PrefixOperator::Not => {
                    // It seems that binary expressions do not shortcut through a not evaluation.
                    let expr = self.compile_expr(None, expr).eval(self);
                    let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
                    self.registers.free_temp(expr);
                    self.instructions.push(Instruction::Not {
                        dst: dst.0,
                        src: expr.0 as u16,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::Negative => {
                    let expr = self.compile_expr(None, expr).eval(self);
                    let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
                    self.registers.free_temp(expr);
                    self.instructions.push(Instruction::Negative {
                        dst: dst.0,
                        op: expr.0 as u16,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::AddOne => {
                    let ass = AssignmentTarget::from_expr(self, expr);
                    let value = ass.compile_use(None, self);
                    let one = self.compile_literal(None, Literal::Integer(1));
                    self.registers.free_temp(value);
                    self.registers.free_temp(one);
                    let tgt_placement = ass.placement(self);
                    let dst = tgt_placement
                        .or(placement)
                        .unwrap_or_else(|| self.registers.alloc_temp());

                    self.instructions.push(Instruction::Add {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    match (placement, tgt_placement) {
                        (Some(to), Some(from)) => {
                            self.instructions.push(Instruction::Move {
                                src: from.0 as u16,
                                dst: to.0,
                            });
                        }
                        _ => {}
                    }
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::SubtractOne => {
                    let ass = AssignmentTarget::from_expr(self, expr);
                    let value = ass.compile_use(None, self);
                    let one = self.compile_literal(None, Literal::Integer(1));
                    self.registers.free_temp(value);
                    self.registers.free_temp(one);
                    let tgt_placement = ass.placement(self);
                    let dst = tgt_placement
                        .or(placement)
                        .unwrap_or_else(|| self.registers.alloc_temp());

                    self.instructions.push(Instruction::Sub {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    match (placement, tgt_placement) {
                        (Some(to), Some(from)) => {
                            self.instructions.push(Instruction::Move {
                                src: from.0 as u16,
                                dst: to.0,
                            });
                        }
                        _ => {}
                    }
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                _ => todo!(),
            },
        }
    }

    fn compile_binary_expr(
        &mut self,
        placement: Option<Register>,
        left: &'a Expr<A>,
        op: &'a BinaryOperator<A>,
        right: &'a Expr<A>,
    ) -> ExprValue<A> {
        match op {
            BinaryOperator::And => {
                let mut left = self.compile_expr(None, left);
                left.false_list
                    .push(self.instructions.push(Instruction::JumpFalse {
                        cond: left.register.0,
                        tgt: 1,
                    }));
                for i in left.true_list.iter().copied() {
                    self.patch_jump(i, self.next_instruction_id());
                }
                let mut right = self.compile_expr(Some(left.register), right);
                left.false_list.append(&mut right.false_list);
                return ExprValue {
                    register: left.register,
                    true_list: right.true_list,
                    false_list: left.false_list,
                };
            }
            BinaryOperator::Or => {
                let mut left = self.compile_expr(None, left);
                left.true_list
                    .push(self.instructions.push(Instruction::JumpTrue {
                        cond: left.register.0,
                        tgt: 1,
                    }));
                for i in left.false_list.iter().copied() {
                    self.patch_jump(i, self.next_instruction_id());
                }
                let mut right = self.compile_expr(Some(left.register), right);
                left.true_list.append(&mut right.true_list);
                return ExprValue {
                    register: left.register,
                    true_list: left.true_list,
                    false_list: right.false_list,
                };
            }
            BinaryOperator::Index => {
                let left = self.compile_expr(None, left).eval(self);
                let right = self.compile_expr(None, right).eval(self);
                self.registers.free_temp(left);
                self.registers.free_temp(right);
                let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
                self.instructions.push(Instruction::Index {
                    obj: left.0,
                    key: right.0,
                    dst: dst.0,
                });
                return ExprValue::new_in(dst, self.alloc.clone());
            }
            BinaryOperator::TenaryNull => todo!(),
            BinaryOperator::NullCoalessing => {
                let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
                self.compile_expr(Some(dst), left).eval(self);
                let tmp = self.registers.alloc_temp();
                self.instructions.push(Instruction::IsNullish {
                    op: dst.0 as u16,
                    dst: tmp.0,
                });
                self.registers.free_temp(tmp);
                let jump = self.instructions.push(Instruction::JumpFalse {
                    cond: tmp.0,
                    tgt: 1,
                });
                self.compile_expr(Some(dst), right).eval(self);
                self.patch_jump(jump, self.next_instruction_id());
                return ExprValue::new_in(dst, self.alloc.clone());
            }
            BinaryOperator::Ternary(inner) => {
                let left = self.compile_expr(None, left);
                left.true_list
                    .into_iter()
                    .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
                let reg = left.register;
                let jump = self.instructions.push(Instruction::JumpFalse {
                    cond: reg.0,
                    tgt: 1,
                });
                self.registers.free_temp(reg);
                let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
                self.compile_expr(Some(dst), inner).eval(self);
                let jump_after = self
                    .instructions
                    .push(Instruction::Jump { null: 0, tgt: 1 });
                left.false_list
                    .into_iter()
                    .for_each(|x| self.patch_jump(x, self.next_instruction_id()));
                self.patch_jump(jump, self.next_instruction_id());
                self.compile_expr(Some(dst), right).eval(self);
                self.patch_jump(jump_after, self.next_instruction_id());
                return ExprValue::new_in(dst, self.alloc.clone());
            }
            _ => {}
        }

        let left = self.compile_expr(None, left);
        let left = left.eval(self);
        let right = self.compile_expr(None, right).eval(self);
        let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
        self.registers.free_temp(left);
        self.registers.free_temp(right);
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
    }

    fn compile_assignment(
        &mut self,
        placement: Option<Register>,
        assign: &'a Expr<A>,
        op: &'a AssignOperator,
        value: &'a Expr<A>,
    ) -> Register {
        let assign_target = AssignmentTarget::from_expr(self, assign);
        let place = assign_target.placement(self);

        if let AssignOperator::Assign = op {
            let expr = self.compile_expr(place, value).eval(self);
            assign_target.compile_assign(self, expr);
            return expr;
        }

        let assign_value = assign_target.compile_use(None, self);
        let expr = self.compile_expr(None, value).eval(self);
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
    fn compile_prime(
        &mut self,
        placement: Option<Register>,
        expr: &'a PrimeExpr<A>,
    ) -> ExprValue<A> {
        match expr {
            PrimeExpr::Variable(symbol) => ExprValue::new_in(
                self.compile_symbol_use(placement, *symbol),
                self.alloc.clone(),
            ),
            PrimeExpr::Covered(x) => self.compile_expressions(placement, x),
            PrimeExpr::Literal(x) => {
                ExprValue::new_in(self.compile_literal(placement, *x), self.alloc.clone())
            }
            PrimeExpr::Object(bindings) => ExprValue::new_in(
                self.compile_object_literal(placement, bindings),
                self.alloc.clone(),
            ),
            PrimeExpr::Function(scope, symbol, args, stmts) => {
                let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
                let id = self.push_pending_function(*scope, args, stmts);
                if id.requires_long() {
                    self.instructions.push(Instruction::LoadFunctionL {
                        dst: dst.0,
                        null: 0,
                        func: id.0,
                    });
                } else {
                    self.instructions.push(Instruction::LoadFunction {
                        dst: dst.0,
                        func: id.0 as u16,
                    });
                }
                if let Some(_) = symbol {
                    todo!()
                }
                ExprValue::new_in(dst, self.alloc.clone())
            }
        }
    }

    /// Compile the use of a symbol
    /// Will put result of expression in given placement register if there is one.
    fn compile_symbol_use(&mut self, placement: Option<Register>, symbol_id: SymbolId) -> Register {
        let symbol = &self.symbol_table.symbols()[symbol_id];
        match self.lexical_info.symbol_info[symbol_id] {
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
            SymbolInfo::Argument(ArgAllocInfo::Register(reg)) => {
                if let Some(place) = placement {
                    if reg != place {
                        self.instructions.push(Instruction::Move {
                            dst: place.0,
                            src: reg.0 as u16,
                        });
                    }
                    return place;
                } else {
                    return reg;
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

    fn compile_object_literal(
        &mut self,
        placement: Option<Register>,
        bindings: &'a Vec<(StringId, Expr<A>), A>,
    ) -> Register {
        let object = placement.unwrap_or_else(|| self.registers.alloc_temp());
        self.instructions.push(Instruction::CreateObject {
            dst: object.0,
            null: 0,
        });
        for (name, value) in bindings {
            let expr = self.compile_expr(None, value).eval(self);
            let key = self.compile_literal(None, Literal::String(*name));
            self.registers.free_temp(expr);
            self.registers.free_temp(key);
            self.instructions.push(Instruction::IndexAssign {
                obj: object.0,
                val: expr.0,
                key: key.0,
            });
        }

        object
    }

    fn compile_function_call(
        &mut self,
        placement: Option<Register>,
        lhs: &'a Expr<A>,
        args: &'a Vec<Expr<A>, A>,
    ) -> Register {
        let func = self.compile_expr(None, lhs).eval(self);
        for (idx, arg) in args.iter().enumerate() {
            if idx >= 16 {
                todo!()
            }
            let reg = self.compile_expr(None, arg).eval(self);
            self.registers.free_temp(reg);
            self.instructions.push(Instruction::SetArg {
                tgt: idx as u8,
                src: reg.0 as u16,
            });
        }
        self.registers.free_temp(func);
        let dst = placement.unwrap_or_else(|| self.registers.alloc_temp());
        self.instructions.push(Instruction::Call {
            dst: dst.0,
            func: func.0,
            num: args.len() as u8,
        });
        dst
    }
}
