use ast::{
    symbol_table::{DeclType, Symbol},
    ArrowBody, AssignOperator, BinaryOperator, Expr, Literal, PostfixOperator, PrefixOperator,
    PrimeExpr, SymbolId,
};
use common::atom::Atom;
use vm::instructions::Instruction;

use crate::{register::Register, Compiler, InstructionId};
use std::{alloc::Allocator, convert::TryInto};

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
        self.true_list.into_iter().for_each(|x| {
            this.builder
                .patch_jump(x, this.builder.next_instruction_id());
        });
        self.false_list.into_iter().for_each(|x| {
            this.builder
                .patch_jump(x, this.builder.next_instruction_id());
        });
        self.register
    }
}

pub enum AssignmentTarget {
    Variable(SymbolId),
    Dot(Register, Atom),
    Index(Register, Register),
}

impl AssignmentTarget {
    /// Create the assignment target from an assignment expression.
    pub fn from_expr<'a, 'rt, 'cell, A: Allocator + Clone>(
        this: &mut Compiler<'a, 'rt, 'cell, A>,
        assign: &'a Expr<A>,
    ) -> Self {
        match assign {
            Expr::Prime(PrimeExpr::Variable(symbol)) => {
                let symbol = this.builder.symbol_table.resolve_symbol(*symbol);
                AssignmentTarget::Variable(symbol)
            }
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

    pub fn from_symbol(id: SymbolId) -> Self {
        AssignmentTarget::Variable(id)
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
                let reg = this.compile_atom(placement, *string);
                this.builder.push(Instruction::Index {
                    dst: reg.0,
                    obj: register.0,
                    key: reg.0,
                });
                reg
            }
            Self::Index(obj, key) => {
                let dst = placement.unwrap_or_else(|| this.builder.alloc_temp());
                this.builder.push(Instruction::Index {
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
        if let Self::Variable(x) = *self {
            if this.builder.symbol_table.is_symbol_local(x)
                && this
                    .builder
                    .symbol_table
                    .in_scope(x, this.builder.lexical_scope())
            {
                Some(this.builder.alloc_symbol(x))
            } else {
                None
            }
        } else {
            None
        }
    }

    /// Compiles the code for assigning this target its value from the given builder
    /// If the target is a local variable the value should already be in the corresponding
    /// register.
    pub fn compile_assign<A: Allocator + Clone>(&self, this: &mut Compiler<A>, src: Register) {
        match *self {
            Self::Variable(symbol_id) => {
                let symbol = &this.builder.symbol_table.symbols()[symbol_id];
                if !this.builder.symbol_table.is_symbol_local(symbol_id) {
                    let name = this
                        .compile_atom(None, this.builder.symbol_table.symbols()[symbol_id].ident);
                    this.builder.free_temp(name);

                    this.builder.push(Instruction::GlobalAssign {
                        src: src.0,
                        key: name.0,
                    });
                } else if !this
                    .builder
                    .symbol_table
                    .in_scope(symbol_id, this.builder.lexical_scope())
                {
                    let scope = symbol.decl_scope;
                    let upvalue = this.builder.capture_upvalue(symbol_id, scope);
                    this.builder.push(Instruction::UpvalueAssign {
                        src: src.0,
                        slot: upvalue.0,
                    });
                }
            }
            Self::Dot(obj, name) => {
                let name = this.compile_atom(None, name);
                this.builder.free_temp(name);
                this.builder.push(Instruction::IndexAssign {
                    obj: obj.0,
                    src: src.0,
                    key: name.0,
                });
            }
            Self::Index(obj, index) => {
                this.builder.push(Instruction::IndexAssign {
                    obj: obj.0,
                    src: src.0,
                    key: index.0,
                });
            }
        }
    }

    pub fn free_temp<A: Allocator + Clone>(&self, this: &mut Compiler<A>) {
        match *self {
            Self::Variable(_) => {}
            Self::Index(a, b) => {
                this.builder.free_temp(a);
                this.builder.free_temp(b);
            }
            Self::Dot(a, _) => {
                this.builder.free_temp(a);
            }
        }
    }
}

macro_rules! match_binary_instruction{
    (match ($this:expr,$dst:expr,$left:expr,$right:expr,$m:expr) { $($op:ident => $instr:ident,)* }) => {

        match $m{
            $(
                BinaryOperator::$op => {$this.builder.push(Instruction::$instr{
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

impl<'a, 'rt, 'cell, A: Allocator + Clone> Compiler<'a, 'rt, 'cell, A> {
    pub(crate) fn compile_expressions(
        &mut self,
        placment: Option<Register>,
        expr: &'a [Expr<A>],
    ) -> ExprValue<A> {
        for e in expr[..expr.len() - 1].iter() {
            let expr = self.compile_expr(None, e).eval(self);
            self.builder.free_temp(expr);
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
                    let key = self.compile_atom(None, name);
                    self.builder.free_temp(expr);
                    self.builder.free_temp(key);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.push(Instruction::Index {
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
                    self.builder.free_temp(expr);
                    self.builder.free_temp(key);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.push(Instruction::Index {
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
                            let new_value = self.builder.alloc_temp();
                            self.builder.push(Instruction::Move {
                                src: value.0,
                                dst: new_value.0,
                            });
                            value = new_value;
                        }
                    }
                    self.builder.free_temp(one);
                    let dst = ass
                        .placement(self)
                        .unwrap_or_else(|| self.builder.alloc_temp());

                    self.builder.push(Instruction::Add {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    ass.compile_assign(self, dst);
                    ass.free_temp(self);
                    self.builder.free_temp(dst);
                    ExprValue::new_in(value, self.alloc.clone())
                }
                PostfixOperator::SubtractOne => {
                    let ass = AssignmentTarget::from_expr(self, expr);
                    let one = self.compile_literal(None, Literal::Integer(1));
                    let mut value = ass.compile_use(placement, self);
                    if let Some(x) = ass.placement(self) {
                        if x == value {
                            let new_value = self.builder.alloc_temp();
                            self.builder.push(Instruction::Move {
                                src: value.0,
                                dst: new_value.0,
                            });
                            value = new_value;
                        }
                    }
                    self.builder.free_temp(one);
                    let dst = ass
                        .placement(self)
                        .unwrap_or_else(|| self.builder.alloc_temp());

                    self.builder.push(Instruction::Sub {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    ass.compile_assign(self, dst);
                    ass.free_temp(self);
                    self.builder.free_temp(dst);
                    ExprValue::new_in(value, self.alloc.clone())
                }
            },
            Expr::UnaryPrefix(op, expr) => match op {
                PrefixOperator::Not => {
                    // It seems that binary expressions do not shortcut through a not evaluation.
                    let expr = self.compile_expr(None, expr).eval(self);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.free_temp(expr);
                    self.builder.push(Instruction::Not {
                        dst: dst.0,
                        src: expr.0,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::BitwiseNot => {
                    // It seems that binary expressions do not shortcut through a not evaluation.
                    let expr = self.compile_expr(None, expr).eval(self);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.free_temp(expr);
                    self.builder.push(Instruction::BitwiseNot {
                        dst: dst.0,
                        src: expr.0,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::Negative => {
                    let expr = self.compile_expr(None, expr).eval(self);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.free_temp(expr);
                    self.builder.push(Instruction::Negative {
                        dst: dst.0,
                        op: expr.0,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::Positive => {
                    let expr = self.compile_expr(None, expr).eval(self);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.free_temp(expr);
                    self.builder.push(Instruction::Positive {
                        dst: dst.0,
                        op: expr.0,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::AddOne => {
                    let ass = AssignmentTarget::from_expr(self, expr);
                    let value = ass.compile_use(None, self);
                    let one = self.compile_literal(None, Literal::Integer(1));
                    self.builder.free_temp(value);
                    self.builder.free_temp(one);
                    let tgt_placement = ass.placement(self);
                    let dst = tgt_placement
                        .or(placement)
                        .unwrap_or_else(|| self.builder.alloc_temp());

                    self.builder.push(Instruction::Add {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    ass.compile_assign(self, dst);
                    ass.free_temp(self);
                    if let (Some(to), Some(from)) = (placement, tgt_placement) {
                        self.builder.push(Instruction::Move {
                            src: from.0,
                            dst: to.0,
                        });
                    }
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::SubtractOne => {
                    let ass = AssignmentTarget::from_expr(self, expr);
                    let value = ass.compile_use(None, self);
                    let one = self.compile_literal(None, Literal::Integer(1));
                    self.builder.free_temp(value);
                    self.builder.free_temp(one);
                    let tgt_placement = ass.placement(self);
                    let dst = tgt_placement
                        .or(placement)
                        .unwrap_or_else(|| self.builder.alloc_temp());

                    ass.free_temp(self);
                    self.builder.push(Instruction::Sub {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    if let (Some(to), Some(from)) = (placement, tgt_placement) {
                        self.builder.push(Instruction::Move {
                            src: from.0,
                            dst: to.0,
                        });
                    }
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::New => {
                    let dst = self.compile_new(placement, expr);
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::TypeOf => {
                    let expr = self.compile_expr(None, expr).eval(self);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.free_temp(expr);
                    self.builder.push(Instruction::TypeOf {
                        dst: dst.0,
                        src: expr.0,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::Delete => {
                    let ass = AssignmentTarget::from_expr(self, expr);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    match ass {
                        AssignmentTarget::Dot(reg, atom) => {
                            let atom = self.compile_atom(None, atom);
                            self.builder.push(Instruction::Delete {
                                dst: dst.0,
                                obj: reg.0,
                                key: atom.0,
                            });
                            self.builder.free_temp(atom);
                        }
                        AssignmentTarget::Index(obj, key) => {
                            self.builder.push(Instruction::Delete {
                                dst: dst.0,
                                obj: obj.0,
                                key: key.0,
                            });
                        }
                        AssignmentTarget::Variable(_) => {
                            // TODO assigned to global variables.
                            // TODO Strict mode
                            self.compile_literal(Some(dst), Literal::Boolean(false));
                        }
                    }
                    ass.free_temp(self);
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::Void => {
                    let expr = self.compile_expr(None, expr).eval(self);
                    self.builder.free_temp(expr);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.compile_literal(Some(dst), Literal::Undefined);
                    ExprValue::new_in(dst, self.alloc.clone())
                }
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
                let mut left = self.compile_expr(placement, left);
                left.false_list
                    .push(self.builder.push(Instruction::JumpFalse {
                        cond: left.register.0,
                        tgt: 1,
                    }));
                for i in left.true_list.iter().copied() {
                    self.builder
                        .patch_jump(i, self.builder.next_instruction_id());
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
                let mut left = self.compile_expr(placement, left);
                left.true_list
                    .push(self.builder.push(Instruction::JumpTrue {
                        cond: left.register.0,
                        tgt: 1,
                    }));
                for i in left.false_list.iter().copied() {
                    self.builder
                        .patch_jump(i, self.builder.next_instruction_id());
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
                self.builder.free_temp(left);
                self.builder.free_temp(right);
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::Index {
                    obj: left.0,
                    key: right.0,
                    dst: dst.0,
                });
                return ExprValue::new_in(dst, self.alloc.clone());
            }
            BinaryOperator::TenaryNull => todo!("operator tenary null"),
            BinaryOperator::NullCoalessing => {
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.compile_expr(Some(dst), left).eval(self);
                let tmp = self.builder.alloc_temp();
                self.builder.push(Instruction::IsNullish {
                    op: dst.0,
                    dst: tmp.0,
                });
                self.builder.free_temp(tmp);
                let jump = self.builder.push(Instruction::JumpFalse {
                    cond: tmp.0,
                    tgt: 1,
                });
                self.compile_expr(Some(dst), right).eval(self);
                self.builder
                    .patch_jump(jump, self.builder.next_instruction_id());
                return ExprValue::new_in(dst, self.alloc.clone());
            }
            BinaryOperator::Ternary(inner) => {
                let left = self.compile_expr(None, left);
                left.true_list.into_iter().for_each(|x| {
                    self.builder
                        .patch_jump(x, self.builder.next_instruction_id());
                });
                let reg = left.register;
                let jump = self.builder.push(Instruction::JumpFalse {
                    cond: reg.0,
                    tgt: 1,
                });
                self.builder.free_temp(reg);
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.compile_expr(Some(dst), inner).eval(self);
                let jump_after = self.builder.push(Instruction::Jump { tgt: 1 });
                left.false_list.into_iter().for_each(|x| {
                    self.builder
                        .patch_jump(x, self.builder.next_instruction_id());
                });
                self.builder
                    .patch_jump(jump, self.builder.next_instruction_id());
                self.compile_expr(Some(dst), right).eval(self);
                self.builder
                    .patch_jump(jump_after, self.builder.next_instruction_id());
                return ExprValue::new_in(dst, self.alloc.clone());
            }
            _ => {}
        }

        let left = self.compile_expr(None, left);
        let left = left.eval(self);
        let right = self.compile_expr(None, right).eval(self);
        let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
        self.builder.free_temp(left);
        self.builder.free_temp(right);
        // Simple macro to shorten repetitive code.
        // each entry results in roughly
        // ```
        // self.builder.push(Instruction::value{
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
            BitwiseXor => BitwiseXor,
            BitwiseOr => BitwiseOr,
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
        let assign_place = assign_target.placement(self);
        let place = assign_place.or(placement);

        if let AssignOperator::Assign = op {
            let expr = self.compile_expr(place, value).eval(self);
            if let (Some(dst), Some(src)) = (placement, assign_place) {
                self.builder.push(Instruction::Move {
                    dst: dst.0,
                    src: src.0,
                });
            }
            assign_target.compile_assign(self, expr);
            assign_target.free_temp(self);
            return expr;
        }

        let assign_value = assign_target.compile_use(None, self);
        let expr = self.compile_expr(None, value).eval(self);
        let dst = place
            .or(placement)
            .unwrap_or_else(|| self.builder.alloc_temp());
        self.builder.free_temp(assign_value);
        self.builder.free_temp(expr);

        match op {
            AssignOperator::Assign => unreachable!(),
            AssignOperator::Add => {
                self.builder.push(Instruction::Add {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::Subtract => {
                self.builder.push(Instruction::Sub {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::Multiply => {
                self.builder.push(Instruction::Mul {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::Divide => {
                self.builder.push(Instruction::Div {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::ShiftLeft => {
                self.builder.push(Instruction::ShiftLeft {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::ShiftRight => {
                self.builder.push(Instruction::ShiftRight {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::ShiftRightUnsigned => {
                self.builder.push(Instruction::ShiftUnsigned {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::BitwiseOr => {
                self.builder.push(Instruction::BitwiseOr {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::BitwiseAnd => {
                self.builder.push(Instruction::BitwiseAnd {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::BitwiseXor => {
                self.builder.push(Instruction::BitwiseXor {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            AssignOperator::Exponentiate => {
                self.builder.push(Instruction::Pow {
                    dst: dst.0,
                    left: assign_value.0,
                    righ: expr.0,
                });
            }
            ref x => todo!("AssignOperator: {:?}", x),
        }

        assign_target.compile_assign(self, dst);
        assign_target.free_temp(self);

        if let Some(x) = placement {
            if let Some(src) = place {
                self.builder.push(Instruction::Move {
                    dst: x.0,
                    src: src.0,
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
            PrimeExpr::Array(bindings) => ExprValue::new_in(
                self.compile_array_literal(placement, bindings),
                self.alloc.clone(),
            ),
            PrimeExpr::Function(scope, symbol, args, stmts) => {
                let id = self.compile_function_decl(*scope, args, stmts);
                if let Some(symbol) = symbol {
                    let tgt = AssignmentTarget::Variable(*symbol);
                    let symbol_placement = tgt.placement(self);
                    let dst = symbol_placement
                        .or(placement)
                        .unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.push(Instruction::LoadConstructor {
                        dst: dst.0,
                        func: id.0,
                    });
                    if let Some(x) = symbol_placement.and(placement) {
                        self.builder.push(Instruction::Move {
                            dst: x.0,
                            src: dst.0,
                        });
                    }
                    tgt.compile_assign(self, dst);
                    ExprValue::new_in(dst, self.alloc.clone())
                } else {
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());

                    self.builder.push(Instruction::LoadConstructor {
                        dst: dst.0,
                        func: id.0,
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
            }
            PrimeExpr::ArrowArgs(_) => {
                panic!("PrimeExpr::ArrowArgs made it into completed ast!")
            }
            PrimeExpr::ArrowFunction(scope, args, body) => {
                let id = self.compile_arrow_function_decl(*scope, args, body);
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::LoadFunction {
                    dst: dst.0,
                    func: id.0,
                });
                ExprValue::new_in(dst, self.alloc.clone())
            }
            PrimeExpr::This => {
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::LoadThis { dst: dst.0 });
                ExprValue::new_in(dst, self.alloc.clone())
            }
            PrimeExpr::NewTarget => {
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::LoadTarget { dst: dst.0 });
                ExprValue::new_in(dst, self.alloc.clone())
            }
            PrimeExpr::Eval(args) => {
                let expr = self.compile_expressions(None, args).eval(self);
                self.builder.push(Instruction::Push { src: expr.0 });
                self.builder.free_temp(expr);

                let cons = self.constants.push_string("eval");
                let tmp = self.builder.alloc_temp();
                self.builder.push(Instruction::LoadConst {
                    dst: tmp.0,
                    cons: cons.0 as u16,
                });
                self.builder.push(Instruction::GlobalIndex {
                    dst: tmp.0,
                    key: tmp.0,
                });
                let dst = placement.unwrap_or(tmp);
                self.builder.push(Instruction::Call {
                    dst: dst.0,
                    func: tmp.0,
                });
                if dst != tmp {
                    self.builder.free_temp(tmp);
                }
                ExprValue::new_in(dst, self.alloc.clone())
            }
        }
    }

    /// Compile the use of a symbol
    /// Will put result of expression in given placement register if there is one.
    fn compile_symbol_use(&mut self, placement: Option<Register>, symbol_id: SymbolId) -> Register {
        let symbol_id = self.builder.symbol_table.resolve_symbol(symbol_id);
        let symbol = &self.builder.symbol_table.symbols()[symbol_id];
        if self.builder.symbol_table.is_symbol_local(symbol_id) {
            if self
                .builder
                .symbol_table
                .in_scope(symbol_id, self.builder.lexical_scope())
            {
                if let Some(place) = placement {
                    let reg = self.builder.alloc_symbol(symbol_id);
                    if reg != place {
                        self.builder.push(Instruction::Move {
                            dst: place.0,
                            src: reg.0,
                        });
                    }
                    place
                } else {
                    self.builder.alloc_symbol(symbol_id)
                }
            } else {
                let scope = symbol.decl_scope;
                let upvalue = self.builder.capture_upvalue(symbol_id, scope);
                let place = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::Upvalue {
                    dst: place.0,
                    slot: upvalue.0,
                });
                place
            }
        } else {
            let ident = symbol.ident;
            let name = self.compile_atom(None, ident);
            if let Some(place) = placement {
                self.builder.free_temp(name);
                self.builder.push(Instruction::GlobalIndex {
                    dst: place.0,
                    key: name.0,
                });
                place
            } else {
                // Just reuse name temp instruction.
                self.builder.push(Instruction::GlobalIndex {
                    dst: name.0,
                    key: name.0,
                });
                name
            }
        }
    }

    /// Compile the use of a literal expression
    /// Will put result of expression in given placement register if there is one.
    pub(crate) fn compile_literal(
        &mut self,
        placement: Option<Register>,
        literal: Literal,
    ) -> Register {
        let register = placement.unwrap_or_else(|| self.builder.alloc_temp());
        let constant = self.constants.push_literal(literal);
        if constant.0 < u16::MAX as u32 {
            self.builder.push(Instruction::LoadConst {
                dst: register.0,
                cons: constant.0 as u16,
            });
        } else {
            panic!("To many constants")
        }
        register
    }

    /// Compile the use of a literal expression
    /// Will put result of expression in given placement register if there is one.
    pub(crate) fn compile_atom(&mut self, placement: Option<Register>, ident: Atom) -> Register {
        let register = placement.unwrap_or_else(|| self.builder.alloc_temp());
        let constant = self.constants.push_atom(ident);
        if constant.0 < u16::MAX as u32 {
            self.builder.push(Instruction::LoadConst {
                dst: register.0,
                cons: constant.0 as u16,
            });
        } else {
            panic!("To many constants")
        }
        register
    }

    /// Compile the use of a string expression
    /// Will put result of expression in given placement register if there is one.
    pub(crate) fn compile_string(&mut self, placement: Option<Register>, string: &str) -> Register {
        let register = placement.unwrap_or_else(|| self.builder.alloc_temp());
        let constant = self.constants.push_string(string);
        if constant.0 < u16::MAX as u32 {
            self.builder.push(Instruction::LoadConst {
                dst: register.0,
                cons: constant.0 as u16,
            });
        } else {
            panic!("To many constants")
        }
        register
    }

    fn compile_object_literal(
        &mut self,
        placement: Option<Register>,
        bindings: &'a Vec<(Atom, Expr<A>), A>,
    ) -> Register {
        let mut object = None;
        for (name, value) in bindings {
            let expr = self.compile_expr(None, value).eval(self);
            let key = self.compile_atom(None, *name);
            if object.is_none() {
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::CreateObject { dst: dst.0 });
                object = Some(dst);
            }
            self.builder.free_temp(expr);
            self.builder.free_temp(key);
            self.builder.push(Instruction::IndexAssign {
                obj: object.unwrap().0,
                src: expr.0,
                key: key.0,
            });
        }
        if object.is_none() {
            let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
            self.builder.push(Instruction::CreateObject { dst: dst.0 });
            object = Some(dst);
        }
        object.unwrap()
    }

    fn compile_array_literal(
        &mut self,
        placement: Option<Register>,
        bindings: &'a [Expr<A>],
    ) -> Register {
        let mut array = None;
        for (idx, value) in bindings.iter().enumerate() {
            let expr = self.compile_expr(None, value).eval(self);
            let key = self.compile_literal(None, Literal::Integer(idx.try_into().unwrap()));
            if array.is_none() {
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::CreateArray { dst: dst.0 });
                array = Some(dst);
            }
            self.builder.free_temp(expr);
            self.builder.free_temp(key);
            self.builder.push(Instruction::IndexAssign {
                obj: array.unwrap().0,
                src: expr.0,
                key: key.0,
            });
        }
        if array.is_none() {
            let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
            self.builder.push(Instruction::CreateArray { dst: dst.0 });
            array = Some(dst);
        }
        array.unwrap()
    }

    fn compile_new(&mut self, placement: Option<Register>, rhs: &'a Expr<A>) -> Register {
        let func = if let Expr::UnaryPostfix(expr, PostfixOperator::Call(args)) = rhs {
            let func = self.compile_expr(None, expr).eval(self);
            for (idx, arg) in args.iter().enumerate() {
                if idx >= 16 {
                    todo!("more then 16 arguments")
                }
                let reg = self.compile_expr(None, arg).eval(self);
                self.builder.free_temp(reg);
                self.builder.push(Instruction::Push { src: reg.0 });
            }
            func
        } else {
            self.compile_expr(None, rhs).eval(self)
        };
        self.builder.free_temp(func);
        let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
        self.builder.push(Instruction::CallConstruct {
            dst: dst.0,
            func: func.0,
            obj: func.0,
        });
        dst
    }

    fn compile_function_call(
        &mut self,
        placement: Option<Register>,
        lhs: &'a Expr<A>,
        args: &'a [Expr<A>],
    ) -> Register {
        enum CallType {
            Method { obj: Register, key: Register },
            Procedure { func: Register },
        }

        // Handle methods
        let instr = match *lhs {
            Expr::UnaryPostfix(ref lhs, PostfixOperator::Dot(ident)) => {
                let obj = self.compile_expr(None, lhs).eval(self);
                let key = self.compile_atom(None, ident);
                CallType::Method { key, obj }
            }
            Expr::UnaryPostfix(ref lhs, PostfixOperator::Index(ref expr)) => {
                let obj = self.compile_expr(None, lhs).eval(self);
                let key = self.compile_expr(None, expr).eval(self);
                CallType::Method { key, obj }
            }
            ref lhs => {
                let func = self.compile_expr(None, lhs).eval(self);
                CallType::Procedure { func }
            }
        };
        // Temp fix for args
        // Don't like allocing so many registers for each argument
        let args: Vec<_> = args
            .iter()
            .enumerate()
            .map(|(idx, arg)| {
                if idx >= 16 {
                    todo!()
                }
                self.compile_expr(None, arg).eval(self)
            })
            .collect();

        for r in args {
            self.builder.push(Instruction::Push { src: r.0 });
            self.builder.free_temp(r);
        }

        match instr {
            CallType::Method { obj, key } => {
                self.builder.free_temp(obj);
                self.builder.free_temp(key);
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::CallMethod {
                    obj: obj.0,
                    dst: dst.0,
                    key: key.0,
                });
                dst
            }
            CallType::Procedure { func } => {
                self.builder.free_temp(func);
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::Call {
                    func: func.0,
                    dst: dst.0,
                });
                dst
            }
        }
    }
}
