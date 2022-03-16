use ast::{
    symbol_table::{DeclType, Symbol},
    AssignOperator, BinaryOperator, Expr, Literal, PostfixOperator, PrefixOperator, PrimeExpr,
    SymbolId,
};
use common::interner::StringId;
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
                .patch_jump(x, this.builder.next_instruction_id())
        });
        self.false_list.into_iter().for_each(|x| {
            this.builder
                .patch_jump(x, this.builder.next_instruction_id())
        });
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
        if let Self::Variable(x) = self {
            let symbol = &this.symbol_table.symbols()[*x];
            if symbol.decl_type.is_local() && this.symbol_table.in_scope(*x, this.builder.scope()) {
                Some(this.builder.alloc_symbol(*x))
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
            Self::Variable(x) => {
                let symbol = &this.symbol_table.symbols()[x];
                if !symbol.decl_type.is_local() {
                    let name = this.compile_literal(
                        None,
                        Literal::String(this.symbol_table.symbols()[x].ident),
                    );
                    this.builder.free_temp(name);

                    this.builder.push(Instruction::GlobalAssign {
                        src: src.0,
                        key: name.0,
                    });
                } else {
                    if !this.symbol_table.in_scope(x, this.builder.scope()) {
                        let upvalue = this.builder.capture_upvalue(x, symbol.decl_scope);
                        this.builder.push(Instruction::UpvalueAssign {
                            src: src.0,
                            slot: upvalue.0,
                        });
                    }
                }
            }
            Self::Dot(obj, name) => {
                let name = this.compile_literal(None, Literal::String(name));
                this.builder.free_temp(name);
                this.builder.push(Instruction::IndexAssign {
                    obj: obj.0,
                    val: src.0,
                    key: name.0,
                });
            }
            Self::Index(obj, index) => {
                this.builder.push(Instruction::IndexAssign {
                    obj: obj.0,
                    val: src.0,
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

impl<'a, A: Allocator + Clone> Compiler<'a, A> {
    pub(crate) fn compile_expressions(
        &mut self,
        placment: Option<Register>,
        expr: &'a Vec<Expr<A>, A>,
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
                    let key = self.compile_literal(None, Literal::String(name));
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
                    match (placement, tgt_placement) {
                        (Some(to), Some(from)) => {
                            self.builder.push(Instruction::Move {
                                src: from.0,
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
                    self.builder.free_temp(value);
                    self.builder.free_temp(one);
                    let tgt_placement = ass.placement(self);
                    let dst = tgt_placement
                        .or(placement)
                        .unwrap_or_else(|| self.builder.alloc_temp());

                    self.builder.push(Instruction::Sub {
                        dst: dst.0,
                        left: value.0,
                        righ: one.0,
                    });
                    match (placement, tgt_placement) {
                        (Some(to), Some(from)) => {
                            self.builder.push(Instruction::Move {
                                src: from.0,
                                dst: to.0,
                            });
                        }
                        _ => {}
                    }
                    ExprValue::new_in(dst, self.alloc.clone())
                }
                PrefixOperator::New => {
                    let expr = self.compile_expr(None, expr).eval(self);
                    let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.free_temp(expr);
                    self.builder.push(Instruction::New {
                        dst: dst.0,
                        src: expr.0,
                    });
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
                x => {
                    println!("ast: {:#?}", x);
                    todo!()
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
                let mut left = self.compile_expr(None, left);
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
                let mut left = self.compile_expr(None, left);
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
            BinaryOperator::TenaryNull => todo!(),
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
                        .patch_jump(x, self.builder.next_instruction_id())
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
                        .patch_jump(x, self.builder.next_instruction_id())
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
            _ => todo!(),
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
            PrimeExpr::Function(scope, symbol, args, stmts) => {
                let id = self.compile_function_decl(*scope, args, stmts);
                if let Some(symbol) = symbol {
                    let tgt = AssignmentTarget::Variable(*symbol);
                    let symbol_placement = tgt.placement(self);
                    let dst = symbol_placement
                        .or(placement)
                        .unwrap_or_else(|| self.builder.alloc_temp());
                    self.builder.push(Instruction::LoadFunction {
                        dst: dst.0,
                        func: id.0.try_into().unwrap(),
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

                    self.builder.push(Instruction::LoadFunction {
                        dst: dst.0,
                        func: id.0.try_into().unwrap(),
                    });
                    ExprValue::new_in(dst, self.alloc.clone())
                }
            }
            PrimeExpr::This => {
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::LoadThis { dst: dst.0 });
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
        let symbol = &self.symbol_table.symbols()[symbol_id];
        if symbol.decl_type.is_local() {
            if self.symbol_table.in_scope(symbol_id, self.builder.scope()) {
                if let Some(place) = placement {
                    let reg = self.builder.alloc_symbol(symbol_id);
                    if reg != place {
                        self.builder.push(Instruction::Move {
                            dst: place.0,
                            src: reg.0,
                        });
                    }
                    return place;
                } else {
                    return self.builder.alloc_symbol(symbol_id);
                }
            } else {
                let upvalue = self.builder.capture_upvalue(symbol_id, symbol.decl_scope);
                let place = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::Upvalue {
                    dst: place.0,
                    slot: upvalue.0,
                });
                return place;
            }
        } else {
            let name = self.compile_literal(None, Literal::String(symbol.ident));
            if let Some(place) = placement {
                self.builder.free_temp(name);
                self.builder.push(Instruction::GlobalIndex {
                    dst: place.0,
                    key: name.0,
                });
                return place;
            } else {
                // Just reuse name temp instruction.
                self.builder.push(Instruction::GlobalIndex {
                    dst: name.0,
                    key: name.0,
                });
                return name;
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
        let constant = self.constants.push_constant(literal);
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
        bindings: &'a Vec<(StringId, Expr<A>), A>,
    ) -> Register {
        let mut object = None;
        for (name, value) in bindings {
            let expr = self.compile_expr(None, value).eval(self);
            let key = self.compile_literal(None, Literal::String(*name));
            if object.is_none() {
                let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
                self.builder.push(Instruction::CreateObject { dst: dst.0 });
                object = Some(dst);
            }
            self.builder.free_temp(expr);
            self.builder.free_temp(key);
            self.builder.push(Instruction::IndexAssign {
                obj: object.unwrap().0,
                val: expr.0,
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
            self.builder.free_temp(reg);
            self.builder.push(Instruction::Push { src: reg.0 });
        }
        self.builder.free_temp(func);
        let dst = placement.unwrap_or_else(|| self.builder.alloc_temp());
        self.builder.push(Instruction::Call {
            dst: dst.0,
            func: func.0,
        });
        dst
    }
}
