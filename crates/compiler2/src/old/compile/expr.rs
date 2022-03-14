use crate::Compiler;
use crate::{constants::Constant, ssa::*};
use ast::*;
use common::interner::StringId;
use std::alloc::Allocator;
use std::ptr;

#[must_use]
pub struct CompiledExpr<A: Allocator> {
    id: SsaId,
    pub true_list: Vec<SsaId, A>,
    pub false_list: Vec<SsaId, A>,
}

impl<A: Allocator> CompiledExpr<A> {
    pub fn new_in(id: SsaId, alloc: A) -> Self {
        CompiledExpr {
            id,
            true_list: Vec::new_in(alloc),
            false_list: Vec::new_in(alloc),
        }
    }

    pub fn id(&self) -> SsaId {
        self.id
    }

    pub fn eval(self, compiler: &mut Compiler<A>) -> SsaId {
        let cur = compiler.ssa.cur();
        for j in self.true_list.iter().copied() {
            compiler.ssa.patch_jump(j, cur)
        }
        for j in self.false_list.iter().copied() {
            compiler.ssa.patch_jump(j, cur)
        }
        self.id
    }
}

#[derive(Copy, Clone, Debug)]
pub enum Place {
    Object { object: SsaId, key: SsaId },
    Variable(SymbolId),
}

impl<'a, A: Allocator> Compiler<'a, A> {
    pub(crate) fn compile_expr(&mut self, expr: &Expr<A>) -> CompiledExpr<A> {
        match expr {
            Expr::UnaryPostfix(expr, op) => match *op {
                PostfixOperator::Dot(x) => {
                    let expr = self.compile_expr(expr);
                    let constant = self.module.constants.add_string(x);
                    let constant = self.ssa.insert(Ssa::LoadString { constant });
                    CompiledExpr::new_in(
                        self.ssa.insert(Ssa::Index {
                            object: expr.id,
                            key: constant,
                        }),
                        self.alloc,
                    )
                }
                PostfixOperator::Index(ref key) => {
                    let key = self.compile_expr(key).id;
                    let object = self.compile_expr(expr).id;
                    CompiledExpr::new_in(self.ssa.insert(Ssa::Index { object, key }), self.alloc)
                }
                PostfixOperator::Call(ref args) => {
                    if args.len() > 0 {
                        todo!()
                    }
                    let function = self.compile_expr(expr).id;
                    CompiledExpr::new_in(
                        self.ssa.insert(Ssa::Call {
                            function,
                            args: None,
                        }),
                        self.alloc,
                    )
                }
                ref x @ PostfixOperator::AddOne | ref x @ PostfixOperator::SubtractOne => {
                    let place = self.compile_place(expr);
                    let value = self.compile_place_use(place);
                    let value = self.ssa.push(Ssa::Unary {
                        op: UnaryOperation::ToNumber,
                        operand: value,
                    });
                    let constant = self.module.constants.add(Constant::Integer(1));
                    let one = self.ssa.push(Ssa::LoadConstant { constant });
                    let op = if *x == PostfixOperator::AddOne {
                        BinaryOperation::Add
                    } else {
                        BinaryOperation::Subtract
                    };
                    let added = self.ssa.push(Ssa::Binary {
                        op,
                        left: value,
                        right: one,
                    });
                    self.compile_assignment(place, added);
                    CompiledExpr::new_in(value, self.alloc)
                }
            },
            Expr::Binary(left, op, right) => {
                let op = match *op {
                    BinaryOperator::Add => BinaryOperation::Add,
                    BinaryOperator::Subtract => BinaryOperation::Subtract,
                    BinaryOperator::Multiply => BinaryOperation::Multiply,
                    BinaryOperator::Divide => BinaryOperation::Divide,
                    BinaryOperator::Modulo => BinaryOperation::Modulo,
                    BinaryOperator::Exponentiate => BinaryOperation::Exponentiate,
                    BinaryOperator::Equal => BinaryOperation::Equal,
                    BinaryOperator::NotEqual => BinaryOperation::NotEqual,
                    BinaryOperator::StrictEqual => BinaryOperation::StrictEqual,
                    BinaryOperator::StrictNotEqual => BinaryOperation::StrictNotEqual,
                    BinaryOperator::Less => BinaryOperation::Less,
                    BinaryOperator::LessEqual => BinaryOperation::LessEqual,
                    BinaryOperator::Greater => BinaryOperation::Greater,
                    BinaryOperator::GreaterEqual => BinaryOperation::GreaterEqual,
                    BinaryOperator::BitwiseOr => BinaryOperation::BitwiseOr,
                    BinaryOperator::BitwiseAnd => BinaryOperation::BitwiseAnd,
                    BinaryOperator::BitwiseXor => BinaryOperation::BitwiseXor,
                    BinaryOperator::ShiftLeft => BinaryOperation::LeftShift,
                    BinaryOperator::ShiftRight => BinaryOperation::RightShift,
                    BinaryOperator::ShiftRightUnsigned => BinaryOperation::UnsignedRightShift,
                    BinaryOperator::And => {
                        let mut left = self.compile_expr(left);
                        let false_jump = self.ssa.insert(Ssa::ConditionalJump {
                            condition: left.id,
                            to: None,
                            jump_true: false,
                        });
                        left.false_list.push(false_jump);
                        let next = self.ssa.cur().next();
                        for j in left.true_list.iter().copied() {
                            self.ssa.patch_jump(j, next);
                        }
                        let mut right = self.compile_expr(right);
                        left.false_list.append(&mut right.false_list);
                        let alias = self.ssa.insert(Ssa::Alias {
                            left: left.id,
                            right: right.id,
                        });
                        return CompiledExpr {
                            id: alias,
                            true_list: right.true_list,
                            false_list: left.false_list,
                        };
                    }
                    BinaryOperator::Or => {
                        let mut left = self.compile_expr(left);
                        let true_jump = self.ssa.insert(Ssa::ConditionalJump {
                            condition: left.id,
                            to: None,
                            jump_true: true,
                        });
                        left.true_list.push(true_jump);
                        let next = self.ssa.cur().next();
                        for j in left.false_list.iter().copied() {
                            self.ssa.patch_jump(j, next);
                        }
                        let mut right = self.compile_expr(right);
                        left.true_list.append(&mut right.true_list);
                        let alias = self.ssa.insert(Ssa::Alias {
                            left: left.id,
                            right: right.id,
                        });
                        return CompiledExpr {
                            id: alias,
                            true_list: left.true_list,
                            false_list: right.false_list,
                        };
                    }
                    BinaryOperator::Ternary(ref middle) => {
                        let left = self.compile_expr(left).id;
                        let jump_before = self.ssa.insert(Ssa::ConditionalJump {
                            condition: left,
                            to: None,
                            jump_true: false,
                        });
                        let middle = self.compile_expr(middle).id;
                        let jump_after = self.ssa.insert(Ssa::Jump { to: None });
                        let right = self.compile_expr(right).id;
                        self.ssa.patch_jump(jump_before, jump_after.next());
                        self.ssa.patch_jump(jump_after, middle.next());
                        return CompiledExpr::new_in(
                            self.ssa.push(Ssa::Alias {
                                left: middle,
                                right,
                            }),
                            self.alloc,
                        );
                    }
                    _ => todo!(),
                };
                let left = self.compile_expr(left).id;
                let right = self.compile_expr(right).id;
                CompiledExpr::new_in(self.ssa.insert(Ssa::Binary { op, left, right }), self.alloc)
            }
            Expr::UnaryPrefix(op, right) => {
                let op = match *op {
                    PrefixOperator::Not => UnaryOperation::Not,
                    PrefixOperator::Positive => UnaryOperation::ToNumber,
                    PrefixOperator::Negative => UnaryOperation::Negative,
                    PrefixOperator::BinaryNot => UnaryOperation::BinaryNot,
                    PrefixOperator::TypeOf => UnaryOperation::Typeof,
                    ref x @ PrefixOperator::AddOne | ref x @ PrefixOperator::SubtractOne => {
                        let place = self.compile_place(right);
                        let value = self.compile_place_use(place);
                        let value = self.ssa.push(Ssa::Unary {
                            op: UnaryOperation::ToNumber,
                            operand: value,
                        });
                        let constant = self.module.constants.add(Constant::Integer(1));
                        let one = self.ssa.push(Ssa::LoadConstant { constant });
                        let op = if *x == PrefixOperator::AddOne {
                            BinaryOperation::Add
                        } else {
                            BinaryOperation::Subtract
                        };
                        let added = self.ssa.push(Ssa::Binary {
                            op,
                            left: value,
                            right: one,
                        });
                        self.compile_assignment(place, added);
                        return CompiledExpr::new_in(added, self.alloc);
                    }
                    _ => todo!(),
                };
                let right = self.compile_expr(right).id;
                CompiledExpr::new_in(
                    self.ssa.insert(Ssa::Unary { op, operand: right }),
                    self.alloc,
                )
            }
            Expr::Prime(x) => CompiledExpr::new_in(self.compile_prime(x), self.alloc),
            Expr::Assign(left, AssignOperator::Assign, right) => {
                let right = self.compile_expr(right).id;
                let place = self.compile_place(left);
                self.compile_assignment(place, right);
                CompiledExpr::new_in(right, self.alloc)
            }
            Expr::Assign(left, op, right) => {
                let right = self.compile_expr(right).id;
                let place = self.compile_place(left);
                let place_use = self.compile_place_use(place);
                let op = match op {
                    AssignOperator::Assign => unreachable!(),
                    AssignOperator::Add => BinaryOperation::Add,
                    AssignOperator::Subtract => BinaryOperation::Subtract,
                    AssignOperator::Multiply => BinaryOperation::Multiply,
                    AssignOperator::Divide => BinaryOperation::Divide,
                    AssignOperator::Modulo => BinaryOperation::Modulo,
                    AssignOperator::Exponentiate => BinaryOperation::Exponentiate,
                    AssignOperator::ShiftLeft => BinaryOperation::LeftShift,
                    AssignOperator::ShiftRight => BinaryOperation::RightShift,
                    AssignOperator::ShiftRightUnsigned => BinaryOperation::UnsignedRightShift,
                    AssignOperator::BitwiseAnd => BinaryOperation::BitwiseAnd,
                    AssignOperator::BitwiseXor => BinaryOperation::BitwiseXor,
                    AssignOperator::BitwiseOr => BinaryOperation::BitwiseOr,
                };
                let value = self.ssa.push(Ssa::Binary {
                    op,
                    left: place_use,
                    right,
                });
                self.compile_assignment(place, value);
                CompiledExpr::new_in(value, self.alloc)
            }
        }
    }

    pub(crate) fn compile_place_use(&mut self, place: Place) -> SsaId {
        match place {
            Place::Variable(x) => {
                let variable = &self.variables[x];
                match variable.kind {
                    VariableKind::Global => {
                        if variable.scope == self.variables.root() {
                            let object = self.ssa.global();
                            let constant = self.module.constants.add_string(variable.name);
                            let key = self.ssa.insert(Ssa::LoadString { constant });
                            self.ssa.insert(Ssa::Index { object, key })
                        } else {
                            todo!()
                        }
                    }
                    VariableKind::Local(slot)
                    | VariableKind::Captured(slot)
                    | VariableKind::LocalConstant(slot)
                    | VariableKind::CapturedConstant(slot) => {
                        let depth = self.scope.as_ref().unwrap().ty.as_function().stack_depth
                            - self.variables[x].define_depth;
                        let env = self.ssa.environment(depth);
                        self.ssa.insert(Ssa::IndexEnvironment { env, slot })
                    }
                    _ => todo!(),
                }
            }
            Place::Object { object, key } => self.ssa.insert(Ssa::Index { object, key }),
        }
    }

    pub(crate) fn compile_place(&mut self, expr: &Expr<A>) -> Place {
        match *expr {
            Expr::Prime(ref x) => match *x {
                PrimeExpr::Variable(x) => Place::Variable(x),
                _ => panic!("not assignable"),
            },
            Expr::UnaryPostfix(ref expr, ref op) => match *op {
                PostfixOperator::Index(ref x) => {
                    let object = self.compile_expr(expr).id;
                    let key = self.compile_expr(x).id;
                    Place::Object { object, key }
                }
                PostfixOperator::Dot(x) => {
                    let object = self.compile_expr(expr).id;
                    let constant = self.module.constants.add_string(x);
                    let key = self.ssa.insert(Ssa::LoadString { constant });
                    Place::Object { object, key }
                }
                _ => panic!("not assignable"),
            },
            _ => panic!("not assignable"),
        }
    }

    pub(crate) fn compile_assignment(&mut self, place: Place, value: SsaId) {
        match place {
            Place::Variable(x) => {
                let variable = &self.variables[x];
                match variable.kind {
                    VariableKind::Global => {
                        if variable.scope == self.variables.root() {
                            let object = self.ssa.global();
                            let constant = self.module.constants.add_string(variable.name);
                            let key = self.ssa.insert(Ssa::LoadString { constant });
                            self.ssa.insert(Ssa::Assign { object, key, value })
                        } else {
                            todo!()
                        }
                    }
                    VariableKind::Local(slot)
                    | VariableKind::Captured(slot)
                    | VariableKind::LocalConstant(slot)
                    | VariableKind::CapturedConstant(slot) => {
                        let depth = self.scope.as_ref().unwrap().ty.as_function().stack_depth
                            - self.variables[x].define_depth;
                        let env = self.ssa.environment(depth);
                        self.ssa.insert(Ssa::AssignEnvironment { value, env, slot })
                    }
                    _ => todo!(),
                }
            }
            Place::Object { object, key } => self.ssa.insert(Ssa::Assign { object, key, value }),
        };
    }

    pub(crate) fn compile_prime(&mut self, prime: &PrimeExpr<A>) -> SsaId {
        match *prime {
            PrimeExpr::Literal(x) => {
                let constant = match x {
                    Literal::Float(x) => Constant::Float(x),
                    Literal::Boolean(x) => Constant::Boolean(x),
                    Literal::Integer(x) => Constant::Integer(x),
                    Literal::String(x) => {
                        let constant = self.module.constants.add_string(x);
                        return self.ssa.insert(Ssa::LoadString { constant });
                    }
                };
                let constant = self.module.constants.add(constant);
                return self.ssa.insert(Ssa::LoadConstant { constant });
            }
            PrimeExpr::Covered(ref exprs) => {
                let mut res = None;
                for expr in exprs.iter() {
                    res = Some(self.compile_expr(expr));
                }
                res.unwrap().eval(self)
            }
            PrimeExpr::Object(ref entries) => self.compile_object(entries),
            PrimeExpr::Variable(x) => {
                let var = &self.variables[x];
                match var.kind {
                    VariableKind::Global => {
                        if var.scope == self.variables.root() {
                            let constant = self.module.constants.add_string(var.name);
                            let key = self.ssa.insert(Ssa::LoadString { constant });
                            self.ssa.insert(Ssa::Index {
                                object: self.ssa.global(),
                                key,
                            })
                        } else {
                            todo!()
                        }
                    }
                    VariableKind::Local(slot)
                    | VariableKind::Captured(slot)
                    | VariableKind::LocalConstant(slot)
                    | VariableKind::CapturedConstant(slot) => {
                        let depth = self.scope.as_ref().unwrap().ty.as_function().stack_depth
                            - self.variables[x].define_depth;
                        let env = self.ssa.environment(depth);
                        self.ssa.insert(Ssa::IndexEnvironment { env, slot })
                    }
                    _ => todo!(),
                }
            }
        }
    }

    pub(crate) fn compile_object(&mut self, entries: &Vec<(StringId, Expr<A>), A>) -> SsaId {
        let res = self.ssa.insert(Ssa::CreateObject);
        for (k, v) in entries.iter() {
            let key_const = self.module.constants.add_string(*k);
            let value = self.compile_expr(v).eval(self);
            let key = self.ssa.insert(Ssa::LoadString {
                constant: key_const,
            });
            self.ssa.insert(Ssa::Assign {
                object: res,
                key,
                value,
            });
        }
        res
    }
}
