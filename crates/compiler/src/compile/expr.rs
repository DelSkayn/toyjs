use crate::Compiler;
use crate::{constants::Constant, ssa::*};
use ast::*;
use bumpalo::{collections::Vec, Bump};
use common::interner::StringId;
use std::ptr;

#[derive(Copy, Clone, Debug)]
pub enum Place {
    Object { object: SsaId, key: SsaId },
    Variable(VariableId),
}

impl<'a, 'alloc> Compiler<'a, 'alloc> {
    pub(crate) fn compile_expr(&mut self, expr: &Expr<'alloc>) -> SsaId {
        match expr {
            Expr::UnaryPostfix(expr, op) => match *op {
                PostfixOperator::Dot(x) => {
                    let expr = self.compile_expr(expr);
                    let constant = self.constants.add(Constant::String(x));
                    let constant = self.ssa.insert(Ssa::LoadConstant { constant });
                    self.ssa.insert(Ssa::Index {
                        object: expr,
                        key: constant,
                    })
                }
                PostfixOperator::Index(ref key) => {
                    let key = self.compile_expr(key);
                    let object = self.compile_expr(expr);
                    self.ssa.insert(Ssa::Index { object, key })
                }
                ref x @ PostfixOperator::AddOne | ref x @ PostfixOperator::SubtractOne => {
                    let place = self.compile_place(expr);
                    let value = self.compile_place_use(place);
                    let value = self.ssa.push(Ssa::Unary {
                        op: UnaryOperation::ToNumber,
                        operand: value,
                    });
                    let constant = self.constants.add(Constant::Integer(1));
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
                    value
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
                    BinaryOperator::Ternary(ref middle) => {
                        let left = self.compile_expr(left);
                        let jump_before = self.ssa.insert(Ssa::ConditionalJump {
                            condition: left,
                            to: None,
                        });
                        let right = self.compile_expr(right);
                        let jump_after = self.ssa.insert(Ssa::Jump { to: None });
                        self.ssa.patch_jump(jump_before, jump_after.next());
                        let middle = self.compile_expr(middle);
                        self.ssa.patch_jump(jump_after, middle.next());
                        return self.ssa.push(Ssa::Alias {
                            left: right,
                            right: middle,
                        });
                    }
                    _ => todo!(),
                };
                let left = self.compile_expr(left);
                let right = self.compile_expr(right);
                self.ssa.insert(Ssa::Binary { op, left, right })
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
                        let constant = self.constants.add(Constant::Integer(1));
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
                        return added;
                    }
                    _ => todo!(),
                };
                let right = self.compile_expr(right);
                self.ssa.insert(Ssa::Unary { op, operand: right })
            }
            Expr::Prime(x) => self.compile_prime(x),
            Expr::Assign(left, AssignOperator::Assign, right) => {
                let right = self.compile_expr(right);
                let place = self.compile_place(left);
                self.compile_assignment(place, right);
                right
            }
            Expr::Assign(left, op, right) => {
                let right = self.compile_expr(right);
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
                value
            }
        }
    }

    pub(crate) fn compile_place_use(&mut self, place: Place) -> SsaId {
        match place {
            Place::Variable(x) => {
                let variable = &self.variables[x];
                match variable.kind {
                    VariableKind::Global => {
                        if ptr::eq(variable.scope, self.variables.root()) {
                            let object = self.ssa.global();
                            let constant = self.constants.add(Constant::String(variable.name));
                            let key = self.ssa.insert(Ssa::LoadConstant { constant });
                            self.ssa.insert(Ssa::Index { object, key })
                        } else {
                            todo!()
                        }
                    }
                    _ => {
                        let slot = self.variables[x].slot.unwrap();
                        let depth = self.scope.as_ref().unwrap().stack_depth
                            - self.variables[x].define_depth;
                        let env = self.ssa.environment(depth);
                        self.ssa.insert(Ssa::IndexEnvironment { env, slot })
                    }
                }
            }
            Place::Object { object, key } => self.ssa.insert(Ssa::Index { object, key }),
        }
    }

    pub(crate) fn compile_place(&mut self, expr: &Expr<'alloc>) -> Place {
        match *expr {
            Expr::Prime(ref x) => match *x {
                PrimeExpr::Variable(x) => Place::Variable(x),
                _ => panic!("not assignable"),
            },
            Expr::UnaryPostfix(ref expr, ref op) => match *op {
                PostfixOperator::Index(ref x) => {
                    let object = self.compile_expr(expr);
                    let key = self.compile_expr(x);
                    Place::Object { object, key }
                }
                PostfixOperator::Dot(x) => {
                    let object = self.compile_expr(expr);
                    let constant = self.constants.add(Constant::String(x));
                    let key = self.ssa.insert(Ssa::LoadConstant { constant });
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
                        if ptr::eq(variable.scope, self.variables.root()) {
                            let object = self.ssa.global();
                            let constant = self.constants.add(Constant::String(variable.name));
                            let key = self.ssa.insert(Ssa::LoadConstant { constant });
                            self.ssa.insert(Ssa::Assign { object, key, value })
                        } else {
                            todo!()
                        }
                    }
                    _ => {
                        let slot = self.variables[x].slot.unwrap();
                        let depth = self.scope.as_ref().unwrap().stack_depth
                            - self.variables[x].define_depth;
                        let env = self.ssa.environment(depth);
                        self.ssa.insert(Ssa::AssignEnvironment { value, env, slot })
                    }
                }
            }
            Place::Object { object, key } => self.ssa.insert(Ssa::Assign { object, key, value }),
        };
    }

    pub(crate) fn compile_prime(&mut self, prime: &PrimeExpr<'alloc>) -> SsaId {
        match *prime {
            PrimeExpr::Literal(x) => {
                let constant = self.constants.add(Constant::from_literal(x));
                self.ssa.insert(Ssa::LoadConstant { constant })
            }
            PrimeExpr::Covered(ref exprs) => {
                let mut res = None;
                for expr in exprs.iter() {
                    res = Some(self.compile_expr(expr));
                }
                res.unwrap()
            }
            PrimeExpr::Object(ref entries) => self.compile_object(entries),
            PrimeExpr::Variable(x) => {
                let var = &self.variables[x];
                match var.kind {
                    VariableKind::Global => {
                        if ptr::eq(var.scope, self.variables.root()) {
                            let constant = self.constants.add(Constant::String(var.name));
                            let key = self.ssa.insert(Ssa::LoadConstant { constant });
                            self.ssa.insert(Ssa::Index {
                                object: self.ssa.global(),
                                key,
                            })
                        } else {
                            todo!()
                        }
                    }
                    _ => {
                        let slot = self.variables[x].slot.unwrap();
                        let depth = self.scope.as_ref().unwrap().stack_depth
                            - self.variables[x].define_depth;
                        let env = self.ssa.environment(depth);
                        self.ssa.insert(Ssa::IndexEnvironment { env, slot })
                    }
                }
            }
        }
    }

    pub(crate) fn compile_object(
        &mut self,
        entries: &Vec<'alloc, (StringId, Expr<'alloc>)>,
    ) -> SsaId {
        let res = self.ssa.insert(Ssa::CreateObject);
        for (k, v) in entries.iter() {
            let key_const = self.constants.add(Constant::String(*k));
            let value = self.compile_expr(v);
            let key = self.ssa.insert(Ssa::LoadConstant {
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
