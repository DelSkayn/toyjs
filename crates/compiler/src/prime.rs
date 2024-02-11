use ast::{NodeId, ObjectLiteral};
use bc::{FunctionId, Instruction, Reg};
use common::number::NumberId;

use crate::{expr::ExprResult, Compiler, Error, Limits, Result};

impl<'a> Compiler<'a> {
    pub fn compile_prime(&mut self, expr: NodeId<ast::PrimeExpr>) -> Result<ExprResult> {
        let dst = Reg::this_reg();
        match self.ast[expr] {
            ast::PrimeExpr::Number(id) => self.compile_number(id),
            ast::PrimeExpr::String(_) => to_do!(),
            ast::PrimeExpr::Template(_) => to_do!(),
            ast::PrimeExpr::Regex(_) => to_do!(),
            ast::PrimeExpr::Ident(sym) => {
                let sym_id = self.variables.symbol_of_ast(sym);
                let use_order = self.variables.use_of_ast(sym);
                let res = if let Some(reg) = self.registers.find_symbol(sym_id) {
                    ExprResult::Register(reg)
                } else {
                    let instr = self.push(Instruction::LoadPrim { dst, imm: 10 })?;
                    ExprResult::InstrDst(instr)
                };
                self.registers.advance_usage(use_order);
                Ok(res)
            }
            ast::PrimeExpr::Boolean(x) => {
                let instr = if x {
                    self.push(Instruction::LoadPrim { dst, imm: 6 })?
                } else {
                    self.push(Instruction::LoadPrim { dst, imm: 7 })?
                };
                Ok(ExprResult::InstrDst(instr))
            }
            ast::PrimeExpr::Function(func) => self.compile_function_load(func),
            ast::PrimeExpr::Class(_) => to_do!(),
            ast::PrimeExpr::Object(obj) => self.compile_object_literal(obj),
            ast::PrimeExpr::Array(_) => to_do!(),
            ast::PrimeExpr::NewTarget | ast::PrimeExpr::This => {
                Ok(ExprResult::Register(Reg::this_reg()))
            }
            ast::PrimeExpr::Null => self
                .push(Instruction::LoadPrim { dst, imm: 2 })
                .map(ExprResult::InstrDst),
            ast::PrimeExpr::Super => to_do!(),
            ast::PrimeExpr::Covered(x) => self.compile_exprs(x),
        }
    }

    pub fn compile_function_load(&mut self, func: NodeId<ast::Function>) -> Result<ExprResult> {
        let id = self.function_id;
        self.functions.push_back(func);
        self.function_id += 1;
        let cons = FunctionId(
            id.try_into()
                .map_err(|_| Error::ExceededLimits(Limits::Functions))?,
        );
        let instr = self.push(Instruction::LoadFunction {
            dst: Reg::this_reg(),
            cons,
        })?;
        Ok(ExprResult::InstrDst(instr))
    }

    pub fn compile_number(&mut self, id: NumberId) -> Result<ExprResult> {
        let dst = Reg::this_reg();
        let number = self.interners.numbers[id];
        let instr = if let Some(imm) = number.cast() {
            self.push(Instruction::Loadi8 { dst, imm })?
        } else if let Some(imm) = number.cast() {
            self.push(Instruction::Loadi16 { dst, imm })?
        } else if let Some(imm) = number.cast() {
            self.push(Instruction::Loadi32 { dst, imm })?
        } else if let Some(imm) = number.cast() {
            self.push(Instruction::Loadf32 { dst, imm })?
        } else {
            self.push(Instruction::Loadf64 { dst, imm: number.0 })?
        };
        Ok(ExprResult::InstrDst(instr))
    }

    pub fn compile_object_literal(&mut self, object: ObjectLiteral) -> Result<ExprResult> {
        let instr = self.push(Instruction::NewObject {
            dst: Reg::this_reg(),
        })?;

        let ObjectLiteral::Item(mut cur) = object else {
            return Ok(ExprResult::InstrDst(instr));
        };

        let tmp = self
            .registers
            .alloc_tmp()
            .ok_or(Error::ExceededLimits(Limits::Registers))?;

        loop {
            let item = self.ast[cur].item;
            self.compile_object_prop_definition(tmp, item)?;
            let Some(next) = self.ast[cur].next else {
                break;
            };
            cur = next;
        }

        self.patch_dst(instr, tmp);
        Ok(ExprResult::Register(tmp))
    }

    pub fn compile_object_prop_definition(
        &mut self,
        obj: Reg,
        prop: NodeId<ast::PropertyDefinition>,
    ) -> Result<()> {
        match self.ast[prop] {
            ast::PropertyDefinition::Ident { ident } => {
                to_do!()
            }
            ast::PropertyDefinition::Define { property, expr } => {
                let src = self.compile_expr(expr)?.to_register(self)?;
                match property {
                    ast::PropertyName::Ident(x) | ast::PropertyName::String(x) => {
                        let str = self.map_string(x);
                        let key = self
                            .registers
                            .next_free()
                            .ok_or(Error::ExceededLimits(Limits::Registers))?;
                        self.registers.free_if_tmp(src);
                        self.push(Instruction::LoadString {
                            dst: key,
                            cons: bc::StringId(
                                str.try_into()
                                    .map_err(|_| Error::ExceededLimits(Limits::Strings))?,
                            ),
                        })?;
                        self.push(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Number(num) => {
                        let key = self.compile_number(num)?.to_register(self)?;
                        self.registers.free_if_tmp(key);
                        self.registers.free_if_tmp(src);
                        self.push(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Computed(c) => {
                        let key = self.compile_expr(c)?.to_register(self)?;
                        self.registers.free_if_tmp(key);
                        self.registers.free_if_tmp(src);
                        self.push(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                }
            }
            ast::PropertyDefinition::Method { property, func } => {
                let src = self.compile_function_load(func)?.to_register(self)?;

                match property {
                    ast::PropertyName::Ident(x) | ast::PropertyName::String(x) => {
                        let str = self.map_string(x);
                        let key = self
                            .registers
                            .next_free()
                            .ok_or(Error::ExceededLimits(Limits::Registers))?;
                        self.registers.free_if_tmp(src);
                        self.push(Instruction::LoadString {
                            dst: key,
                            cons: bc::StringId(
                                str.try_into()
                                    .map_err(|_| Error::ExceededLimits(Limits::Strings))?,
                            ),
                        })?;
                        self.push(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Number(num) => {
                        let key = self.compile_number(num)?.to_register(self)?;
                        self.registers.free_if_tmp(key);
                        self.registers.free_if_tmp(src);
                        self.push(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Computed(c) => {
                        let key = self.compile_expr(c)?.to_register(self)?;
                        self.registers.free_if_tmp(key);
                        self.registers.free_if_tmp(src);
                        self.push(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                }
            }
            ast::PropertyDefinition::Getter { property, func } => to_do!(),
            ast::PropertyDefinition::Setter { property, func } => to_do!(),
            ast::PropertyDefinition::Rest(_) => to_do!(),
            // should already be removed by the compiler
            ast::PropertyDefinition::Covered { .. } => unreachable!(),
        }
    }
}
