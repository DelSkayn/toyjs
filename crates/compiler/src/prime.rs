use ast::{ArrayLiteralEntry, ListHead, NodeId, ObjectLiteral};
use bc::{FunctionId, Instruction, Primitive, Reg};
use common::{
    number::{Number, NumberId},
    span::Span,
    string::StringId,
};

use crate::{
    expr::{ExprPosition, ExprResult},
    Compiler, Error, InstrOffset, Limits, Result,
};

impl<'a> Compiler<'a> {
    pub fn compile_prime(&mut self, expr: NodeId<ast::PrimeExpr>) -> Result<ExprResult> {
        let dst = Reg::this_reg();
        match self.ast[expr] {
            ast::PrimeExpr::Number(id) => self.compile_number(id).map(|x| x.into()),
            ast::PrimeExpr::String(x) => Ok(self.compile_string(x).map(ExprResult::from)?),
            ast::PrimeExpr::Template(_) => to_do!(),
            ast::PrimeExpr::Regex(_) => to_do!(),
            ast::PrimeExpr::Ident(sym) => self.load_symbol(self.variables.symbol_of_ast(sym)),
            ast::PrimeExpr::Boolean(x) => {
                let instr = if x {
                    self.emit(Instruction::LoadPrim {
                        dst,
                        imm: Primitive::t(),
                    })?
                } else {
                    self.emit(Instruction::LoadPrim {
                        dst,
                        imm: Primitive::f(),
                    })?
                };
                Ok(instr.into())
            }
            ast::PrimeExpr::Function(func) => self.compile_function_load(func).map(|x| x.into()),
            ast::PrimeExpr::Class(_) => to_do!(),
            ast::PrimeExpr::Object(obj) => self.compile_object_literal(obj).map(|x| x.into()),
            ast::PrimeExpr::Array(array) => self.compile_array_literal(array),
            ast::PrimeExpr::NewTarget | ast::PrimeExpr::This => Ok(Reg::this_reg().into()),
            ast::PrimeExpr::Null => self
                .emit(Instruction::LoadPrim {
                    dst,
                    imm: Primitive::null(),
                })
                .map(Into::into),
            ast::PrimeExpr::Super => to_do!(),
            ast::PrimeExpr::Covered(x) => self.compile_exprs(x),
        }
    }

    pub fn compile_function_load(&mut self, func: NodeId<ast::Function>) -> Result<ExprPosition> {
        let id = self.functions.len();
        self.functions.push(bc::Function {
            offset: 0,
            len: 0,
            reflect_info: None,
            span: Span::empty(),
            registers: 0,
            upvalues: 0,
        });
        self.pending_functions.push(func);
        let cons = FunctionId(id.try_into().map_err(|_| Error::Limit(Limits::Functions))?);
        let instr = self.emit(Instruction::LoadFunction {
            dst: Reg::this_reg(),
            cons,
        })?;
        Ok(ExprPosition::InstrDst(instr))
    }

    pub fn compile_number(&mut self, id: NumberId) -> Result<ExprPosition> {
        let number = self.interners.numbers[id];
        self.compile_number_value(number)
    }

    pub fn compile_number_value(&mut self, number: Number) -> Result<ExprPosition> {
        let dst = Reg::this_reg();
        let instr = if let Some(imm) = number.cast() {
            self.emit(Instruction::Loadi8 { dst, imm })?
        } else if let Some(imm) = number.cast() {
            self.emit(Instruction::Loadi16 { dst, imm })?
        } else if let Some(imm) = number.cast() {
            self.emit(Instruction::Loadi32 { dst, imm })?
        } else if let Some(imm) = number.cast() {
            self.emit(Instruction::Loadf32 { dst, imm })?
        } else {
            self.emit(Instruction::Loadf64 { dst, imm: number.0 })?
        };
        Ok(ExprPosition::InstrDst(instr))
    }

    pub fn compile_object_literal(&mut self, object: ObjectLiteral) -> Result<ExprPosition> {
        let instr = self.emit(Instruction::NewObject {
            dst: Reg::this_reg(),
        })?;

        let ObjectLiteral::Item(mut cur) = object else {
            return Ok(ExprPosition::InstrDst(instr));
        };

        let tmp = self.alloc_tmp_register()?;
        loop {
            let item = self.ast[cur].item;
            self.compile_object_prop_definition(tmp, item)?;
            let Some(next) = self.ast[cur].next else {
                break;
            };
            cur = next;
        }

        self.patch_dst(instr, tmp);
        Ok(ExprPosition::Register(tmp))
    }

    pub fn compile_array_literal(
        &mut self,
        array: ListHead<ArrayLiteralEntry>,
    ) -> Result<ExprResult> {
        let instr = self.emit(Instruction::NewArray {
            dst: Reg::this_reg(),
        })?;

        let ListHead::Present(mut entry) = array else {
            return Ok(ExprPosition::InstrDst(instr).into());
        };
        let tmp = self.alloc_tmp_register()?;
        self.patch_dst(instr, tmp);

        let mut i = 0;
        loop {
            let item = self.ast[entry].item;
            if let Some(expr) = self.ast[item].expr {
                if self.ast[item].is_spread {
                    to_do!()
                } else {
                    let expr = self.compile_expr(expr)?.into_register(self)?;
                    let key = self.compile_number_value(Number(i.into()))?;
                    let key = ExprResult::from(key).into_register(self)?;
                    self.free_tmp_register(key);
                    self.free_tmp_register(expr);
                    self.emit(Instruction::IndexStore {
                        obj: tmp,
                        key,
                        src: expr,
                    })?;
                }
            }
            let Some(next) = self.ast[entry].next else {
                break;
            };
            entry = next;
            i += 1;
        }

        Ok(tmp.into())
    }

    pub fn compile_object_prop_definition(
        &mut self,
        obj: Reg,
        prop: NodeId<ast::PropertyDefinition>,
    ) -> Result<()> {
        match self.ast[prop] {
            ast::PropertyDefinition::Ident { ident } => {
                let sym = self.variables.symbol_of_ast(ident);
                let src = self.load_symbol(sym)?.into_register(self)?;
                let str_instr = self.compile_string(self.ast[ident].name)?;
                let key = self.alloc_tmp_register()?;
                self.free_tmp_register(src);
                self.patch_dst(str_instr, key);
                self.emit(Instruction::IndexStore { obj, key, src })?;
                Ok(())
            }
            ast::PropertyDefinition::Define { property, expr } => {
                let src = self.compile_expr(expr)?.into_register(self)?;
                match property {
                    ast::PropertyName::Ident(x) | ast::PropertyName::String(x) => {
                        let str = self.map_string(x);
                        let key = self.alloc_tmp_register()?;
                        self.free_tmp_register(src);
                        self.emit(Instruction::LoadString {
                            dst: key,
                            cons: bc::StringId(
                                str.try_into().map_err(|_| Error::Limit(Limits::Strings))?,
                            ),
                        })?;
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Number(num) => {
                        let key = ExprResult::new(self.compile_number(num)?).into_register(self)?;
                        self.free_tmp_register(key);
                        self.free_tmp_register(src);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Computed(c) => {
                        let key = self.compile_expr(c)?.into_register(self)?;
                        self.free_tmp_register(key);
                        self.free_tmp_register(src);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                }
            }
            ast::PropertyDefinition::Method { property, func } => {
                let src = ExprResult::new(self.compile_function_load(func)?).into_register(self)?;

                match property {
                    ast::PropertyName::Ident(x) | ast::PropertyName::String(x) => {
                        let str_instr = self.compile_string(x)?;
                        let key = self.next_free_register()?;
                        self.free_tmp_register(src);
                        self.patch_dst(str_instr, key);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Number(num) => {
                        let key = ExprResult::new(self.compile_number(num)?).into_register(self)?;
                        self.free_tmp_register(key);
                        self.free_tmp_register(src);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Computed(c) => {
                        let key = self.compile_expr(c)?.into_register(self)?;
                        self.free_tmp_register(key);
                        self.free_tmp_register(src);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
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

    pub fn compile_string(&mut self, s: StringId) -> Result<InstrOffset> {
        let str = self.map_string(s);
        self.emit(Instruction::LoadString {
            dst: Reg::tmp(),
            cons: bc::StringId(str.try_into().map_err(|_| Error::Limit(Limits::Strings))?),
        })
    }
}
