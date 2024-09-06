use ast::{ArrayLiteralEntry, NodeId, NodeListId, ObjectLiteral};
use bc::{FunctionId, Instruction, Primitive, Reg};
use common::{number::Number, span::Span, string::String};

use crate::{
    expr::{ExprPosition, ExprResult},
    Compiler, Error, InstrOffset, Limits, Result,
};

impl<'a> Compiler<'a> {
    pub fn compile_prime(&mut self, expr: NodeId<ast::PrimeExpr>) -> Result<ExprResult> {
        let dst = Reg::this_reg();
        match self.ast[expr] {
            ast::PrimeExpr::Number { value } => self.compile_number(value).map(|x| x.into()),
            ast::PrimeExpr::String { value } => {
                Ok(self.compile_string(value).map(ExprResult::from)?)
            }
            ast::PrimeExpr::Template { .. } => to_do!(),
            ast::PrimeExpr::Regex { .. } => to_do!(),
            ast::PrimeExpr::Ident { symbol } => {
                self.load_symbol(self.variables.symbol_of_ast(symbol))
            }
            ast::PrimeExpr::Boolean { value } => {
                let instr = if value {
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
            ast::PrimeExpr::Function { function } => {
                self.compile_function_load(function).map(|x| x.into())
            }
            ast::PrimeExpr::Class { .. } => to_do!(),
            ast::PrimeExpr::Object { object } => {
                self.compile_object_literal(object).map(|x| x.into())
            }
            ast::PrimeExpr::Array { array } => self.compile_array_literal(array),
            ast::PrimeExpr::NewTarget | ast::PrimeExpr::This => Ok(Reg::this_reg().into()),
            ast::PrimeExpr::Null => self
                .emit(Instruction::LoadPrim {
                    dst,
                    imm: Primitive::null(),
                })
                .map(Into::into),
            ast::PrimeExpr::Super => to_do!(),
            ast::PrimeExpr::Covered { expr } => self.compile_exprs(expr),
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

    pub fn compile_number(&mut self, id: NodeId<Number>) -> Result<ExprPosition> {
        let number = self.ast[id];
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

        let ObjectLiteral::Item { mut definition } = object else {
            return Ok(ExprPosition::InstrDst(instr));
        };

        let tmp = self.alloc_tmp_register()?;
        loop {
            let item = self.ast[definition].item;
            self.compile_object_prop_definition(tmp, item)?;
            let Some(next) = self.ast[definition].next else {
                break;
            };
            definition = next;
        }

        self.patch_dst(instr, tmp);
        Ok(ExprPosition::Register(tmp))
    }

    pub fn compile_array_literal(
        &mut self,
        array: Option<NodeListId<ArrayLiteralEntry>>,
    ) -> Result<ExprResult> {
        let instr = self.emit(Instruction::NewArray {
            dst: Reg::this_reg(),
        })?;

        let Some(mut entry) = array else {
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
                    ast::PropertyName::Ident { name }
                    | ast::PropertyName::String { value: name } => {
                        let str = self.map_string(name);
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
                    ast::PropertyName::Number { value } => {
                        let key =
                            ExprResult::new(self.compile_number(value)?).into_register(self)?;
                        self.free_tmp_register(key);
                        self.free_tmp_register(src);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Computed { expr } => {
                        let key = self.compile_expr(expr)?.into_register(self)?;
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
                    ast::PropertyName::Ident { name }
                    | ast::PropertyName::String { value: name } => {
                        let str_instr = self.compile_string(name)?;
                        let key = self.next_free_register()?;
                        self.free_tmp_register(src);
                        self.patch_dst(str_instr, key);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Number { value } => {
                        let key =
                            ExprResult::new(self.compile_number(value)?).into_register(self)?;
                        self.free_tmp_register(key);
                        self.free_tmp_register(src);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                    ast::PropertyName::Computed { expr } => {
                        let key = self.compile_expr(expr)?.into_register(self)?;
                        self.free_tmp_register(key);
                        self.free_tmp_register(src);
                        self.emit(Instruction::IndexStore { obj, key, src })?;
                        Ok(())
                    }
                }
            }
            ast::PropertyDefinition::Getter { property, func } => to_do!(),
            ast::PropertyDefinition::Setter { property, func } => to_do!(),
            ast::PropertyDefinition::Rest { .. } => to_do!(),
            // should already be removed by the compiler
            ast::PropertyDefinition::Covered { .. } => unreachable!(),
        }
    }

    pub fn compile_string(&mut self, s: NodeId<String>) -> Result<InstrOffset> {
        let str = self.map_string(s);
        self.emit(Instruction::LoadString {
            dst: Reg::tmp(),
            cons: bc::StringId(str.try_into().map_err(|_| Error::Limit(Limits::Strings))?),
        })
    }
}
