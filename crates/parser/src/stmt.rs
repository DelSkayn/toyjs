use ast::{
    Ast, CaseItem, CatchStmt, CstyleDecl, Expr, ForLoopHead, FunctionKind, IdentOrPattern,
    InOfDecl, List, NodeId, NodeListId, PrimeExpr, Stmt, VariableDecl, VariableKind,
};
use common::span::Span;
use token::t;

use crate::{
    alter_state, error::ErrorKind, expect, function::FunctionCtx, peek_expect, unexpected, Error,
    Parse, Parser, ParserState, Result,
};

impl Parse for Stmt {
    fn parse(parser: &mut Parser, ast: &mut ast::Ast) -> Result<NodeId<Self>> {
        let token = parser.peek();
        let expr = match token.kind() {
            t!("{") => {
                parser.next();
                let list = parser.parse_block_stmt()?;
                parser.ast.push_node(Stmt::Block { list })
            }
            t!("var") => {
                parser.next();
                parser.parse_variable_decl(VariableKind::Var)?
            }
            t!("let") => {
                parser.next();
                parser.parse_variable_decl(VariableKind::Let)?
            }
            t!("const") => {
                parser.next();
                parser.parse_variable_decl(VariableKind::Const)?
            }
            t!("if") => {
                parser.next();
                parser.parse_if_stmt()?
            }
            t!("while") => {
                parser.next();
                parser.parse_while_stmt()?
            }
            t!("do") => {
                parser.next();
                parser.parse_do_while_stmt()?
            }
            t!("for") => {
                parser.next();
                parser.parse_for_stmt()?
            }
            t!("switch") => {
                parser.next();
                parser.parse_switch_stmt()?
            }
            t!("return") => {
                parser.next();
                parser.parse_return_stmt()?
            }
            t!("break") => {
                parser.next();
                parser.parse_cntrl_flow_stmt(true)?
            }
            t!("continue") => {
                parser.next();
                parser.parse_cntrl_flow_stmt(false)?
            }
            t!("try") => {
                parser.next();
                parser.parse_try_stmt()?
            }
            t!("throw") => {
                parser.next();
                parser.parse_throw_stmt()?
            }
            t!("class") => {
                parser.next();
                let class = parser.parse_class(true)?;
                Ok(ast.push(Stmt::Class { class })?)
            }
            t!("function") => {
                parser.next();
                let kind = if parser.eat(t!("*")) {
                    FunctionKind::Generator
                } else {
                    FunctionKind::Simple
                };
                let func = parser.parse_function(FunctionCtx::Stmt, kind)?;
                ast.push(Stmt::Function { func }).into()
            }
            t!("async") => {
                parser.next();
                expect!(parser, "function");
                let kind = if parser.eat(t!("*")) {
                    FunctionKind::AsyncGenerator
                } else {
                    FunctionKind::Async
                };
                parser.no_line_terminator()?;
                let func = parser.parse_function(FunctionCtx::Stmt, kind)?;
                ast.push(Stmt::Function { func }).into()
            }
            t!("debugger") => {
                parser.next();
                parser.semicolon()?;
                parser.ast.push_node(Stmt::Debugger)
            }
            t!("with") => {
                parser.next();
                parser.parse_with_stmt()?
            }
            t!(";") => {
                parser.next();
                parser.ast.push_node(Stmt::Empty)
            }
            _ => {
                alter_state!(parser => {
                    parser.state.insert(ParserState::In);
                    let expr = parser.parse_expr()?;
                });
                if parser.eat(t!(":")) {
                    if parser.ast[expr].next.is_some() {
                        unexpected!(parser, t!(":"), ";");
                    }
                    let prime = parser.ast[expr].item;
                    let Expr::Prime { expr } = parser.ast[prime] else {
                        unexpected!(parser, t!(":"), ";");
                    };
                    let PrimeExpr::Ident(label) = parser.ast[expr] else {
                        unexpected!(parser, t!(":"), ";");
                    };
                    let name = parser.ast[label].name;
                    let stmt = parser.parse_stmt()?;
                    parser.ast.push_node(Stmt::Labeled { label: name, stmt })
                } else {
                    parser.semicolon()?;
                    parser.ast.push_node(Stmt::Expr { expr })
                }
            }
        };

        Ok(expr)
    }
}

/// Parser a block statement:
/// ```javascript
/// // starts here {
///     var a = 1;
/// }
/// ```
pub fn parse_block_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<Option<NodeListId<Stmt>>> {
    let mut head = None;
    let mut cur = None;

    loop {
        if let t!("}") = parser.peek_kind() {
            parser.next();
            return Ok(head);
        }
        let stmt = parser.parse(ast)?;
        ast.push_list(&mut head, &mut cur, stmt)
    }

    Ok(head)
}

pub fn parse_if_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    expect!(parser, "(");
    let cond = parser.save_state(|parser| {
        parser.state.insert(ParserState::In);
        parser.parse_expr()
    })?;
    expect!(parser, ")");
    let body = parser.parse(ast)?;
    let r#else = if parser.eat(t!("else")) {
        Some(parser.parse(ast)?)
    } else {
        None
    };

    Ok(ast.push(Stmt::If { cond, body, r#else })?)
}

pub fn parse_while_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    expect!(parser, "(");
    let cond = parser.save_state(|parser| {
        parser.state.insert(ParserState::In);
        let cond = parser.parse(ast)?;
    });
    expect!(parser, ")");

    let body = parser.save_state(|parser| {
        parser
            .state
            .insert(ParserState::Break | ParserState::Continue);
        parser.parse(ast)
    })?;

    ast.push(Stmt::While { cond, body }).into()
}

pub fn parse_do_while_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    parser.save_state(|parser| {
        parser
            .state
            .insert(ParserState::Break | ParserState::Continue);
        parser.parse(ast)?;
    })?;

    expect!(parser, "while");
    expect!(parser, "(");
    let cond = parser.parse(ast)?;
    expect!(parser, ")");
    Ok(ast.push(Stmt::DoWhile { cond, body }))
}

/// Parse a c style for loop declaration:
/// ```javascript
/// for(let i /* start here */ = foo, b = bar; i < 10;i++){ body }
/// ```
pub fn parse_c_style_decl(
    parser: &mut Parser,
    ast: &mut Ast,
    kind: VariableKind,
    decl: NodeId<IdentOrPattern>,
    decl_span: Span,
) -> Result<NodeListId<VariableDecl>> {

    let initializer = parser
        .eat(t!("="))
        .then(|| parser.parse(ast))
        .transpose()?;

    if initializer.is_none() {
        if kind == VariableKind::Const {
            return Err(Error::new(ErrorKind::ConstNotInitialized, decl_span));
        }
        if matches!(ast[decl], IdentOrPattern::Pattern(_)) {
            return Err(Error::new(ErrorKind::DestructringNotInitalized, decl_span));
        }
    }

    let decl = ast.push(VariableDecl { decl, initializer })?;
    let mut head = None;
    let mut cur = None;

    ast.push_list(&mut head, &mut cur, None)

    while parser.eat(t!(",")) {
        let start = *parser.last_span();
        let decl = parser.parse_ident_or_pattern()?;
        let decl_span = start.covers(parser.last_span());
        let initializer = parser
            .eat(t!("="))
            .then(|| parser.parse(ast))
            .transpose()?;

        if initializer.is_none() {
            if kind == VariableKind::Const {
                return Err(Error::new(ErrorKind::ConstNotInitialized, decl_span));
            }
            if matches!(ast[decl], IdentOrPattern::Pattern(_)) {
                return Err(Error::new(ErrorKind::DestructringNotInitalized, decl_span));
            }
        }

        let decl = ast.push(VariableDecl { decl, initializer });

        prev = ast.append_list(decl, Some(prev));
    }
    Ok(head)
}

/// Parse a c style for loop:
/// ```javascript
/// for(let i = foo /* start here */; i < 10;i++){ body }
/// ```
pub fn parse_c_style_for(
    parser: &mut Parser,
    ast: &mut Ast,
    decl: CstyleDecl,
) -> Result<NodeId<Stmt>> {
    expect!(parser, ";");
    let cond = if let t!(";") = parser.peek_kind() {
        None
    } else {
        Some(parser.parse_expr()?)
    };
    expect!(parser, ";");
    let post = if let t!(")") = parser.peek_kind() {
        None
    } else {
        Some(parser.parse_expr()?)
    };
    expect!(parser, ")");
    let head = ast.push(ForLoopHead::CStyle { decl, cond, post });
    alter_state!(parser => {
        parser.state.insert(ParserState::Break | ParserState::Continue);
        let body = parser.parse_stmt()?;
    });
    Ok(ast.push(Stmt::For { head, body }))
}

pub fn parse_for_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    //TODO await loop
    expect!(parser, "(");
    let next = parser.peek_kind();
    let decl = match next {
        t!("let") | t!("var") | t!("const") => {
            let kind = match next {
                t!("let") => VariableKind::Let,
                t!("var") => VariableKind::Var,
                t!("const") => VariableKind::Const,
                _ => unreachable!(),
            };
            parser.next();
            parser.peek(); // peek to advance the last span to the next token.
            let start = *parser.last_span();
            let binding = parser.parse_ident_or_pattern()?;
            let binding_span = start.covers(parser.last_span());
            if let t!("=") | t!(",") = parser.peek_kind() {
                let decl = parser.parse_c_style_decl(kind, binding, binding_span)?;
                return parser.parse_c_style_for(CstyleDecl::Decl { kind, decl });
            } else {
                InOfDecl::Decl { kind, binding }
            }
        }
        t!(";") => return parser.parse_c_style_for(CstyleDecl::Empty),
        _ => {
            let state = parser.state;
            parser.state.remove(ParserState::In);
            let expr = parser.parse_assignment_expr()?;

            if parser.eat(t!(",")) {
                let next = Some(parser.parse_expr()?);
                let expr = ast.push_list(List { item: expr, next });
                parser.state = state;
                return parser.parse_c_style_for(CstyleDecl::Expr(expr));
            }
            parser.state = state;
            InOfDecl::Expr(expr)
        }
    };

    let next = peek_expect!(parser);
    let head = match next.kind() {
        t!(";") => {
            let decl = match decl {
                InOfDecl::Expr(expr) => CstyleDecl::Expr(ast.append_list(expr, None)),
                InOfDecl::Decl { kind, binding } => {
                    let decl = ast.push(VariableDecl {
                        decl: binding,
                        initializer: None,
                    });
                    let decl = ast.append_list(decl, None);
                    CstyleDecl::Decl { kind, decl }
                }
            };
            return parser.parse_c_style_for(decl);
        }
        t!("in") => {
            parser.next();
            let expr = parser.parse_expr()?;
            ForLoopHead::In { decl, expr }
        }
        t!("of") => {
            parser.next();
            let expr = parser.parse_assignment_expr()?;
            ForLoopHead::Of { decl, expr }
        }
        //TODO 'of'
        x => unexpected!(parser, x, ";", "in"),
    };
    let head = ast.push(head);

    expect!(parser, ")");
    alter_state!(parser => {
        parser.state.insert(ParserState::Break | ParserState::Continue);
        let body = parser.parse_stmt()?;
    });
    Ok(ast.push(Stmt::For { head, body }))
}

pub fn parse_switch_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    expect!(parser, "(");
    let cond = parser.parse_expr()?;
    expect!(parser, ")");
    expect!(parser, "{");
    let mut head = ListHead::Empty;
    let mut prev = None;
    let mut default = None;
    loop {
        let case = match parser.peek_kind() {
            t!("case") => {
                parser.next();
                Some(parser.parse_expr()?)
            }
            t!("default") => {
                if default.is_some() {
                    unexpected!(parser, t!("default") => "multiple default cases is not allowed")
                }
                parser.next();
                None
            }
            t!("}") => {
                parser.next();
                return Ok(ast.push(Stmt::Switch {
                    cond,
                    cases: head,
                    default,
                }));
            }
            x => unexpected!(parser, x, "case", "default", "}"),
        };
        expect!(parser, ":");

        alter_state!(parser => {
            parser.state.insert(ParserState::Break);
            let mut stmt_head = ListHead::Empty;
            let mut stmt_prev = None;
            loop {
                match peek_expect!(parser).kind() {
                    t!("case") | t!("default") | t!("}") => break,
                    _ => {
                        let stmt = parser.parse_stmt()?;
                        stmt_prev = Some(ast.append_list(stmt, stmt_prev));
                        stmt_head = stmt_head.or(stmt_prev.into())
                    }
                }
            }
        });

        if let Some(expr) = case {
            let node = ast.push(CaseItem {
                expr,
                stmts: stmt_head,
            });
            prev = Some(ast.append_list(node, prev));
            head = head.or(prev.into())
        } else {
            // Default case
            default = Some(stmt_head)
        }
    }
}

pub fn parse_return_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    debug_assert!(parser.peek.is_none());
    if parser.eat_semicolon() {
        Ok(ast.push(Stmt::Return { expr: None }))
    } else {
        let expr = Some(parser.parse_expr()?);
        parser.semicolon()?;
        Ok(ast.push(Stmt::Return { expr }))
    }
}

pub fn parse_cntrl_flow_stmt(
    parser: &mut Parser,
    ast: &mut Ast,
    is_break: bool,
) -> Result<NodeId<Stmt>> {
    if is_break && !parser.state.contains(ParserState::Break)
        || !is_break && !parser.state.contains(ParserState::Continue)
    {
        return Err(crate::Error::new(
            ErrorKind::DisallowedToken {
                found: if is_break {
                    t!("break")
                } else {
                    t!("continue")
                },
                message: None,
            },
            *parser.last_span(),
        ));
    }

    let label = if parser.eat_semicolon() {
        None
    } else {
        let token = peek_expect!(parser);
        if let t!("ident") = token.kind() {
            parser.next();
            Some(token.data_id().unwrap())
        } else {
            unexpected!(parser, token.kind(), "ident");
        }
    };

    let node = if is_break {
        Stmt::Break { label }
    } else {
        Stmt::Continue { label }
    };
    Ok(ast.push(node))
}

pub fn parse_try_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    expect!(parser, "{");
    let block = parser.parse_block_stmt()?;
    let catch = parser
        .eat(t!("catch"))
        .then(|| {
            let binding = parser
                .eat(t!("("))
                .then(|| {
                    let res = parser.parse_ident_or_pattern()?;
                    expect!(parser, ")");
                    Ok(res)
                })
                .transpose()?;

            expect!(parser, "{");
            let block = parser.parse_block_stmt()?;
            Ok(ast.push(CatchStmt { binding, block }))
        })
        .transpose()?;

    let finally = parser
        .eat(t!("finally"))
        .then(|| {
            expect!(parser, "{");
            parser.parse_block_stmt()
        })
        .transpose()?;

    Ok(ast.push(Stmt::Try {
        block,
        catch,
        finally,
    }))
}

pub fn parse_throw_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    peek_expect!(parser);
    parser.no_line_terminator()?;
    let expr = parser.parse_expr()?;
    parser.semicolon()?;
    Ok(ast.push(Stmt::Throw { expr }))
}

pub fn parse_with_stmt(parser: &mut Parser, ast: &mut Ast) -> Result<NodeId<Stmt>> {
    if parser.state.contains(ParserState::Strict) {
        todo!("disallow with in strict mode")
    }
    expect!(parser, "(");
    let expr = parser.parse_expr()?;
    expect!(parser, ")");
    let stmt = parser.parse_stmt()?;
    Ok(ast.push(Stmt::With { expr, stmt }))
}

/// Parse a variable declaration, e.g. any of:
/// ```javascript
/// var a = 1;
/// let [b,,,...rest] = 1;
/// const { c } = foo;
/// ```
pub fn parse_variable_decl(
    parser: &mut Parser,
    ast: &mut Ast,
    kind: VariableKind,
) -> Result<NodeId<Stmt>> {
    let span = parser.peek().span;

    let decl = parser.parse_ident_or_pattern()?;
    let decl_span = span.covers(parser.last_span());

    let initializer = parser
        .eat(t!("="))
        .then(|| parser.parse_assignment_expr())
        .transpose()?;

    if initializer.is_none() {
        if kind == VariableKind::Const {
            return Err(Error::new(ErrorKind::ConstNotInitialized, decl_span));
        }
        if matches!(ast[decl], IdentOrPattern::Pattern(_)) {
            return Err(Error::new(ErrorKind::DestructringNotInitalized, decl_span));
        }
    }

    let decl = ast.push(VariableDecl { decl, initializer });

    let head = ast.append_list(decl, None);
    let mut prev = head;
    while parser.eat(t!(",")) {
        let span = parser.peek().span;
        let decl = parser.parse_ident_or_pattern()?;
        let decl_span = span.covers(parser.last_span());

        let initializer = parser
            .eat(t!("="))
            .then(|| parser.parse_assignment_expr())
            .transpose()?;

        if initializer.is_none() {
            if kind == VariableKind::Const {
                return Err(Error::new(ErrorKind::ConstNotInitialized, decl_span));
            }
            if matches!(ast[decl], IdentOrPattern::Pattern(_)) {
                return Err(Error::new(ErrorKind::DestructringNotInitalized, decl_span));
            }
        }

        let decl = ast.push(VariableDecl { decl, initializer });
        prev = ast.append_list(decl, Some(prev));
    }
    parser.semicolon()?;

    let res = ast.push(Stmt::VariableDecl { kind, decl: head });

    Ok(res)
}
