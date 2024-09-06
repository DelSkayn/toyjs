use ast::{
    CaseItem, CatchStmt, CstyleDecl, Expr, ForLoopHead, FunctionKind, IdentOrPattern, InOfDecl,
    NodeId, NodeList, NodeListId, PrimeExpr, Stmt, VariableDecl, VariableKind,
};
use common::span::Span;
use token::t;

use crate::{
    alter_state,
    class::parse_class,
    error::ErrorKind,
    expect,
    function::{parse_function, FunctionCtx},
    peek_expect, unexpected, Error, Parse, Parser, ParserState, Result,
};

impl Parse for Stmt {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let token = parser.peek();
        let expr = match token.kind {
            t!("{") => {
                parser.next();
                let list = parse_block_stmt(parser)?;
                parser.push(Stmt::Block { list })
            }
            t!("var") => {
                parser.next();
                parse_variable_decl(parser, VariableKind::Var)?
            }
            t!("let") => {
                parser.next();
                parse_variable_decl(parser, VariableKind::Let)?
            }
            t!("const") => {
                parser.next();
                parse_variable_decl(parser, VariableKind::Const)?
            }
            t!("if") => {
                parser.next();
                parse_if_stmt(parser)?
            }
            t!("while") => {
                parser.next();
                parse_while_stmt(parser)?
            }
            t!("do") => {
                parser.next();
                parse_do_while_stmt(parser)?
            }
            t!("for") => {
                parser.next();
                parse_for_stmt(parser)?
            }
            t!("switch") => {
                parser.next();
                parse_switch_stmt(parser)?
            }
            t!("return") => {
                parser.next();
                parse_return_stmt(parser)?
            }
            t!("break") => {
                parser.next();
                parse_cntrl_flow_stmt(parser, true)?
            }
            t!("continue") => {
                parser.next();
                parse_cntrl_flow_stmt(parser, false)?
            }
            t!("try") => {
                parser.next();
                parse_try_stmt(parser)?
            }
            t!("throw") => {
                parser.next();
                parse_throw_stmt(parser)?
            }
            t!("class") => {
                let class = parse_class(parser, true)?;
                parser.push(Stmt::Class { class })
            }
            t!("function") => {
                parser.next();
                let kind = if parser.eat(t!("*")) {
                    FunctionKind::Generator
                } else {
                    FunctionKind::Simple
                };
                let func = parse_function(parser, FunctionCtx::Stmt, kind)?;
                parser.push(Stmt::Function { func })
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
                let func = parse_function(parser, FunctionCtx::Stmt, kind)?;
                parser.push(Stmt::Function { func })
            }
            t!("debugger") => {
                parser.next();
                parser.semicolon()?;
                parser.push(Stmt::Debugger)
            }
            t!("with") => {
                parser.next();
                parse_with_stmt(parser)?
            }
            t!(";") => {
                parser.next();
                parser.push(Stmt::Empty)
            }
            _ => {
                let expr = parser.save_state(|parser| {
                    parser.state.insert(ParserState::In);
                    parser.parse::<NodeList<Expr>>()
                })?;
                if parser.eat(t!(":")) {
                    if parser[expr].next.is_some() {
                        unexpected!(parser, t!(":"), ";");
                    }
                    let prime = parser[expr].item;
                    let Expr::Prime { expr } = parser[prime] else {
                        unexpected!(parser, t!(":"), ";");
                    };
                    let PrimeExpr::Ident { symbol } = parser[expr] else {
                        unexpected!(parser, t!(":"), ";");
                    };
                    let name = parser[symbol].name;
                    let stmt = parser.parse()?;
                    parser.push(Stmt::Labeled {
                        label: Some(name),
                        stmt,
                    })
                } else {
                    parser.semicolon()?;
                    parser.push(Stmt::Expr { expr })
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
pub fn parse_block_stmt(parser: &mut Parser) -> Result<Option<NodeListId<Stmt>>> {
    let mut head = None;
    let mut cur = None;

    loop {
        if let t!("}") = parser.peek_kind() {
            parser.next();
            return Ok(head);
        }
        let stmt = parser.parse()?;
        parser.push_list(&mut head, &mut cur, stmt);
    }
}

pub fn parse_if_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
    expect!(parser, "(");
    let cond = parser.save_state(|parser| {
        parser.state.insert(ParserState::In);
        parser.parse()
    })?;
    expect!(parser, ")");
    let body = parser.parse()?;
    let r#else = if parser.eat(t!("else")) {
        Some(parser.parse()?)
    } else {
        None
    };

    Ok(parser.push(Stmt::If { cond, body, r#else }))
}

pub fn parse_while_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
    expect!(parser, "(");
    let cond = parser.save_state(|parser| {
        parser.state.insert(ParserState::In);
        parser.parse()
    })?;
    expect!(parser, ")");

    let body = parser.save_state(|parser| {
        parser
            .state
            .insert(ParserState::Break | ParserState::Continue);
        parser.parse()
    })?;

    Ok(parser.push(Stmt::While { cond, body }))
}

pub fn parse_do_while_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
    let body = parser.save_state(|parser| {
        parser
            .state
            .insert(ParserState::Break | ParserState::Continue);
        parser.parse()
    })?;

    expect!(parser, "while");
    expect!(parser, "(");
    let cond = parser.parse()?;
    expect!(parser, ")");
    Ok(parser.push(Stmt::DoWhile { cond, body }))
}

/// Parse a c style for loop declaration:
/// ```javascript
/// for(let i /* start here */ = foo, b = bar; i < 10;i++){ body }
/// ```
pub fn parse_c_style_decl(
    parser: &mut Parser,
    kind: VariableKind,
    decl: NodeId<IdentOrPattern>,
    decl_span: Span,
) -> Result<NodeListId<VariableDecl>> {
    let initializer = parser.eat(t!("=")).then(|| parser.parse()).transpose()?;

    if initializer.is_none() {
        if kind == VariableKind::Const {
            return Err(Error::new(ErrorKind::ConstNotInitialized, decl_span));
        }
        if matches!(parser[decl], IdentOrPattern::Pattern { .. }) {
            return Err(Error::new(ErrorKind::DestructringNotInitalized, decl_span));
        }
    }

    let decl = parser.push(VariableDecl { decl, initializer });
    let head = parser.push(NodeList {
        item: decl,
        next: None,
    });
    let mut cur = head;

    while parser.eat(t!(",")) {
        let start = *parser.last_span();
        let decl = parser.parse::<IdentOrPattern>()?;
        let decl_span = start.covers(parser.last_span());
        let initializer = parser.eat(t!("=")).then(|| parser.parse()).transpose()?;

        if initializer.is_none() {
            if kind == VariableKind::Const {
                return Err(Error::new(ErrorKind::ConstNotInitialized, decl_span));
            }
            if matches!(parser[decl], IdentOrPattern::Pattern { .. }) {
                return Err(Error::new(ErrorKind::DestructringNotInitalized, decl_span));
            }
        }

        let decl = parser.push(VariableDecl { decl, initializer });
        let new_cur = parser.push(NodeList {
            item: decl,
            next: None,
        });
        parser[cur].next = Some(new_cur);
        cur = new_cur;
    }
    Ok(head)
}

/// Parse a c style for loop:
/// ```javascript
/// for(let i = foo /* start here */; i < 10;i++){ body }
/// ```
pub fn parse_c_style_for(parser: &mut Parser, decl: CstyleDecl) -> Result<NodeId<Stmt>> {
    expect!(parser, ";");
    let cond = if let t!(";") = parser.peek_kind() {
        None
    } else {
        Some(parser.parse()?)
    };
    expect!(parser, ";");
    let post = if let t!(")") = parser.peek_kind() {
        None
    } else {
        Some(parser.parse()?)
    };
    expect!(parser, ")");
    let head = parser.push(ForLoopHead::CStyle { decl, cond, post });
    alter_state!(parser => {
        parser.state.insert(ParserState::Break | ParserState::Continue);
        let body = parser.parse()?;
    });
    Ok(parser.push(Stmt::For { head, body }))
}

pub fn parse_for_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
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
            let binding = parser.parse()?;
            let binding_span = start.covers(parser.last_span());
            if let t!("=") | t!(",") = parser.peek_kind() {
                let decl = parse_c_style_decl(parser, kind, binding, binding_span)?;
                return parse_c_style_for(parser, CstyleDecl::Decl { kind, decl });
            } else {
                InOfDecl::Decl { kind, binding }
            }
        }
        t!(";") => return parse_c_style_for(parser, CstyleDecl::Empty),
        _ => {
            let state = parser.state;
            parser.state.remove(ParserState::In);
            let expr = parser.parse()?;

            if parser.eat(t!(",")) {
                let next = Some(parser.parse()?);
                let expr = parser.push(NodeList { item: expr, next });
                parser.state = state;
                return parse_c_style_for(parser, CstyleDecl::Expr { expr });
            }
            parser.state = state;
            InOfDecl::Expr { expr }
        }
    };

    let next = peek_expect!(parser);
    let head = match next.kind {
        t!(";") => {
            let decl = match decl {
                InOfDecl::Expr { expr } => CstyleDecl::Expr {
                    expr: parser.push(NodeList {
                        item: expr,
                        next: None,
                    }),
                },
                InOfDecl::Decl { kind, binding } => {
                    let decl = parser.push(VariableDecl {
                        decl: binding,
                        initializer: None,
                    });
                    let decl = parser.push(NodeList {
                        item: decl,
                        next: None,
                    });
                    CstyleDecl::Decl { kind, decl }
                }
            };
            return parse_c_style_for(parser, decl);
        }
        t!("in") => {
            parser.next();
            let expr = parser.parse()?;
            ForLoopHead::In { decl, expr }
        }
        t!("of") => {
            parser.next();
            let expr = parser.parse()?;
            ForLoopHead::Of { decl, expr }
        }
        //TODO 'of'
        x => unexpected!(parser, x, ";", "in"),
    };
    let head = parser.push(head);

    expect!(parser, ")");
    let body = parser.save_state(|parser| {
        parser
            .state
            .insert(ParserState::Break | ParserState::Continue);
        parser.parse()
    })?;
    Ok(parser.push(Stmt::For { head, body }))
}

pub fn parse_switch_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
    expect!(parser, "(");
    let cond = parser.parse()?;
    expect!(parser, ")");
    expect!(parser, "{");
    let mut head = None;
    let mut cur = None;
    let mut default = None;
    loop {
        let case = match parser.peek_kind() {
            t!("case") => {
                parser.next();
                Some(parser.parse()?)
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
                return Ok(parser.push(Stmt::Switch {
                    cond,
                    cases: head,
                    default,
                }));
            }
            x => unexpected!(parser, x, "case", "default", "}"),
        };
        expect!(parser, ":");

        let stmts = parser.save_state(|parser| -> Result<_> {
            parser.state.insert(ParserState::Break);
            let mut stmt_head = None;
            let mut stmt_cur = None;
            loop {
                match peek_expect!(parser).kind {
                    t!("case") | t!("default") | t!("}") => break,
                    _ => {
                        let stmt = parser.parse::<Stmt>()?;
                        parser.push_list(&mut stmt_head, &mut stmt_cur, stmt);
                    }
                }
            }
            Ok(stmt_head)
        })?;

        if let Some(expr) = case {
            let node = parser.push(CaseItem { expr, stmts });
            parser.push_list(&mut head, &mut cur, node);
        } else {
            // Default case
            default = stmts
        }
    }
}

pub fn parse_return_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
    debug_assert!(parser.peek.is_none());
    if parser.eat_semicolon() {
        Ok(parser.push(Stmt::Return { expr: None }))
    } else {
        let expr = Some(parser.parse()?);
        parser.semicolon()?;
        Ok(parser.push(Stmt::Return { expr }))
    }
}

pub fn parse_cntrl_flow_stmt(parser: &mut Parser, is_break: bool) -> Result<NodeId<Stmt>> {
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
        if let t!("ident") = token.kind {
            parser.next();
            Some(token.data.unwrap().entype())
        } else {
            unexpected!(parser, token.kind, "ident");
        }
    };

    let node = if is_break {
        Stmt::Break { label }
    } else {
        Stmt::Continue { label }
    };
    Ok(parser.push(node))
}

pub fn parse_try_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
    expect!(parser, "{");
    let block = parse_block_stmt(parser)?;
    let catch = parser
        .eat(t!("catch"))
        .then(|| {
            let binding = parser
                .eat(t!("("))
                .then(|| {
                    let res = parser.parse()?;
                    expect!(parser, ")");
                    Ok(res)
                })
                .transpose()?;

            expect!(parser, "{");
            let block = parse_block_stmt(parser)?;
            Ok(parser.push(CatchStmt { binding, block }))
        })
        .transpose()?;

    let finally = parser
        .eat(t!("finally"))
        .then(|| {
            expect!(parser, "{");
            parse_block_stmt(parser)
        })
        .transpose()?
        .and_then(|x| x);

    Ok(parser.push(Stmt::Try {
        block,
        catch,
        finally,
    }))
}

pub fn parse_throw_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
    peek_expect!(parser);
    parser.no_line_terminator()?;
    let expr = parser.parse()?;
    parser.semicolon()?;
    Ok(parser.push(Stmt::Throw { expr }))
}

pub fn parse_with_stmt(parser: &mut Parser) -> Result<NodeId<Stmt>> {
    if parser.state.contains(ParserState::Strict) {
        todo!("disallow with in strict mode")
    }
    expect!(parser, "(");
    let expr = parser.parse()?;
    expect!(parser, ")");
    let stmt = parser.parse()?;
    Ok(parser.push(Stmt::With { expr, stmt }))
}

/// Parse a variable declaration, e.g. any of:
/// ```javascript
/// var a = 1;
/// let [b,,,...rest] = 1;
/// const { c } = foo;
/// ```
pub fn parse_variable_decl(parser: &mut Parser, kind: VariableKind) -> Result<NodeId<Stmt>> {
    let span = parser.peek().span;

    let decl = parser.parse::<IdentOrPattern>()?;
    let decl_span = span.covers(parser.last_span());

    let initializer = parser.eat(t!("=")).then(|| parser.parse()).transpose()?;

    if initializer.is_none() {
        if kind == VariableKind::Const {
            return Err(Error::new(ErrorKind::ConstNotInitialized, decl_span));
        }
        if matches!(parser[decl], IdentOrPattern::Pattern { .. }) {
            return Err(Error::new(ErrorKind::DestructringNotInitalized, decl_span));
        }
    }

    let decl = parser.push(VariableDecl { decl, initializer });

    let mut head = None;
    let mut cur = None;

    parser.push_list(&mut head, &mut cur, decl);
    while parser.eat(t!(",")) {
        let span = parser.peek().span;
        let decl = parser.parse::<IdentOrPattern>()?;
        let decl_span = span.covers(parser.last_span());

        let initializer = parser.eat(t!("=")).then(|| parser.parse()).transpose()?;

        if initializer.is_none() {
            if kind == VariableKind::Const {
                return Err(Error::new(ErrorKind::ConstNotInitialized, decl_span));
            }
            if matches!(parser[decl], IdentOrPattern::Pattern { .. }) {
                return Err(Error::new(ErrorKind::DestructringNotInitalized, decl_span));
            }
        }

        let decl = parser.push(VariableDecl { decl, initializer });
        parser.push_list(&mut head, &mut cur, decl);
    }
    parser.semicolon()?;

    let res = parser.push(Stmt::VariableDecl {
        kind,
        decl: head.unwrap(),
    });

    Ok(res)
}
