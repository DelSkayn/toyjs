use ast::{Function, FunctionKind, NodeId, NodeListId, Stmt};
use token::{t, TokenKind};

use crate::{alter_state, expect, is_strict_directive, unexpected, Parser, ParserState, Result};

pub struct FunctionBody {
    pub body: Option<NodeListId<Stmt>>,
    pub is_strict: bool,
}

pub enum FunctionCtx {
    Stmt,
    Expression,
    Method,
}

/// Parses the parameters and body of a function:
/// ```javascript
/// // function declaration.
/// function foo /* starts here */(a,b){
///     return a + b
/// }
/// ({
///     // member
///     foo /* starts here */(a,b){
///         return a + b
///     }
/// })
/// ```
pub fn parse_function(
    parser: &mut Parser,
    ctx: FunctionCtx,
    kind: FunctionKind,
) -> Result<NodeId<Function>> {
    let name = match ctx {
        FunctionCtx::Stmt => Some(parser.parse()?),
        FunctionCtx::Expression => {
            let token = parser.peek();
            match token.kind {
                TokenKind::UnreservedKeyword(_) | TokenKind::Ident => Some(parser.parse()?),
                t!("(") => None,
                _ => {
                    unexpected!(parser, token.kind, "ident");
                }
            }
        }
        FunctionCtx::Method => None,
    };

    expect!(parser, "(");
    let mut head = None;
    let mut cur = None;
    let mut rest_param = None;
    loop {
        match parser.peek_kind() {
            t!(")") => {
                break;
            }
            t!("...") => {
                parser.next();
                rest_param = Some(parser.parse()?);
                break;
            }
            _ => {
                let elem = parser.parse()?;
                parser.push_list(&mut head, &mut cur, elem)?;
                if !parser.eat(t!(",")) {
                    break;
                }
            }
        }
    }
    expect!(parser, ")");

    alter_state!(parser => {
        parser.state.set(ParserState::AwaitIdent,
                       matches!(kind,FunctionKind::Simple | FunctionKind::Generator));
        parser.state.set(ParserState::YieldIdent,
                       matches!(kind,FunctionKind::Simple | FunctionKind::Async));
        let body = parse_function_body(parser)?;
    });

    let function = match ctx {
        FunctionCtx::Method | FunctionCtx::Expression => parser.push(Function::Expr {
            name,
            kind,
            params: head,
            rest_param,
            body: body.body,
            is_strict: body.is_strict,
        })?,
        FunctionCtx::Stmt => parser.push(Function::Declared {
            name: name.unwrap(),
            kind,
            params: head,
            rest_param,
            body: body.body,
            is_strict: body.is_strict,
        })?,
    };
    Ok(function)
}

/// Parses the parameters and body of a getter method:
pub fn parse_getter(parser: &mut Parser) -> Result<NodeId<Function>> {
    expect!(parser,"(" => "expected start of getter parameters");
    expect!(parser,")" => "getter can't have any parameters");
    let body = parse_function_body(parser)?;
    Ok(parser.push(Function::Expr {
        is_strict: body.is_strict,
        kind: FunctionKind::Simple,
        name: None,
        params: None,
        rest_param: None,
        body: body.body,
    })?)
}

/// Parses the parameters and body of a setter method:
pub fn parse_setter(parser: &mut Parser) -> Result<NodeId<Function>> {
    expect!(parser,"(" => "expected start of setter parameters");
    let param = parser.parse()?;
    let mut head = None;
    parser.push_list(&mut head, &mut None, param)?;
    expect!(parser,")" => "setter must have a single parameter");

    let body = parse_function_body(parser)?;
    Ok(parser.push(Function::Expr {
        is_strict: body.is_strict,
        kind: FunctionKind::Simple,
        name: None,
        params: head,
        rest_param: None,
        body: body.body,
    })?)
}

/// Parses the body of a function:
/// function foo() /* start here */ {
///     // some body
/// }
pub fn parse_function_body(parser: &mut Parser) -> Result<FunctionBody> {
    expect!(parser, "{");
    let state = parser.state;

    let mut head = None;
    let mut cur = None;

    loop {
        if let t!("}") = parser.peek_kind() {
            parser.next();
            break;
        }

        let stmt = parser.parse()?;
        if head.is_none()
            && !parser.state.contains(ParserState::Strict)
            && is_strict_directive(parser, stmt)
        {
            parser.state.insert(ParserState::Strict);
        }
        parser.push_list(&mut head, &mut cur, stmt)?;
    }
    let is_strict = parser.state.contains(ParserState::Strict);
    parser.state = state;
    Ok(FunctionBody {
        body: head,
        is_strict,
    })
}
