use ast::{
    Argument, AssignOp, BaseOp, BinaryOp, BindingPattern, Expr, NodeId, NodeList, NodeListId,
    PostfixOp, PrefixOp, PrimeExpr,
};
use common::span::Span;
use token::{t, TokenKind};

use crate::{
    binding::parse_ident_name,
    expect,
    prime::{parse_prime, parse_template, reparse_array_lit, reparse_object_lit},
    unexpected, Error, ErrorKind, Parse, Parser, ParserState, Result,
};

fn infix_binding_power(kind: TokenKind) -> Option<(u8, u8)> {
    match kind {
        t!("=")
        | t!("*=")
        | t!("/=")
        | t!("%=")
        | t!("+=")
        | t!("-=")
        | t!("<<=")
        | t!(">>=")
        | t!(">>>=")
        | t!("&=")
        | t!("^=")
        | t!("|=")
        | t!("**=") => Some((2, 1)),
        t!("?") => Some((4, 3)),
        t!("??") => Some((6, 5)),
        t!("||") => Some((7, 8)),
        t!("&&") => Some((9, 10)),
        t!("|") => Some((11, 12)),
        t!("^") => Some((13, 14)),
        t!("&") => Some((15, 16)),
        t!("==") | t!("!=") | t!("===") | t!("!==") => Some((17, 18)),
        t!("<") | t!(">") | t!("<=") | t!(">=") | t!("instanceof") | t!("in") => Some((19, 20)),
        t!("<<") | t!(">>") | t!(">>>") => Some((21, 22)),
        t!("+") | t!("-") => Some((23, 24)),
        t!("*") | t!("%") | t!("/") => Some((25, 26)),
        t!("**") => Some((27, 28)),
        t!("?.") => Some((36, 35)),
        //t!(".") => Some((38, 37)),
        _ => None,
    }
}

fn postfix_binding_power(kind: TokenKind) -> Option<(u8, ())> {
    match kind {
        t!("++") | t!("--") => Some((31, ())),
        t!("[") | t!(".") | t!("(") | t!("``") | t!("` ${") => Some((38, ())),
        _ => None,
    }
}

fn prefix_binding_power(kind: TokenKind) -> Option<((), u8)> {
    match kind {
        t!("await")
        | t!("delete")
        | t!("void")
        | t!("typeof")
        | t!("+")
        | t!("-")
        | t!("~")
        | t!("!") => Some(((), 29)),
        t!("++") | t!("--") => Some(((), 31)),
        t!("new") => Some(((), 33)),
        _ => None,
    }
}

impl Parse for NodeList<Expr> {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        let first = parser.parse()?;

        let mut head: Option<NodeListId<Expr>> = None;
        let mut cur = None;

        parser.push_list(&mut head, &mut cur, first)?;

        while parser.eat(t!(",")) {
            let first = parser.parse()?;
            parser.push_list(&mut head, &mut cur, first)?;
        }

        Ok(head.unwrap())
    }
}

impl Parse for Expr {
    fn parse(parser: &mut Parser) -> Result<NodeId<Self>> {
        if !parser.state.contains(ParserState::YieldIdent) && parser.eat(t!("yield")) {
            let star = parser.eat(t!("*"));
            let expr = parser.parse()?;
            return Ok(parser.push(Expr::Yield { star, expr })?);
        }
        pratt_parse_expr(parser, 0)
    }
}

/// Parsers any prefix operator, called in a pratt parser.
/// Only call if the next token is prefix operator.
fn parse_prefix_op(parser: &mut Parser, r_bp: u8) -> Result<NodeId<Expr>> {
    let token = parser.peek();

    let operator = match token.kind {
        t!("delete") => PrefixOp::Delete,
        t!("void") => PrefixOp::Void,
        t!("typeof") => PrefixOp::TypeOf,
        t!("new") => {
            parser.next();
            if parser.eat(t!(".")) {
                let token = parser.next();
                if token.kind != t!("target") {
                    unexpected!(parser, token.kind, "target");
                }
                let expr = parser.push(PrimeExpr::NewTarget)?;
                return Ok(parser.push(Expr::Prime { expr })?);
            }
            let expr = pratt_parse_expr(parser, r_bp)?;
            return Ok(parser.push(Expr::Prefix {
                op: PrefixOp::New,
                expr,
            })?);
        }
        t!("await") => {
            if parser.state.contains(ParserState::AwaitIdent) {
                let (expr, _) = parse_prime(parser)?;
                return Ok(parser.push(Expr::Prime { expr })?);
            } else {
                PrefixOp::Await
            }
        }
        t!("+") => PrefixOp::Plus,
        t!("-") => PrefixOp::Minus,
        t!("!") => PrefixOp::Not,
        t!("~") => PrefixOp::BitwiseNot,
        t!("++") => PrefixOp::AddOne,
        t!("--") => PrefixOp::SubOne,
        _ => {
            panic!("`parse_prefix_op` should only be called when the next token is a operator")
        }
    };
    parser.next();

    let expr = pratt_parse_expr(parser, r_bp)?;
    Ok(parser.push(Expr::Prefix { op: operator, expr })?)
}

/// Parsers any postfix operator, called in a pratt parser.
/// Only call if the next token is postfix operator.
fn parse_postfix_op(parser: &mut Parser, _l_bp: u8, lhs: NodeId<Expr>) -> Result<NodeId<Expr>> {
    let token = parser.peek();

    let op = match token.kind {
        t!("++") => PostfixOp::AddOne,
        t!("--") => PostfixOp::SubOne,
        t!("[") => {
            parser.next();
            let index = parser.parse()?;
            expect!(parser, "]");
            return Ok(parser.push(Expr::Index { index, expr: lhs })?);
        }
        t!(".") => {
            parser.next();
            let ident = parse_ident_name(parser)?;
            return Ok(parser.push(Expr::Dot { ident, expr: lhs })?);
        }
        t!("(") => {
            parser.next();
            let args = parse_arguments(parser)?;
            return Ok(parser.push(Expr::Call { args, expr: lhs })?);
        }
        t!("` ${") | t!("``") => {
            let template = parse_template(parser)?;
            return Ok(parser.push(Expr::TaggedTemplate { tag: lhs, template })?);
        }
        x => panic!("`parse_postfix_op` called with not a token {:?}", x),
    };
    parser.next();

    Ok(parser.push(Expr::Postfix { op, expr: lhs })?)
}

/// Parsers any infix operator, called in a pratt parser.
/// Only call if the next token is infix operator.
fn parse_infix_op(
    parser: &mut Parser,
    r_bp: u8,
    lhs: NodeId<Expr>,
    lhs_span: Span,
) -> Result<NodeId<Expr>> {
    let token = parser.next();
    let op = match token.kind {
        t!("??") => BinaryOp::Base(BaseOp::NullCoalessing),
        t!("?.") => BinaryOp::Base(BaseOp::TenaryNull),
        t!("||") => BinaryOp::Base(BaseOp::Or),
        t!("&&") => BinaryOp::Base(BaseOp::And),
        t!("&") => BinaryOp::Base(BaseOp::BitwiseAnd),
        t!("|") => BinaryOp::Base(BaseOp::BitwiseOr),
        t!("^") => BinaryOp::Base(BaseOp::BitwiseXor),
        t!("+") => BinaryOp::Base(BaseOp::Add),
        t!("-") => BinaryOp::Base(BaseOp::Sub),
        t!("*") => BinaryOp::Base(BaseOp::Mul),
        t!("/") => BinaryOp::Base(BaseOp::Div),
        t!("%") => BinaryOp::Base(BaseOp::Mod),
        t!("**") => BinaryOp::Base(BaseOp::Exp),
        t!("<") => BinaryOp::Base(BaseOp::Less),
        t!("<=") => BinaryOp::Base(BaseOp::LessEqual),
        t!(">") => BinaryOp::Base(BaseOp::Greater),
        t!(">=") => BinaryOp::Base(BaseOp::GreaterEqual),
        t!("<<") => BinaryOp::Base(BaseOp::ShiftLeft),
        t!(">>") => BinaryOp::Base(BaseOp::ShiftRight),
        t!(">>>") => BinaryOp::Base(BaseOp::ShiftRightUnsigned),
        t!("instanceof") => BinaryOp::Base(BaseOp::InstanceOf),
        t!("in") => BinaryOp::Base(BaseOp::In),
        t!("==") => BinaryOp::Base(BaseOp::Equal),
        t!("===") => BinaryOp::Base(BaseOp::StrictEqual),
        t!("!=") => BinaryOp::Base(BaseOp::NotEqual),
        t!("!==") => BinaryOp::Base(BaseOp::StrictNotEqual),
        t!("=") => {
            if let Expr::Prime { expr } = parser[lhs] {
                // Test for destructuring assignment.
                if let Some(pattern) = reparse_destructuring(parser, lhs_span, expr)? {
                    let right = pratt_parse_expr(parser, r_bp)?;
                    let res = parser.push(Expr::Destructure {
                        pattern,
                        expr: right,
                    })?;
                    return Ok(res);
                }
            }
            BinaryOp::Assign(AssignOp::Assign)
        }
        t!("+=") => BinaryOp::Assign(AssignOp::Add),
        t!("-=") => BinaryOp::Assign(AssignOp::Sub),
        t!("*=") => BinaryOp::Assign(AssignOp::Mul),
        t!("/=") => BinaryOp::Assign(AssignOp::Div),
        t!("%=") => BinaryOp::Assign(AssignOp::Mod),
        t!("**=") => BinaryOp::Assign(AssignOp::Exp),
        t!("<<=") => BinaryOp::Assign(AssignOp::ShiftLeft),
        t!(">>=") => BinaryOp::Assign(AssignOp::ShiftRight),
        t!(">>>=") => BinaryOp::Assign(AssignOp::ShiftRightUnsigned),
        t!("&=") => BinaryOp::Assign(AssignOp::BitwiseAnd),
        t!("|=") => BinaryOp::Assign(AssignOp::BitwiseOr),
        t!("^=") => BinaryOp::Assign(AssignOp::BitwiseXor),
        t!("?") => {
            let then = parser.parse()?;
            expect!(parser, ":");
            let r#else = parser.parse()?;
            let ternary = parser.push(ast::Tenary {
                cond: lhs,
                then,
                r#else,
            })?;
            return Ok(parser.push(Expr::Ternary { ternary })?);
        }
        _ => {
            panic!("`parse_infix_op` called without a infix operator")
        }
    };

    if let BinaryOp::Assign(_) = op {
        if !is_assignable(parser, lhs) {
            return Err(Error::new(ErrorKind::NotAssignable, lhs_span));
        }
    }

    let right = pratt_parse_expr(parser, r_bp)?;
    Ok(parser.push(Expr::Binary {
        op,
        left: lhs,
        right,
    })?)
}

/// The pratt parser, uses binding power to parse operator with correct precedence.
fn pratt_parse_expr(parser: &mut Parser, min_bp: u8) -> Result<NodeId<Expr>> {
    let start_span = parser.peek().span;
    let mut lhs = if let Some(((), r_bp)) = prefix_binding_power(parser.peek_kind()) {
        parse_prefix_op(parser, r_bp)?
    } else {
        let (expr, span) = parse_prime(parser)?;
        if let Some(span) = span {
            let lhs_span = start_span.covers(parser.last_span());
            // Found a covered initializer in an object literal.
            // If the next token isn't `=` this would be invalid.
            if let t!("=") = parser.peek_kind() {
                parser.next();
                // This should always succeed, otherwise a span was returned when the prime
                // expression wasn't an object literal with a covered initializer.
                let pattern = reparse_destructuring(parser, lhs_span, expr)?.unwrap();
                let right = pratt_parse_expr(parser, min_bp)?;
                let res = parser.push(Expr::Destructure {
                    pattern,
                    expr: right,
                })?;
                return Ok(res);
            } else {
                return Err(Error::new(ErrorKind::CoveredObjectLiteral, span));
            }
        }

        parser.push(Expr::Prime { expr })?
    };

    let mut lhs_span = start_span.covers(parser.last_span());
    loop {
        let op = parser.peek_kind();
        if let Some((l_bp, ())) = postfix_binding_power(op) {
            if l_bp < min_bp {
                break;
            }
            lhs = parse_postfix_op(parser, l_bp, lhs)?;
        } else {
            let Some((l_bp, r_bp)) = infix_binding_power(op) else {
                break;
            };
            // In is not allowed in some contexts.
            if !parser.state.contains(ParserState::In) && t!("in") == op {
                break;
            }
            if l_bp < min_bp {
                break;
            }
            lhs = parse_infix_op(parser, r_bp, lhs, lhs_span)?;
            lhs_span = start_span.covers(parser.last_span());
        }
    }
    Ok(lhs)
}

/// Parse argument for a function call.
/// ```javascript
/// foo(/* start here */ 1,2,...b)
/// ```
fn parse_arguments(parser: &mut Parser) -> Result<Option<NodeId<NodeList<Argument>>>> {
    let mut head = None;
    let mut cur = None;

    loop {
        let mut is_spread = false;
        match parser.peek_kind() {
            t!(")") => break,
            t!("...") => {
                parser.next();
                is_spread = true;
            }
            _ => {}
        };
        let expr = parser.parse()?;
        let argument = parser.push(Argument { is_spread, expr })?;
        parser.push_list(&mut head, &mut cur, argument)?;
        if !parser.eat(t!(",")) {
            break;
        }
    }
    expect!(parser, ")");
    Ok(head)
}

fn is_assignable(parser: &mut Parser, mut tgt: NodeId<ast::Expr>) -> bool {
    loop {
        match parser[tgt] {
            Expr::Binary { .. }
            | Expr::Prefix { .. }
            | Expr::Postfix { .. }
            | Expr::Ternary { .. }
            | Expr::Call { .. }
            | Expr::Yield { .. }
            | Expr::TaggedTemplate { .. }
            | Expr::Destructure { .. } => return false,
            Expr::Index { .. } | Expr::Dot { .. } => return true,
            Expr::Prime { expr } => match parser[expr] {
                PrimeExpr::Number { .. }
                | PrimeExpr::String { .. }
                | PrimeExpr::Template { .. }
                | PrimeExpr::Regex { .. }
                | PrimeExpr::Boolean { .. }
                | PrimeExpr::Function { .. }
                | PrimeExpr::Class { .. }
                | PrimeExpr::Object { .. }
                | PrimeExpr::Array { .. }
                | PrimeExpr::This
                | PrimeExpr::Null
                | PrimeExpr::NewTarget => return false,
                PrimeExpr::Covered { expr } => {
                    let list = &parser[expr];
                    if list.next.is_some() {
                        return false;
                    }
                    tgt = list.item;
                }
                PrimeExpr::Ident { .. } | PrimeExpr::Super => return true,
            },
        }
    }
}

pub fn reparse_destructuring(
    parser: &mut Parser,
    span: Span,
    pattern: NodeId<PrimeExpr>,
) -> Result<Option<NodeId<BindingPattern>>> {
    let pat = match parser[pattern] {
        PrimeExpr::Object { object } => reparse_object_lit(parser, object)?
            .ok_or_else(|| Error::new(ErrorKind::InvalidDestructuringAssigment, span))?,
        PrimeExpr::Array { array } => reparse_array_lit(parser, array)?
            .ok_or_else(|| Error::new(ErrorKind::InvalidDestructuringAssigment, span))?,
        _ => return Ok(None),
    };
    Ok(Some(parser.push(pat)?))
}

#[cfg(test)]
mod test {
    use ast::{BaseOp, BinaryOp, Expr, PrimeExpr};

    use crate::create_test_parser;

    #[test]
    fn basic() {
        create_test_parser!("1 + 2", parser);
        let Ok(x) = parser.parse() else { panic!() };
        let Expr::Binary { op, left, right } = parser[x] else {
            panic!()
        };
        let BinaryOp::Base(BaseOp::Add) = op else {
            panic!()
        };
        let Expr::Prime { expr: left } = parser[left] else {
            panic!()
        };
        let Expr::Prime { expr: right } = parser[right] else {
            panic!()
        };
        let PrimeExpr::Number { value: left } = parser[left] else {
            panic!()
        };
        let PrimeExpr::Number { value: right } = parser[right] else {
            panic!()
        };
        let left = parser[left];
        assert_eq!(left.0, 1.0);
        let right = parser[right];
        assert_eq!(right.0, 2.0);
    }
}
