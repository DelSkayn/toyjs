pub enum Operand {
    Ident(Token),
    Super,
    Import,
}

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
        | t!("**=") => Some((1, 2)),
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
        t!(".") => Some((38, 37)),
        _ => None,
    }
}

fn postfix_binding_power(kind: TokenKind) -> Option<(u8, ())> {
    match kind {
        t!("++") | t!("--") => Some((31, ())),
        t!("[") | t!("(") => Some((38, ())),
        _ => None,
    }
}

fn prefix_binding_power(kind: TokenKind) -> Option<((), u8)> {
    match kind {
        t!("delete") | t!("void") | t!("typeof") | t!("+") | t!("-") | t!("~") | t!("!") => {
            Some(((), 29))
        }
        t!("++") | t!("--") => Some(((), 31)),
        t!("new") => Some(((), 33)),
        _ => None,
    }
}

impl<'a> Parser<'a> {
    pub fn parse_ops(&mut self, min_bp: u8) -> PResult<'a, Op> {
        trace_log!("assign expr");
        let mut lhs =
            if let Some(((), r_bp)) = self.peek_kind().and_then(|x| prefix_binding_power(x)) {
                let op = PrefixOp::from_token(self.next().unwrap()).unwrap();
                match op {
                    PrefixOp::New => {
                        if eat!(self, ".") {
                            expect!(self, "meta");
                            Op::NewTarget
                        } else {
                            let expr = Box::new(self.parse_ops(r_bp)?);
                            Op::Prefix {
                                op: PrefixOp::New,
                                expr,
                            }
                        }
                    }
                    op => {
                        let expr = Box::new(self.parse_ops(r_bp)?);
                        Op::Prefix { op, expr }
                    }
                }
            } else {
                let exp = self.parse_primary_expr()?;
                Op::Prime(exp)
            };

        loop {
            let op = match self.peek_kind() {
                Some(x) => x,
                None => break,
            };

            if let Some((l_bp, ())) = postfix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                if eat!(self, "[") {
                    let expr = self.parse_expr()?;
                    expect!(self, "]");
                    lhs = Op::Postfix {
                        expr: Box::new(lhs),
                        op: PostfixOp::Index(expr),
                    };
                } else if is!(self, "(") {
                    let args = self.parse_arguments()?;
                    lhs = Op::Postfix {
                        expr: Box::new(lhs),
                        op: PostfixOp::Call(args),
                    };
                } else {
                    lhs = Op::Postfix {
                        expr: Box::new(lhs),
                        op: if eat!(self, "++") {
                            PostfixOp::Increment
                        } else if eat!(self, "--") {
                            PostfixOp::Decrement
                        } else {
                            panic!(
                                "encountered postfix operator with binding power but no production"
                            )
                        },
                    };
                }
                continue;
            }

            if let Some((l_bp, r_bp)) = infix_binding_power(op) {
                if l_bp < min_bp {
                    break;
                }
                if eat!(self, "?") {
                    let mhs = self.parse_ops(0)?;
                    expect!(self, ":");
                    let rhs = self.parse_ops(r_bp)?;
                    lhs = Op::Bin {
                        lhs: Box::new(lhs),
                        op: BinOp::Tenary(Box::new(mhs)),
                        rhs: Box::new(rhs),
                    };
                } else if eat!(self, "[") {
                } else {
                    let op = BinOp::from_token(self.peek().unwrap().clone()).unwrap();
                    match op {
                        BinOp::Assign(_) => {
                            if !lhs.is_assign_lhs() {
                                unexpected!(self => "left-hand side is not assignable")
                            }
                        }
                        _ => {}
                    }
                    self.next();
                    let rhs = self.parse_ops(r_bp)?;
                    lhs = Op::Bin {
                        lhs: Box::new(lhs),
                        op,
                        rhs: Box::new(rhs),
                    }
                }
                continue;
            }
            break;
        }
        Ok(lhs)
    }
}
res = []
for i in range(len(lst)-2):
    if lst[i] == list[i+2] and lst[i] != list[i+1]:
        res.append([lst[i],lst[i+1],lst[i+2]])
return res;

