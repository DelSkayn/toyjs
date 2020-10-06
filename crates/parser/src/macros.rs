macro_rules! syntax_error {
    ($s:expr,$err:expr) => {{
        let e = $crate::Error {
            kind: $err,
            origin: $s.last_span,
        };
        return Err(e).into();
    }};
}

macro_rules! to_do {
    ($s:expr) => {
        syntax_error!(
            $s,
            $crate::ErrorKind::Todo {
                line: line!(),
                file: file!(),
                token: None,
            }
        )
    };
    ($s:expr, $token:ident) => {
        syntax_error!(
            $s,
            $crate::ErrorKind::Todo {
                line: line!(),
                file: file!(),
                token: Some(format!("{:?}", $token)),
            }
        )
    };
}

macro_rules! unexpected{
    ($s:expr$(,$t:tt)*  $(=> $r:tt)*) => {
        syntax_error!($s,$crate::ErrorKind::UnexpectedToken{
            found: $s.peek()?,
            expected: &[$($t,)*],
            reason: unexpected!(=> $($r)*)
        });
    };
    (=> $r:tt) => {
        Some($r)
    };
    (=>) => {
        None
    }
}

macro_rules! expect{
    ($s:expr$(,$t:tt)* $(=> $r:tt)*) => {{
        let p = $s.peek()?;
        match p.map(|e| e.kind){
            $(Some(token::t!($t)) => {
                $s.next()?.unwrap()
            })*
            _ => {
                unexpected!($s $(,$t)* $(=> $r)*)
            }
        }
    }};
}

macro_rules! expect_bind {
    ($s:expr, let $idnt:ident = $t:tt $(=> $r:tt)*) => {
        let $idnt = match $s.peek_kind()? {
            Some(token::t!($t, $idnt)) => {
                $s.next()?.unwrap();
                $idnt
            }
            _ => {
                unexpected!($s ,$t $(=> $r)*)
            }
        };
    };
}
