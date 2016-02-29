#![allow(unused)]
/// This module contains unit tests for the tokenizer.
///

use super::*;
use lex::token::Token::*;
use base::code::{FileMap, Span, BytePos};
use base::diag;
use std::rc::Rc;

fn raw_toks(src: &str) -> Vec<Result<TokenSpan, TokenSpan>> {
    let fmap = Rc::new(FileMap::new("<unit-test>", src));
    let mut lexer = Tokenizer::new(&fmap);
    lexer.collect()
}

fn spans(src: &str) -> Vec<TokenSpan> {
    let fmap = Rc::new(FileMap::new("<unit-test>", src));
    let mut lexer = Tokenizer::new(&fmap);
    lexer.take_while(|t| t.is_ok()).map(|t| t.unwrap()).collect()
}

fn toks(src: &str) -> Vec<Token> {
    spans(src).into_iter().map(|ts| ts.tok).collect()
}

fn reals(src: &str) -> Vec<Token> {
    toks(src).into_iter().filter(|t| t.is_real()).collect()
}

macro_rules! toks {
    ($s:expr, [$($v:expr),*]) => {
        assert_eq!(toks($s), vec![$($v),*])
    }
}
macro_rules! reals {
    ($s:expr, [$($v:expr),*]) => {
        assert_eq!(reals($s), vec![$($v),*])
    }
}


#[test]
fn empty() {
    toks!("", []);
}

#[test]
fn idents() {
    toks!("foo", [Ident("foo".into())]);
    toks!("foo bar", [
        Ident("foo".into()),
        Whitespace,
        Ident("bar".into())
    ]);
    toks!("1bla", [
        Literal(Lit::Integer { raw: "1".into(), is_long: false, radix: 10 }),
        Ident("bla".into())
    ]);
    toks!("b1la", [Ident("b1la".into())]);

}

#[test]
fn ops() {
    // all seperators and operators
    reals!("(   )   {   }   [   ]   ;   ,   .   ...   @   ::", [
        ParenOp, ParenCl, BraceOp, BraceCl, BracketOp, BracketCl,
        Semi, Comma, Dot, DotDotDot, At, ColonSep
    ]);
    reals!("=   >   <   !   ~   ?   :   ->", [
        Eq, Gt, Lt, Bang, Tilde, Question, Colon, Arrow
    ]);
    reals!("==  >=  <=  !=  &&  ||  ++  --", [
        EqEq, Ge, Le, Ne, AndAnd, OrOr, PlusPlus, MinusMinus
    ]);
    reals!("+   -   *   /   &   |   ^   %   <<   >>   >>>", [
        Plus, Minus, Star, Slash, And, Or, Caret, Percent, Shl, Shr, ShrUn
    ]);
    reals!("+=  -=  *=  /=  &=  |=  ^=  %=  <<=  >>=  >>>=", [
        PlusEq, MinusEq, StarEq, SlashEq, AndEq, OrEq, CaretEq, PercentEq,
        ShlEq, ShrEq, ShrUnEq
    ]);

    // multi char op stress test
    reals!(">>>>>>=>> >>=> >=", [ShrUn, ShrUnEq, Shr, ShrEq, Gt, Ge]);
    reals!("<< <<=< <=", [Shl, ShlEq, Lt, Le]);
}

#[test]
fn easy_literals() {
    reals!("true false null", [
        Literal(Lit::Bool(true)), Literal(Lit::Bool(false)), Literal(Lit::Null)
    ]);
    reals!("truefalse null", [Ident("truefalse".into()), Literal(Lit::Null)]);
}

#[test]
fn int_literals() {
    assert_eq!(toks("123"), vec![
        Literal(Lit::Integer { raw: "123".into(), is_long: false, radix: 10 })
    ]);
    assert_eq!(toks("123l"), vec![
        Literal(Lit::Integer { raw: "123".into(), is_long: true, radix: 10 })
    ]);
    assert_eq!(toks("0123"), vec![
        Literal(Lit::Integer { raw: "123".into(), is_long: false, radix: 8 })
    ]);
    assert_eq!(toks("0x1fa3l"), vec![
        Literal(Lit::Integer { raw: "1fa3".into(), is_long: true, radix: 16 })
    ]);
    assert_eq!(toks("0x1f"), vec![
        Literal(Lit::Integer { raw: "1f".into(), is_long: false, radix: 16 })
    ]);
    assert_eq!(toks("0b101l"), vec![
        Literal(Lit::Integer { raw: "101".into(), is_long: true, radix: 2 })
    ]);
    assert_eq!(toks("0l"), vec![
        Literal(Lit::Integer { raw: "0".into(), is_long: true, radix: 10 })
    ]);
}

#[test]
fn float_literals() {
    // type 1:  Digits . [Digits] [ExponentPart] [FloatTypeSuffix]
    toks!("3.", [Literal(Lit::Float {   // digit dot
        raw: "3.".into(),
        is_double: true,
        radix: 10,
        exp: "".into()
    })]);
    toks!("3.14", [Literal(Lit::Float { // digit dot digit
        raw: "3.14".into(),
        is_double: true,
        radix: 10,
        exp: "".into()
    })]);
    toks!("3.e2", [Literal(Lit::Float { // digit dot exp
        raw: "3.".into(),
        is_double: true,
        radix: 10,
        exp: "2".into()
    })]);
    toks!("3.f", [Literal(Lit::Float {  // digit dot suffix
        raw: "3.".into(),
        is_double: false,
        radix: 10,
        exp: "".into()
    })]);
    toks!("3.14e-3", [Literal(Lit::Float {   // digit dot digit exp
        raw: "3.14".into(),
        is_double: true,
        radix: 10,
        exp: "-3".into()
    })]);
    toks!("3.14f", [Literal(Lit::Float {    // digit dot digit suffix
        raw: "3.14".into(),
        is_double: false,
        radix: 10,
        exp: "".into()
    })]);
    toks!("3.e3f", [Literal(Lit::Float {    // digit dot exp suffix
        raw: "3.".into(),
        is_double: false,
        radix: 10,
        exp: "3".into()
    })]);
    toks!("3.14e-3f", [Literal(Lit::Float {  // digit dot digit exp suffix
        raw: "3.14".into(),
        is_double: false,
        radix: 10,
        exp: "-3".into()
    })]);

    // type 2: . Digits [ExponentPart] [FloatTypeSuffix]
    toks!(".14", [Literal(Lit::Float {
        raw: ".14".into(),
        is_double: true,
        radix: 10,
        exp: "".into()
    })]);
    toks!(".14e-3", [Literal(Lit::Float {
        raw: ".14".into(),
        is_double: true,
        radix: 10,
        exp: "-3".into()
    })]);
    toks!(".14f", [Literal(Lit::Float {
        raw: ".14".into(),
        is_double: false,
        radix: 10,
        exp: "".into()
    })]);
    toks!(".14e3f", [Literal(Lit::Float {
        raw: ".14".into(),
        is_double: false,
        radix: 10,
        exp: "3".into()
    })]);

    // type 3:  Digits ExponentPart [FloatTypeSuffix]
    // type 4: Digits [ExponentPart] FloatTypeSuffix
    toks!("3e-3", [Literal(Lit::Float {
        raw: "3".into(),
        is_double: true,
        radix: 10,
        exp: "-3".into()
    })]);
    toks!("3f", [Literal(Lit::Float {
        raw: "3".into(),
        is_double: false,
        radix: 10,
        exp: "".into()
    })]);
    toks!("3e3f", [Literal(Lit::Float {
        raw: "3".into(),
        is_double: false,
        radix: 10,
        exp: "3".into()
    })]);


    // floating point literals
    toks!("0x3p4", [Literal(Lit::Float {
        raw: "3".into(),
        is_double: true,
        radix: 16,
        exp: "4".into()
    })]);
    toks!("0x3p4_4f", [Literal(Lit::Float {
        raw: "3".into(),
        is_double: false,
        radix: 16,
        exp: "44".into()
    })]);

    toks!("0x3.p4", [Literal(Lit::Float {
        raw: "3.".into(),
        is_double: true,
        radix: 16,
        exp: "4".into()
    })]);
    toks!("0x378.p4f", [Literal(Lit::Float {
        raw: "378.".into(),
        is_double: false,
        radix: 16,
        exp: "4".into()
    })]);

    toks!("0x3.1_55p43", [Literal(Lit::Float {
        raw: "3.155".into(),
        is_double: true,
        radix: 16,
        exp: "43".into()
    })]);
    toks!("0x3.1p4f", [Literal(Lit::Float {
        raw: "3.1".into(),
        is_double: false,
        radix: 16,
        exp: "4".into()
    })]);

    toks!("0x.11p4", [Literal(Lit::Float {
        raw: ".11".into(),
        is_double: true,
        radix: 16,
        exp: "4".into()
    })]);
    toks!("0x.1p44f", [Literal(Lit::Float {
        raw: ".1".into(),
        is_double: false,
        radix: 16,
        exp: "44".into()
    })]);
}

#[test]
fn string_literals() {
    toks!(r#""hi""#, [Literal(Lit::Str("hi".into()))]);
    toks!(r#""hi \" bla""#, [Literal(Lit::Str("hi \" bla".into()))]);
    toks!(r#""\b \t \n \f \r \" \' \\""#, [Literal(Lit::Str(
        "\u{0008} \t \n \u{000c} \r \" \' \\".into()
    ))]);
    toks!(r#""\nn""#, [Literal(Lit::Str("\nn".into()))]);
    reals!(r#""a" "b""#, [
        Literal(Lit::Str("a".into())), Literal(Lit::Str("b".into()))
    ]);
    toks!(r#""\0 \00 \000 \0000 \377 \118 \42 \400""#, [Literal(Lit::Str(
        "\x00 \x00 \x00 \x000 \u{ff} \u{9}8 \u{22} \u{20}0".into()
    ))]);

    toks!(r"'a'", [Literal(Lit::Char('a'))]);
    reals!(r#"'\b' '\t' '\n' '\f' '\r' '\"' '\'' '\\'"#, [
        Literal(Lit::Char('\u{0008}')),
        Literal(Lit::Char('\t')),
        Literal(Lit::Char('\n')),
        Literal(Lit::Char('\u{000c}')),
        Literal(Lit::Char('\r')),
        Literal(Lit::Char('"')),
        Literal(Lit::Char('\'')),
        Literal(Lit::Char('\\'))
    ]);
    reals!(r"'a' 'b'", [Literal(Lit::Char('a')), Literal(Lit::Char('b'))]);
}

#[test]
fn unicode_escapes() {
    // correct
    assert_eq!(spans(r"z\u0078z"), vec![
        TokenSpan {
            tok: Ident("zxz".into()),
            span: Span::new(BytePos(0), BytePos(8))
        }
    ]);

    // too few hex digits
    let mut ra = raw_toks(r"z\u00z");
    assert_eq!(ra.len(), 1);
    let a = ra.remove(0).unwrap_err();   // has to be `Err`
    assert_eq!(a.poison, Some(TokenSpan {
        tok: Ident("zz".into()),
        span: Span::new(BytePos(0), BytePos(6))
    }));
    assert_eq!(a.report, diag::Report {
        kind: diag::ReportKind::Error,
        span: Span::new(BytePos(1), BytePos(5)),
        remarks: a.report.remarks.clone(), // don't care about messages
    });

    // value is not a valid unicode scalar
    let mut ra = raw_toks(r"z\udecez");
    assert_eq!(ra.len(), 1);
    let a = ra.remove(0).unwrap_err();   // has to be `Err`
    assert_eq!(a.poison, Some(TokenSpan {
        tok: Ident("zz".into()),
        span: Span::new(BytePos(0), BytePos(8))
    }));
    assert_eq!(a.report, diag::Report {
        kind: diag::ReportKind::Error,
        span: Span::new(BytePos(1), BytePos(7)),
        remarks: a.report.remarks.clone(), // don't care about messages
    });

    // correct with multiple 'u's
    assert_eq!(spans(r"z\uuuu0078z"), vec![
        TokenSpan {
            tok: Ident("zxz".into()),
            span: Span::new(BytePos(0), BytePos(11))
        }
    ]);
    // backslashes that are not eligible TODO
    // currently the lexer stops at backslash... enable this test again later!
    // assert_eq!(spans(r"z\\uuuu0078z"), vec![
    //     TokenSpan {
    //         tok: Ident(r"z\\uuuu0078z".into()),
    //         span: Span::new(BytePos(0), BytePos(12))
    //     }
    // ]);
}

#[test]
fn new_lines() {
    assert_eq!(spans("abc \n xyz"), vec![
        TokenSpan {
            tok: Ident("abc".into()),
            span: Span::new(BytePos(0), BytePos(3))
        },
        TokenSpan {
            tok: Whitespace,
            span: Span::new(BytePos(3), BytePos(6))
        },
        TokenSpan {
            tok: Ident("xyz".into()),
            span: Span::new(BytePos(6), BytePos(9))
        }
    ]);

    assert_eq!(spans("abc \r\n xyz"), vec![
        TokenSpan {
            tok: Ident("abc".into()),
            span: Span::new(BytePos(0), BytePos(3))
        },
        TokenSpan {
            tok: Whitespace,
            span: Span::new(BytePos(3), BytePos(7))
        },
        TokenSpan {
            tok: Ident("xyz".into()),
            span: Span::new(BytePos(7), BytePos(10))
        }
    ]);
}

#[test]
fn basic_spans() {
    assert_eq!(spans("abc xyz"), vec![
        TokenSpan {
            tok: Ident("abc".into()),
            span: Span::new(BytePos(0), BytePos(3))
        },
        TokenSpan {
            tok: Whitespace,
            span: Span::new(BytePos(3), BytePos(4))
        },
        TokenSpan {
            tok: Ident("xyz".into()),
            span: Span::new(BytePos(4), BytePos(7))
        }
    ]);

    assert_eq!(spans(".xxxx=="), vec![
        TokenSpan {
            tok: Dot,
            span: Span::new(BytePos(0), BytePos(1))
        },
        TokenSpan {
            tok: Ident("xxxx".into()),
            span: Span::new(BytePos(1), BytePos(5))
        },
        TokenSpan {
            tok: EqEq,
            span: Span::new(BytePos(5), BytePos(7))
        }
    ]);

    assert_eq!(spans("     !"), vec![
        TokenSpan {
            tok: Whitespace,
            span: Span::new(BytePos(0), BytePos(5))
        },
        TokenSpan {
            tok: Bang,
            span: Span::new(BytePos(5), BytePos(6))
        }
    ]);
}

#[test]
fn line_breaks() {
    use base::code::LineIdx;
    let src = "foo\nbar\r\nbaz";
    //         012334567788901
    let fmap = Rc::new(FileMap::new("<unit-test>", src));
    let mut lexer = Tokenizer::new(&fmap);

    // run through all token
    lexer.last();

    assert_eq!(fmap.get_line_start(LineIdx(0)), Some(BytePos(0)));
    assert_eq!(fmap.get_line_start(LineIdx(1)), Some(BytePos(4)));
    assert_eq!(fmap.get_line_start(LineIdx(2)), Some(BytePos(9)));

    assert_eq!(fmap.get_line_idx(BytePos(2)), LineIdx(0));
    assert_eq!(fmap.get_line_idx(BytePos(3)), LineIdx(0));
    assert_eq!(fmap.get_line_idx(BytePos(4)), LineIdx(1));
    assert_eq!(fmap.get_line_idx(BytePos(6)), LineIdx(1));
    assert_eq!(fmap.get_line_idx(BytePos(7)), LineIdx(1));
    assert_eq!(fmap.get_line_idx(BytePos(8)), LineIdx(1));
    assert_eq!(fmap.get_line_idx(BytePos(9)), LineIdx(2));
}
