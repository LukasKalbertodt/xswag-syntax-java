//! This module contains the lexing functionality
//!
//! The main type is the Tokenizer that will take the Java source code and
//! produces a sequence of tokens out of it.
//!
//! Most details of this module are specified in section 3 (lexical structure)
//! of the Java language specification.
//!
//! In some places (like octal escapes or float literals) it would have been
//! much easier to use a regex for lexing. However, to avoid the dependency
//! to a rather big crate, the functionality was programmed by hand. Using a
//! regex in the environment of the tokenizer would have been difficult anyway,
//! since it's using `bump()` to read new chars.
//!
//! If the tokenizer runs into a lexing error, it will print a message, but
//! continue to work like before. This implies that the tokenizer may produce
//! some so called poisoned tokens -- tokens that contain some kind of default
//! or artificial value. Any lexing error is recovered as best as it's
//! possible. This behaviour enables the parser to operate normally and the
//! tokenizer to report all lexing errors.
//!
//! This tokenizer is greedy in all cases and will consume as many chars as
//! possible. That behaviour is specified in section 3.2 with one exception:
//! In a type context, consecutive '>' chars should not be lexed as shift-right
//! or shift-right-unsigned, but as single greater-than tokens. Since the
//! tokenizer does not know anything about type contexts, it can't distinguish
//! between those tokens. This tokenizer will be greedy in that case, too. The
//! parser needs to handle incorrectly lexed shift-right's.
//!

// use diagnostics::ErrorHandler;
use base::code::{FileMap, Span, SrcOffset, BytePos};
use base::diag;
use std::iter::Iterator;
use std::str::Chars;
use std::str::FromStr;
use super::token::*;

/// Type returned by the Tokenizer in case of a lexing error. Contains a
/// detailed report and may contain poisoned token that could be used to
/// proceed normally.
#[derive(Clone, PartialEq, Eq, Debug)]
pub struct Error<P> {
    /// Detailed information about the error. Intended for user-friendly
    /// compiler messages.
    pub report: diag::Report,
    /// If an error occured but the lexer is able to recover, it emits a token.
    /// This token will probably contain some "wrong" information. For example:
    /// the java code `'\j'` contains a character literal with a wrong escape
    /// code. In this case the error contains a poisened token "Char(j)". So
    /// the backslash is ignored in order to get some semi usable token.
    pub poison: Option<P>,
}

impl<P> Error<P> {
    /// Returns a new version with a changed poison
    pub fn map_poison<F, U>(self, op: F) -> Error<U>
        where F: FnOnce(P) -> U
    {
        Error {
            report: self.report,
            poison: self.poison.map(op),
        }
    }

    /// Returns a new version with a changed report
    pub fn map_report<F>(self, op: F) -> Error<P>
        where F: FnOnce(diag::Report) -> diag::Report
    {
        Error {
            report: op(self.report),
            poison: self.poison,
        }
    }
}

/// Result with a possible `LexError`
pub type Result<T, P> = ::std::result::Result<T, Error<P>>;

fn map_both<T, U, F>(res: Result<T, T>, op: F) -> Result<U, U>
    where F: FnOnce(T) -> U
{
    match res {
        Ok(t) => Ok(op(t)),
        Err(Error { report: rep, poison: p }) =>
            Err(Error { report: rep, poison: p.map(op) }),
    }
}

/// Helper type for functions scanning string and char literals.
enum ScannedChar {
    Eof,    // EOF was reached
    Normal(char),   // standard char
    Escape(char),   // an escaped char
    InvalidEscape(char),    // invalid escape with fallback
}

/// The Java Tokenizer
///
/// This type takes the Java source code as string and produces a sequence of
/// `Token`s. It reads the source string from front to back only once. During
/// tokenization it will also detect newline characters and notifies the
/// filemap about them.
///
/// This type also implements the `Iterator` trait for convenience.
pub struct Tokenizer<'a> {
    /// Filemap containing the whole source code
    fmap: &'a FileMap,
    /// Iterator into the filemaps's source code to easily obtain chars
    chs: Chars<'a>,

    /// Buffered chars for easy access
    last: Option<char>,
    curr: Option<char>,
    peek: Option<char>,

    /// Byte offset of the corresponding char
    last_pos: BytePos,
    curr_pos: BytePos,
    peek_pos: BytePos,

    /// Byte offset when parsing the current token started
    token_start: BytePos,

    /// Used for translation of unicode escapes. Do not use directly!
    /// Note: It may be easier to use a `Peek` iterator to avoid manual peek
    /// handling. However, it's already written and works... whatever
    upeek: Option<char>,
    escaped_peek: u8,

    /// If a bump() encountered a problem, it's saved here
    bump_err: Option<diag::Report>,
}

impl<'a> Tokenizer<'a> {
    // =======================================================================
    // Public methods of the Tokenizer
    // =======================================================================
    /// Creates and initializes a new Tokenizer with a reference to a filemap.
    pub fn new(fmap: &'a FileMap) -> Tokenizer<'a> {
        let mut tok = Tokenizer {
            chs: fmap.src().chars(),
            fmap: fmap,
            last: None,
            curr: None,
            peek: None,
            last_pos: BytePos(0),
            curr_pos: BytePos(0),
            peek_pos: BytePos(0),
            token_start: BytePos(0),
            upeek: None,
            escaped_peek: 0,
            bump_err: None
        };
        tok.dbump();
        tok
    }

    /// Tells the tokenizer to fetch the next token from the source.
    ///
    /// The function returns...
    ///
    /// - `Ok(None)` if EOF was reached and no lexing error occured
    /// - `Ok(Some(_))` if there was a next token and no lexing error occured
    /// - `Err(_)` if a lexing error occured (invalid source code)
    ///
    /// The error value might contain a poisened token which can be used to
    /// proceed "normally". However, it is not advised to really proceed
    /// normally: usually lexing error results in an ill formed token stream,
    /// which in turn will result in parsing errors. These parsing errors
    /// will look strange to the user, since the tokens the parser works with
    /// are only recovered/poisened.
    pub fn next_token(&mut self) -> Option<Result<TokenSpan, TokenSpan>> {
        let res = self.next_token_inner();
        let out = match self.bump_err.take() {
            None => res,
            Some(rep) => Some(Err(Error {
                report: rep,
                poison: res.and_then(|r| r.map(|o| Some(o))
                                          .unwrap_or_else(|e| e.poison))
            }))
        };

        match out {
            Some(ref r) => trace!("Produced Token: {:?}", r),
            None => trace!("Produced None token!"),
        }
        out
    }

    // =======================================================================
    // Private helper methods
    // =======================================================================
    fn next_token_inner(&mut self) -> Option<Result<TokenSpan, TokenSpan>> {
        self.token_start = self.curr_pos;
        let p = self.peek.unwrap_or('\0');

        let curr = match self.curr {
            None => return None,
            Some(c) => c,
        };

        let res: Result<Token, Token> = match curr {
            // whitespace and comments
            c if is_java_whitespace(c) => {
                self.skip_whitespace();
                Ok(Token::Whitespace)
            },
            '/' if p == '/' || p == '*' => {
                match self.skip_comment() {
                    Some(r) => Err(Error {
                        report: r,
                        poison: Some(Token::Comment),
                    }),
                    None => Ok(Token::Comment),
                }
            },

            // Java separators, ':' and float literals
            '(' => { self.bump(); Ok(Token::ParenOp) },
            ')' => { self.bump(); Ok(Token::ParenCl) },
            '{' => { self.bump(); Ok(Token::BraceOp) },
            '}' => { self.bump(); Ok(Token::BraceCl) },
            '[' => { self.bump(); Ok(Token::BracketOp) },
            ']' => { self.bump(); Ok(Token::BracketCl) },
            ';' => { self.bump(); Ok(Token::Semi) },
            ',' => { self.bump(); Ok(Token::Comma) },
            '.' => {
                // it might be the start of a float literal
                match p {
                    '0' ... '9' => {
                        self.scan_number_literal()
                            .map(|l| Token::Literal(l))
                            .map_err(|e| Error {
                                report: e.report,
                                poison: None,
                            })
                    },
                    _ => {
                        self.bump();
                        if p == '.' && self.peek == Some('.') {
                            self.dbump();
                            Ok(Token::DotDotDot)
                        } else {
                            Ok(Token::Dot)
                        }
                    }
                }
            },
            '@' => { self.bump(); Ok(Token::At) },
            ':' if p == ':' => { self.dbump(); Ok(Token::ColonSep) },
            ':' => { self.bump(); Ok(Token::Colon) },

            // Operators  ==  =  >>>=  >>>  >>=  >>  >=  >  <<=  <<  <=  <
            '=' if p == '=' => { self.dbump(); Ok(Token::EqEq) },
            '=' => { self.bump(); Ok(Token::Eq) },
            '>' if p == '>' => {
                self.dbump();
                match self.curr.unwrap_or('\0') {
                    '>' => {
                        self.bump();
                        if self.curr == Some('=') {
                            self.bump();
                            Ok(Token::ShrUnEq)
                        } else {
                            Ok(Token::ShrUn)
                        }
                    },
                    '=' => {
                        self.bump();
                        Ok(Token::ShrEq)
                    }
                    _ =>  {
                        Ok(Token::Shr)
                    }
                }
            },
            '>' if p == '=' => { self.dbump(); Ok(Token::Ge) },
            '>' => { self.bump(); Ok(Token::Gt) },
            '<' if p == '<' => {
                self.dbump();
                if self.curr == Some('=') {
                    self.bump();
                    Ok(Token::ShlEq)
                } else {
                    Ok(Token::Shl)
                }
            },
            '<' if p == '=' => { self.dbump(); Ok(Token::Le) },
            '<' => { self.bump(); Ok(Token::Lt) },

            // Operators  !=  !  ~  ?
            '!' if p == '=' => { self.dbump(); Ok(Token::Ne) },
            '!' => { self.bump(); Ok(Token::Bang) },
            '~' => { self.bump(); Ok(Token::Tilde) },
            '?' => { self.bump(); Ok(Token::Question) },

            // Operators  +=  ++  +  -=  ->  --  -  &=  &&  &  |=  ||  |
            '+' if p == '=' => { self.dbump(); Ok(Token::PlusEq) },
            '+' if p == '+' => { self.dbump(); Ok(Token::PlusPlus) },
            '+' => { self.bump(); Ok(Token::Plus) },
            '-' if p == '=' => { self.dbump(); Ok(Token::MinusEq) },
            '-' if p == '>' => { self.dbump(); Ok(Token::Arrow) },
            '-' if p == '-' => { self.dbump(); Ok(Token::MinusMinus) },
            '-' => { self.bump(); Ok(Token::Minus) },
            '&' if p == '=' => { self.dbump(); Ok(Token::AndEq) },
            '&' if p == '&' => { self.dbump(); Ok(Token::AndAnd) },
            '&' => { self.bump(); Ok(Token::And) },
            '|' if p == '=' => { self.dbump(); Ok(Token::OrEq) },
            '|' if p == '|' => { self.dbump(); Ok(Token::OrOr) },
            '|' => { self.bump(); Ok(Token::Or) },

            // Operators  *=  *  /=  /  %=  %  ^=  ^
            '*' if p == '=' => { self.dbump(); Ok(Token::StarEq) },
            '*' => { self.bump(); Ok(Token::Star) },
            '/' if p == '=' => { self.dbump(); Ok(Token::SlashEq) },
            '/' => { self.bump(); Ok(Token::Slash) },
            '^' if p == '=' => { self.dbump(); Ok(Token::CaretEq) },
            '^' => { self.bump(); Ok(Token::Caret) },
            '%' if p == '=' => { self.dbump(); Ok(Token::PercentEq) },
            '%' => { self.bump(); Ok(Token::Percent) },

            // Literals
            '"' => {
                map_both(self.scan_string_literal(),
                    |s| Token::Literal(Lit::Str(s)))
            },
            '\'' => {
                map_both(self.scan_char_literal(),
                    |c| Token::Literal(Lit::Char(c)))
            },
            '0' ... '9' => self.scan_number_literal()
                               .map(|l| Token::Literal(l))
                               .map_err(|e| Error {
                                    report: e.report,
                                    poison: None,
                                }),

            // identifier, keyword, bool- or null-literal
            c if is_java_ident_start(c) => Ok(self.scan_word()),

            // If you reach this: congratz!
            _ => {
                return Some(Err(Error {
                    report: diag::Report::simple_error(
                        "illegal character in this context",
                        self.curr_span()
                    ),
                    poison: None,
                }))
            },
        };

        let add_span = |tok| TokenSpan {
            tok: tok,
            span: self.curr_span(),
        };

        Some(res.map(|t| add_span(t))
              .map_err(|e| e.map_poison(|tok| add_span(tok))))
    }

    /// Reads a new char from the iterator, updating last, curr and peek + pos.
    /// Also handles line breaks and unicode escapes.
    fn bump(&mut self) {
        self.last = self.curr;
        self.curr = self.peek;

        // If we already peeked once more to handle unicode escapes, use that
        // value instead of calling `next` already.
        self.peek = if let Some(un) = self.upeek {
            self.upeek = None;
            Some(un)
        } else {
            self.chs.next()
        };

        // update byte offsets
        self.last_pos = self.curr_pos;
        self.curr_pos = self.peek_pos;
        if let Some(c) = self.curr {
            // if peek was created from a unicode escape sequence, we add the
            // escape byte offset instead of the utf8 len
            self.peek_pos = self.peek_pos + if self.escaped_peek > 0 {
                BytePos(self.escaped_peek as SrcOffset)
            } else {
                BytePos(c.len_utf8() as SrcOffset)
            };
            self.escaped_peek = 0;
        }

        // Check if the current char is a line break (end) and add to line
        // break list.
        // Division into lines specified in section 3.4
        if self.curr == Some('\n')
            || (self.curr == Some('\r') && self.peek != Some('\n'))
        {
            self.fmap.add_line(self.peek_pos);
        }

        // Check for unicode escape. More information in section 3.3
        // Warning: Crazy code ahead
        if self.peek == Some('\\') && self.curr != Some('\\') {
            self.upeek = self.chs.next();
            if self.upeek == Some('u') {
                // First of all: We need `upeek` just for the case that the
                // char after '\' is not 'u'. So we reset it here.
                self.upeek = None;

                // we already read \ and u
                let mut pos_offset = 2;

                // We use a temporary peekable iterator here to check for
                // additional u's
                let mut peekiter = self.chs.by_ref().peekable();

                // After the first 'u' may follow arbitrarily many more 'u's...
                while let Some(&'u') = peekiter.peek() {
                    pos_offset += 1;
                    peekiter.next();
                }

                // At this point we expect 4 hexadecimal digits. Try to read
                // all four and convert them to int. We count the digits we
                // read in `num_digits`. If a non-hexadecimal char is found,
                // it'll be saved in `interrupt`.
                let mut value = 0;
                let mut num_digits = 0;
                let mut interrupt = None;
                for c in peekiter.by_ref().take(4) {
                    match c.to_digit(16) {
                        Some(v) => {
                            // converting: Shifting left by 12, 8, 4 and 0.
                            value += v << ((3-num_digits)*4);
                            num_digits += 1;
                        },
                        None => {
                            interrupt = Some(c);
                            break;
                        },
                    }
                }

                // check if all four digits were supplied
                if num_digits < 4 {
                    // report error ...
                    self.bump_err = Some(diag::Report::simple_error(
                        "Invalid unicode escape (less than 4 digits)",
                        Span {
                            lo: self.peek_pos,
                            hi: self.peek_pos +
                                    BytePos(pos_offset + num_digits),
                        }
                    ));
                    // ... but ignore the wrong unicode escape (POISON).
                    // If we couldn't read 4 digits because we reached EOF,
                    // interrupt will be None. If the reason was a non-hex
                    // char, it's saved in interrupt.
                    self.peek = interrupt;

                    // update position accordingly
                    self.peek_pos.0 += pos_offset + num_digits;
                } else {
                    // We read all 4 digits and converted them to int. Now use
                    // that value to create a new char and save it into peek.
                    self.peek = match ::std::char::from_u32(value) {
                        Some(c) => {
                            self.escaped_peek = 4 + pos_offset as u8;
                            Some(c)
                        },
                        None => {
                            self.bump_err = Some(diag::Report::simple_error(
                                "Invalid unicode escape (not a valid unicode \
                                    scalar value)",
                                Span {
                                    lo: self.peek_pos,
                                    hi: self.peek_pos + BytePos(pos_offset + 4),
                                }
                            ));
                            self.peek_pos.0 += pos_offset + 4;
                            peekiter.next()
                        }
                    };
                }
            }
        }

        // Check if peek is the SUB character to ignore it, if it is the last
        // character in the input stream (section 3.5)
        if self.peek == Some('\u{001a}') {
            self.upeek = self.chs.next();

            // If peek is the last char in the input stream, we ignore the
            // SUB character
            if self.upeek == None {
                self.peek = None
            }
        }
    }

    /// Calls `bump` twice. Being lazy sometimes...
    fn dbump(&mut self) {
        self.bump();
        self.bump();
    }

    fn curr_span(&self) -> Span {
        Span::new(self.token_start, self.curr_pos)
    }

    fn simple_error<P, S: Into<String>>(&self, poison: P, msg: S) -> Error<P> {
        Error {
            report: diag::Report::simple_error(msg, self.curr_span()),
            poison: Some(poison),
        }
    }

    // =======================================================================
    // Private skip and scan methods
    // =======================================================================
    /// Calls `bump` until the first non-whitespace char or EOF is reached.
    fn skip_whitespace(&mut self) {
        while is_java_whitespace(self.curr.unwrap_or('x')) {
            self.bump();
        }
    }

    /// Skips `/* */` and `//` comments. Calls `bump` until the first char
    /// after the comment is reached. Section 3.7
    ///
    /// ## Preconditions
    /// `curr` needs to be '/' and `peek` needs to be one of '*' and '/'
    fn skip_comment(&mut self) -> Option<diag::Report> {
        // Note: `self.peek.is_some()` implies `self.curr.is_some()`
        if self.peek == Some('*') {
            // Skip "/*". Otherwise "/*/" would be lexed as a comment
            self.dbump();
            // Skip everything until the end of file or a "*/" is reached.
            loop {
                match (self.curr, self.peek) {
                    (Some('*'), Some('/')) => break,
                    (_, None) => {
                        self.dbump();
                        return Some(diag::Report::simple_error(
                            "unclosed comment block",
                            self.curr_span()
                        ));
                    }
                    _ => self.bump(),
                }
            }
            self.dbump();   // skip last two chars
        } else {
            // precondition tells us that `peek` is '/' here. Skip everything
            // until line break is reached.
            loop {
                match self.curr {
                    None | Some('\n') | Some('\r') => break,
                    _ => self.bump(),
                }
            }
        }
        None
    }

    /// Scans a word and returns it as a `String`.
    ///
    /// This function scans through the source text as if we expect a Java
    /// identifier (Section 3.8). However, it might turn out to be a keyword,
    /// a boolean literal or a null literal.
    ///
    /// ## Preconditions
    /// `curr` needs to be a Java identifier start.
    fn scan_word(&mut self) -> Token {
        // Collect all chars until the first non-ident char or EOF is reached.
        let mut s = String::new();
        loop {
            match self.curr {
                Some(c) if is_java_ident_part(c) => {
                    s.push(c);
                    self.bump();
                },
                _ => break,
            }
        }

        // check if the word is a keyword or literal
        match &s[..] {
            "true" => Token::Literal(Lit::Bool(true)),
            "false" => Token::Literal(Lit::Bool(false)),
            "null" => Token::Literal(Lit::Null),
            _ => match Keyword::from_str(&s) {
                Ok(k) => Token::KeyW(k),
                Err(_) => Token::Ident(s),
            },
        }
    }

    /// Reads a char inside a string or character literal. If `curr` is a
    /// backslash, the escape character is parsed, if possible. The boolean
    /// value denotes if the returned char was created from a escape sequence.
    ///
    /// Returns `None` if
    /// - `curr` is `None` (EOF),
    /// - a '\' followed by EOF is found
    ///
    /// Invalid escape sequences result in an error, but the backslash will be
    /// ignored and the char after it will be returned.
    fn scan_escaped_char(&mut self) -> ScannedChar {
        match self.curr {
            Some('\\') => {
                self.bump();
                let out = match self.curr {
                    None => return ScannedChar::Eof,
                    Some(c) => {
                        self.bump();
                        match c {
                            'b' => '\u{0008}',
                            't' => '\t',
                            'n' => '\n',
                            'f' => '\u{000c}',
                            'r' => '\r',
                            '\'' => '\'',
                            '\"' => '\"',
                            '\\' => '\\',
                            '0' ... '7' => self.scan_octal_escape(),
                            _ => return ScannedChar::InvalidEscape(c),
                        }
                    },
                };
                ScannedChar::Escape(out)
            },
            Some(c) => {
                self.bump();
                ScannedChar::Normal(c)
            },
            None => ScannedChar::Eof,
        }
    }

    /// Scans 1 to 3 digits from an octal escape sequence and returns the char
    /// represented by the escape code.
    ///
    /// ## Preconditions
    /// `last` needs to be '0' ... '7'
    fn scan_octal_escape(&mut self) -> char {
        // We are allowed to unwrap here (precondition)
        let mut val = self.last.unwrap().to_digit(8).unwrap();

        if let Some(c) = self.curr.and_then(|c| c.to_digit(8)) {
            self.bump();
            val = val * 8 + c;
        }

        // only values below 0o400 are allowed (0x100)
        if val < 0o40 {
            if let Some(c) = self.curr.and_then(|c| c.to_digit(8)) {
                self.bump();
                val = val * 8 + c;
            }
        }

        // We can unwrap here, because all possible values from 0 to 0o377 are
        // a valid unicode code point
        ::std::char::from_u32(val).unwrap()
    }

    /// Scans a Java string literal.
    ///
    /// ## Preconditions
    /// `curr` needs to be `"`
    fn scan_string_literal(&mut self) -> Result<String, String> {
        self.bump();    // discard `"`

        let mut s = String::new();
        loop {
            match self.scan_escaped_char() {
                ScannedChar::Normal('\"') => break,
                ScannedChar::Normal(c) | ScannedChar::Escape(c) => s.push(c),
                ScannedChar::InvalidEscape(c) => {
                    s.push(c);
                    let e = self.simple_error(s,
                        format!("invalid escape character `\\{}`", c)
                    ).map_report(|r| r.with_note("valid escape characters are \
                        \\b \\t \\n \\f \\r \\\" \\' \\\\ or octal escapes"
                    ));
                    return Err(e);
                },
                ScannedChar::Eof => return Err(self.simple_error(s,
                                        "unexpected EOF in string literal")),
            }
        }

        Ok(s)
    }

    /// Scans a Java string literal.
    ///
    /// ## Preconditions
    /// `curr` needs to be `'`
    fn scan_char_literal(&mut self) -> Result<char, char> {
        self.bump();    // discard '
        match self.scan_escaped_char() {
            ScannedChar::Normal('\'') => {
                Err(self.simple_error('\0', "empty character literal")
                        .map_report(|r| r.with_note("maybe you want to use an \
                            empty string `\"\"` instead?")))
            },
            ScannedChar::Normal(c) | ScannedChar::Escape(c) => {
                // we need another ' to close the literal
                if self.curr == Some('\'') {
                    self.bump();
                    Ok(c)
                } else {
                    Err(self.simple_error('\0', "unclosed character literal"))
                }
            },
            ScannedChar::InvalidEscape(c) => {
                Err(self.simple_error(c,
                        format!("invalid escape character `\\{}`", c)
                    ).map_report(|r| r.with_note("valid escape characters are \
                        \\b \\t \\n \\f \\r \\\" \\' \\\\ or octal escapes"
                    ))
                )
            },
            ScannedChar::Eof => {
                Err(self.simple_error('\0',
                    "unexpected EOF in character literal"))
            }
        }
    }

    /// Scans a Java number literal and returns it. The literal may be a float
    /// or a integer literal. See section 3.10.1 and 3.10.2.
    ///
    /// Parsing the string as a number could be done at this point in theory.
    /// I need to think about it to find out if it makes sense.
    ///
    /// ## Preconditions
    /// `curr` needs to be in '0' ... '9' or a '.' followed by '0' ... '9'
    fn scan_number_literal(&mut self) -> Result<Lit, ()> {
        // First: lex the whole number part (maybe the only part)
        let (r, s) = match self.curr {
            // if literal is starting with '0' (-> not decimal or single digit)
            Some('0') => {
                match self.peek.unwrap_or('\0') {
                    // hexadecimal literal
                    'x' | 'X' => {
                        self.dbump();   // skip "0x"
                        (16, if self.curr != Some('.') {
                            // will never contain a Report with radix 16
                            try!(self.scan_digits(16))
                        } else {
                            "".into()
                        })
                    },
                    // binary literal
                    'b' | 'B' => {
                        self.dbump();   // skip "0b"
                        (2, try!(self.scan_digits(2)))
                    },
                    // octal literal
                    '0' ... '9' => {
                        self.bump();   // skip "0"
                        (8, try!(self.scan_digits(8)))
                    },
                    // just a '0'
                    _ => {
                        self.bump();
                        (10, "0".into())
                    }
                }
            },
            // literal starting with a dot: decimal float. Note: No bump
            Some('.') => (10, "".into()),
            // literal starting with non-null digit (-> decimal)
            // will never contain a Report with radix 10
            _ => (10, try!(self.scan_digits(10)))
        };

        // look at the first char after the whole number part
        match self.curr.unwrap_or('\0') {
            'l' | 'L' => {
                self.bump();
                Ok(Lit::Integer { raw: s, is_long: true, radix: r as u8 })
            },
            // After a whole number part may follow a float type suffix
            c @ 'f' | c @ 'F' | c @ 'd' | c @ 'D' if !s.is_empty() => {
                self.bump();
                Ok(Lit::Float {
                    raw: s,
                    is_double: (c != 'f' && c != 'F'),
                    radix: r as u8,
                    exp: "".into(),
                })
            },
            // If we have a whole number part, there may follow an exponent part
            'p' | 'P' | 'e' | 'E' if !s.is_empty() => {
                match self.scan_float_exp(r == 16) {
                    // Failing to scan the exponent means that the exponent
                    // char is wrong (p for hex, e for decimal)
                    None => {
                        Err(if r == 16 {
                            self.simple_error((), "invalid exponent indicator \
                                for hex float literal (use 'p' or 'P' instead")
                        } else {
                            self.simple_error((), "invalid exponent indicator \
                                for decimal float literal (use 'e' or 'E' \
                                instead)")
                        })
                    },
                    Some(ex) => {
                        // A float type suffix may follow
                        let double = self.scan_double_suffix().unwrap_or(true);

                        Ok(Lit::Float {
                            raw: s,
                            is_double: double,
                            radix: r as u8,
                            exp: ex,
                        })
                    }
                }
            }

            // A dot means float literal and may occur in two situations:
            // - we already read a whole number part
            // - the dot was the start of the literal
            '.' => {
                // make sure the literal is in the right base
                if r != 10 && r != 16 {
                    return Err(self.simple_error((), format!("float literals \
                        may only be expressed in decimal or hexadecimal, not \
                        base {}", r)));
                }
                self.bump();    // dot

                // We do not need to check if both, the whole number and
                // fraction part, are empty in decimal case for the
                // following reason:
                // The precondition tells us that this function is only
                // called if `curr` is a number OR a dot followed by a
                // number. This guarantees that at least one part is
                // non-empty in decimal case.
                let fraction = try!(self.scan_digits(r));
                if r == 16 && s.is_empty() && fraction.is_empty() {
                    return Err(self.simple_error((), "hex float literals need \
                        either a whole number or a fraction part"));
                }

                // In a hexadecimal float literal the exponent is required
                let exp = self.scan_float_exp(r == 16).unwrap_or("".into());
                if r == 16 && exp.is_empty() {
                    return Err(self.simple_error((), "hex float literals are \
                        required to have an exponent"));
                }

                // type suffix is always optional
                let is_double = self.scan_double_suffix().unwrap_or(true);
                Ok(Lit::Float {
                    raw: format!("{}.{}", s, fraction),
                    is_double: is_double,
                    radix: r as u8,
                    exp: exp,
                })
            },
            _ => Ok(Lit::Integer { raw: s, is_long: false, radix: r as u8 }),
        }
    }

    /// Scans a float suffix ('d' or 'f') if present and returns if the
    /// suffix was a 'd' (double) suffix.
    fn scan_double_suffix(&mut self) -> Option<bool> {
        match self.curr.unwrap_or('\0') {
            'd' | 'D' => { self.bump(); Some(true) },
            'f' | 'F' => { self.bump(); Some(false) },
            _ => None
        }
    }

    /// Scans a float exponent
    fn scan_float_exp(&mut self, hex: bool) -> Option<String> {
        match (hex, self.curr.unwrap_or('\0')) {
            (false, 'e') | (false, 'E') | (true, 'p') | (true, 'P') => {
                self.bump();
                Some(if self.curr == Some('-') {
                    self.bump();
                    // we can unwrap here: no error in scan_digits with r >= 10
                    format!("-{}", self.scan_digits(10).unwrap())
                } else {
                    // we can unwrap: no errors in scan_digits with r >= 10
                    self.scan_digits(10).unwrap()
                })
            },
            _ => None,
        }
    }

    /// Scans digits with the given radix and returns the scanned string.
    ///
    /// The parsing will skip underscores and will stop when a character is
    /// found, that is no digit in the given radix. However, if the radix is
    /// less than 10, all digits from 0 to 9 are scanned and an error is
    /// printed for each digit that is too high for the given radix.
    fn scan_digits(&mut self, radix: u32) -> Result<String, ()> {
        // We possibly scan more digits to report smart errors
        let scan_radix = if radix <= 10 { 10 } else { radix };

        // Prepare report for reporting possible radix errors
        let mut rep = diag::Report {
            kind: diag::ReportKind::Error,
            span: None,
            remarks: vec![],
        };

        let mut s = String::new();
        loop {
            match self.curr.unwrap_or('\0') {
                // skip underscores
                '_' => {
                    self.bump();
                    continue;
                },
                c if c.to_digit(scan_radix).is_some() => {
                    // check if the digit is valid in the given radix
                    // TODO: Maybe stop lexing here
                    if c.to_digit(radix).is_none() {
                        rep.remarks.push(diag::Remark::error(
                            format!("Invalid digit for base{} literal", radix),
                            diag::Snippet::Orig(Span::single(self.curr_pos)),
                        ));
                    }
                    s.push(c);
                    self.bump();
                }
                _ => break,
            }
        }
        if rep.remarks.len() > 0 {
            // Set span for main "Report"
            // We can unwrap everywhere, because the span is always set and we
            // know that we have at least one remark.
            let lo = rep.remarks.iter().map(|rem|
                rem.snippet.span().unwrap().lo
            ).min();
            let hi = rep.remarks.iter().map(|rem|
                rem.snippet.span().unwrap().hi
            ).min();
            rep.span = Some(Span::new(lo.unwrap(), hi.unwrap()));

            Err(Error {
                report: rep,
                poison: None,
            })
        } else {
            Ok(s)
        }
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Result<TokenSpan, TokenSpan>;

    fn next(&mut self) -> Option<Result<TokenSpan, TokenSpan>> {
        self.next_token()
    }
}

// ===========================================================================
// A bunch of helper functions
// ===========================================================================

/// Determines if the given character is a valid Java identifier start char
/// (`JavaLetter`, section 3.8.).
///
/// This function does not match the exact definition! However, it returns the
/// same values for all chars from `\u{0}` up to at least `\x{400}` as the
/// function `isJavaIdentifierStart` from "openjdk8".
fn is_java_ident_start(c: char) -> bool {
    match c {
        '$' | '_' | '¢' | '£' | '¤' | '¥' => true,
        '\u{345}' | '\u{37f}' => false,
        _ => c.is_alphabetic(),
    }
}

/// Determines if the given character is a valid Java identifier char
/// (`JavaLetterOrDigit`, section 3.8.).
///
/// This function does not match the exact definition! However, it returns the
/// same values for all chars from `\u{0}` up to at least `\x{400}` as the
/// function `isJavaIdentifierPart` from "openjdk8".
fn is_java_ident_part(c: char) -> bool {
    match c {
        '\u{ad}'
            | '\u{000}' ... '\u{008}'
            | '\u{00e}' ... '\u{01b}'
            | '\u{07f}' ... '\u{09f}'
            | '\u{300}' ... '\u{374}' => true,
        '\u{37f}' => false,
        _ => c.is_numeric() || is_java_ident_start(c),
    }
}

/// Determines if the given character is a whitespace character as defined
/// in section 3.6
fn is_java_whitespace(c: char) -> bool {
    match c {
        ' ' | '\t' | '\u{000c}' | '\n' | '\r' => true,
        _ => false,
    }
}
