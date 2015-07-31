use std::mem::replace;
use std::char;
use std::u32;
use std::error;
use std::fmt;
use std::ops::{Range, Index};

const OPEN_CHAR:    char = '[';
const CLOSE_CHAR:   char = ']';
const DIVIDER_CHAR: char = '|';
const ESCAPE_CHAR:  char = '\\';
const CONTROL_CHARS: [char; 4] = [OPEN_CHAR, CLOSE_CHAR, DIVIDER_CHAR, ESCAPE_CHAR];

const NULL_ESCAPE_CHAR:    char = '0';
const BYTE_ESCAPE_CHAR:    char = 'x';
const UNICODE_ESCAPE_CHAR: char = 'u';

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<S> {
    Open,
    Close,
    Divider,
    Comment(S),
    Escape(char),
    Null,
    Text(S),
    Whitespace(S),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct NoError;

impl error::Error for NoError {
    fn description(&self) -> &str { "no error" }
}

impl fmt::Display for NoError {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        (self as &error::Error).description().fmt(fmt)
    }
}

#[derive(Debug)]
pub enum Error<E: error::Error> {
    UnknownEscapeSequence(char),
    BadCharacterInCharacterEscape(char),
    InvalidCharacterEncoding(u64),
    UnclosedEscapeSequence,
    UnexpectedEOF,
    IoError(E),
}

impl <E: error::Error> PartialEq for Error<E> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&Error::UnknownEscapeSequence(ref a), &Error::UnknownEscapeSequence(ref b)) => a == b,
            (&Error::BadCharacterInCharacterEscape(ref a), &Error::BadCharacterInCharacterEscape(ref b)) => a == b,
            (&Error::InvalidCharacterEncoding(ref a), &Error::InvalidCharacterEncoding(ref b)) => a == b,
            (&Error::UnclosedEscapeSequence, &Error::UnclosedEscapeSequence) => true,
            (&Error::UnexpectedEOF, &Error::UnexpectedEOF) => true,
            _ => false,
        }
    }
}

impl <E: error::Error> error::Error for Error<E> {
    fn description(&self) -> &str {
        match *self {
            Error::UnknownEscapeSequence(_) => "unknown escape sequence",
            Error::BadCharacterInCharacterEscape(_) => "bad character in escape sequence",
            Error::InvalidCharacterEncoding(_) => "invalid character encoding",
            Error::UnclosedEscapeSequence => "escape sequence was unclosed",
            Error::UnexpectedEOF => "unexpected end of file",
            Error::IoError(ref e) => e.description(),
        }
    }
}

impl <E: error::Error> fmt::Display for Error<E> {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        if let Error::IoError(ref e) = *self {
            (e as &fmt::Display).fmt(fmt)
        } else {
            (self as &error::Error).description().fmt(fmt)
        }
    }
}

impl <E: error::Error> From<E> for Error<E> {
    fn from(err: E) -> Error<E> { Error::IoError(err) }
}

/*
 .o88b. db   db  .d8b.  d8888b.      d8888b. d88888b  .d8b.  d8888b. d88888b d8888b.
d8P  Y8 88   88 d8' `8b 88  `8D      88  `8D 88'     d8' `8b 88  `8D 88'     88  `8D
8P      88ooo88 88ooo88 88oobY'      88oobY' 88ooooo 88ooo88 88   88 88ooooo 88oobY'
8b      88~~~88 88~~~88 88`8b        88`8b   88~~~~~ 88~~~88 88   88 88~~~~~ 88`8b
Y8b  d8 88   88 88   88 88 `88.      88 `88. 88.     88   88 88  .8D 88.     88 `88.
 `Y88P' YP   YP YP   YP 88   YD      88   YD Y88888P YP   YP Y8888D' Y88888P 88   YD
*/

pub trait CharReader<S, E: error::Error> {
    type State;
    type Accumulator;

    fn next(&mut self) -> Option<Result<(char, Self::State), E>>;

    fn acc(&mut self) -> Self::Accumulator;
    fn push(a: &mut Self::Accumulator, cs: (char, Self::State));
    fn accumulate(a: Self::Accumulator) -> S;
}

struct TokenizerReader<S, E: error::Error, C: CharReader<S, E>> {
    iter: C,
    peek: Option<Result<(char, C::State), E>>,
}

impl <S, E: error::Error, C: CharReader<S, E>> TokenizerReader<S, E, C> {
    fn next(&mut self) -> Option<Result<(char, C::State), E>> {
        replace(&mut self.peek, None).or_else(|| self.iter.next())
    }

    fn reinsert(&mut self, prev: Result<(char, C::State), E>) {
        assert!(self.peek.is_none());
        self.peek = Some(prev);
    }

    fn acc(&mut self) -> C::Accumulator {
        self.iter.acc()
    }
}

/*
.d8888. d888888b d8888b. d888888b d8b   db  d888b        .o88b. db   db  .d8b.  d8888b.      d8888b. d88888b  .d8b.  d8888b. d88888b d8888b.
88'  YP `~~88~~' 88  `8D   `88'   888o  88 88' Y8b      d8P  Y8 88   88 d8' `8b 88  `8D      88  `8D 88'     d8' `8b 88  `8D 88'     88  `8D
`8bo.      88    88oobY'    88    88V8o 88 88           8P      88ooo88 88ooo88 88oobY'      88oobY' 88ooooo 88ooo88 88   88 88ooooo 88oobY'
  `Y8b.    88    88`8b      88    88 V8o88 88  ooo      8b      88~~~88 88~~~88 88`8b        88`8b   88~~~~~ 88~~~88 88   88 88~~~~~ 88`8b
db   8D    88    88 `88.   .88.   88  V888 88. ~8~      Y8b  d8 88   88 88   88 88 `88.      88 `88. 88.     88   88 88  .8D 88.     88 `88.
`8888Y'    YP    88   YD Y888888P VP   V8P  Y888P        `Y88P' YP   YP YP   YP 88   YD      88   YD Y88888P YP   YP Y8888D' Y88888P 88   YD
*/

pub struct StringCharReader<I> (I);

impl <I: Iterator<Item = char>> CharReader<String, NoError> for StringCharReader<I> {
    type State = ();
    type Accumulator = String;

    fn next(&mut self) -> Option<Result<(char, Self::State), NoError>> {
        self.0.next().and_then(|c| Some(Ok((c, ()))))
    }

    fn acc(&mut self) -> Self::Accumulator {
        String::new()
    }
    fn push(a: &mut Self::Accumulator, cs: (char, Self::State)) {
        a.push(cs.0)
    }
    fn accumulate(a: Self::Accumulator) -> String {
        a
    }
}

/*
.d8888. d888888b d8888b.      d888888b d8b   db d8888b. d88888b db    db       .o88b. db   db  .d8b.  d8888b.      d8888b. d88888b  .d8b.  d8888b. d88888b d8888b.
88'  YP `~~88~~' 88  `8D        `88'   888o  88 88  `8D 88'     `8b  d8'      d8P  Y8 88   88 d8' `8b 88  `8D      88  `8D 88'     d8' `8b 88  `8D 88'     88  `8D
`8bo.      88    88oobY'         88    88V8o 88 88   88 88ooooo  `8bd8'       8P      88ooo88 88ooo88 88oobY'      88oobY' 88ooooo 88ooo88 88   88 88ooooo 88oobY'
  `Y8b.    88    88`8b           88    88 V8o88 88   88 88~~~~~  .dPYb.       8b      88~~~88 88~~~88 88`8b        88`8b   88~~~~~ 88~~~88 88   88 88~~~~~ 88`8b
db   8D    88    88 `88.        .88.   88  V888 88  .8D 88.     .8P  Y8.      Y8b  d8 88   88 88   88 88 `88.      88 `88. 88.     88   88 88  .8D 88.     88 `88.
`8888Y'    YP    88   YD      Y888888P VP   V8P Y8888D' Y88888P YP    YP       `Y88P' YP   YP YP   YP 88   YD      88   YD Y88888P YP   YP Y8888D' Y88888P 88   YD
*/

pub struct StrIndexCharReader<'a, In: 'a + ?Sized, It> {
    i: &'a In,
    it: It,
}

impl <'a, In: 'a + ?Sized, It> StrIndexCharReader<'a, In, It> {
    pub fn new<F: Fn(&'a In)-> It>(i: &'a In, f: F) -> Self {
        StrIndexCharReader{i: i, it: f(i)}
    }
}

pub struct StrIndexCharAccumulator<'a, In: 'a + ?Sized, Idx=usize> {
    i: &'a In,
    idx: PartialRange<Idx>,
}

enum PartialRange<Idx> {
    None,
    Start(Idx),
    Full(Range<Idx>),
}

impl <'a, Idx: Clone, In: 'a + ?Sized + Index<Range<Idx>, Output=str>, It: Iterator<Item = (Idx, char)>> CharReader<&'a str, NoError> for StrIndexCharReader<'a, In, It> {
    type State = Idx;
    type Accumulator = StrIndexCharAccumulator<'a, In, Idx>;

    fn next(&mut self) -> Option<Result<(char, Self::State), NoError>> {
        self.it.next().and_then(|(i, c)| Some(Ok((c, i))))
    }

    fn acc(&mut self) -> Self::Accumulator {
        StrIndexCharAccumulator{
            i: self.i,
            idx: PartialRange::None,
        }
    }
    fn push(a: &mut Self::Accumulator, (_, s): (char, Self::State)) {
        a.idx = match replace(&mut a.idx, PartialRange::None) {
            PartialRange::None => PartialRange::Start(s),
            PartialRange::Start(idx) => PartialRange::Full(Range{start: idx, end: s}),
            PartialRange::Full(rg) => PartialRange::Full(Range{start: rg.start, end: s}),
        };
    }
    fn accumulate(a: Self::Accumulator) -> &'a str {
        let rg = match a.idx {
            PartialRange::None => return "",
            PartialRange::Start(idx) => {
                let end = idx.clone();
                Range{start: idx, end: end}
            },
            PartialRange::Full(rg) => rg,
        };

        &a.i[rg]
    }
}

/*
d888888b  .d88b.  db   dD d88888b d8b   db d888888b d88888D d88888b d8888b.
`~~88~~' .8P  Y8. 88 ,8P' 88'     888o  88   `88'   YP  d8' 88'     88  `8D
   88    88    88 88,8P   88ooooo 88V8o 88    88       d8'  88ooooo 88oobY'
   88    88    88 88`8b   88~~~~~ 88 V8o88    88      d8'   88~~~~~ 88`8b
   88    `8b  d8' 88 `88. 88.     88  V888   .88.    d8' db 88.     88 `88.
   YP     `Y88P'  YP   YD Y88888P VP   V8P Y888888P d88888P Y88888P 88   YD
*/

pub struct Tokenizer<S, E: error::Error, C: CharReader<S, E>> {
    reader: TokenizerReader<S, E, C>,
}

impl <S, E: error::Error, C: CharReader<S, E>> Tokenizer<S, E, C> {
    pub fn new(iter: C) -> Self {
        Tokenizer{
            reader: TokenizerReader{
                iter: iter,
                peek: None,
            },
        }
    }

    fn parse_comment(&mut self) -> Result<Token<S>, Error<E>> {
        let mut acc = self.reader.acc();

        loop {
            // Strip the Option<Result<>> cases first, so the rest is easier to read.
            let c = match self.reader.next() {
                None => return Err(Error::UnexpectedEOF),
                Some(Err(e)) => return Err(From::from(e)),
                Some(Ok(x)) => x,
            };

            // Check if the comment should end.
            let c = if let (CLOSE_CHAR, s) = c {
                // Peek to see if the ']' is followed by a '|'
                if let Some(x) = self.reader.next() {
                    if let Ok((DIVIDER_CHAR, _)) = x { break }
                    self.reader.reinsert(x);
                }
                (CLOSE_CHAR, s)
            } else {
                c
            };

            C::push(&mut acc, c);
        }

        Ok(Token::Comment(C::accumulate(acc)))
    }

    fn parse_escape_sequence(&mut self) -> Result<Token<S>, Error<E>> {
        // Strip the Option<Result<>> cases first, so the rest is easier to read.
        let c = match self.reader.next() {
            None => return Err(Error::UnexpectedEOF),
            Some(Err(e)) => return Err(From::from(e)),
            Some(Ok(x)) => x,
        };

        match c {
            (OPEN_CHAR, _) => Ok(Token::Escape(OPEN_CHAR)),
            (CLOSE_CHAR, _) => Ok(Token::Escape(CLOSE_CHAR)),
            (DIVIDER_CHAR, _) => Ok(Token::Escape(DIVIDER_CHAR)),
            (ESCAPE_CHAR, _) => Ok(Token::Escape(ESCAPE_CHAR)),
            (NULL_ESCAPE_CHAR, _) => Ok(Token::Null),
            (BYTE_ESCAPE_CHAR, _) => {
                let c1 = match self.reader.next() {
                    None => return Err(Error::UnexpectedEOF),
                    Some(Err(e)) => return Err(From::from(e)),
                    Some(Ok((c, _))) => try!(c.to_digit(16).ok_or(Error::BadCharacterInCharacterEscape(c))),
                };

                let c2 = match self.reader.next() {
                    None => return Err(Error::UnexpectedEOF),
                    Some(Err(e)) => return Err(From::from(e)),
                    Some(Ok((c, _))) => try!(c.to_digit(16).ok_or(Error::BadCharacterInCharacterEscape(c))),
                };

                let c = c1*16 + c2;
                let c = try!(char::from_u32(c).ok_or(Error::InvalidCharacterEncoding(c as u64)));
                Ok(Token::Escape(c))
            },
            (UNICODE_ESCAPE_CHAR, _) => {
                match self.reader.next() {
                    None => return Err(Error::UnexpectedEOF),
                    Some(Err(e)) => return Err(From::from(e)),
                    Some(Ok((OPEN_CHAR, _))) => {},
                    Some(Ok((c, _))) => return Err(Error::BadCharacterInCharacterEscape(c)),
                }
                let mut c = match self.reader.next() {
                    None => return Err(Error::UnexpectedEOF),
                    Some(Err(e)) => return Err(From::from(e)),
                    Some(Ok((c, _))) => try!(c.to_digit(16).ok_or(Error::BadCharacterInCharacterEscape(c))),
                } as u64;

                for _ in 0..6 {
                    c = c*16 + match self.reader.next() {
                        None => return Err(Error::UnexpectedEOF),
                        Some(Err(e)) => return Err(From::from(e)),
                        Some(Ok((CLOSE_CHAR, _))) => {
                            let c = try!(
                                if c > u32::MAX as u64 { None } else { Some(c as u32) }
                                .and_then(char::from_u32)
                                .ok_or(Error::InvalidCharacterEncoding(c))
                            );
                            return Ok(Token::Escape(c));
                        },
                        Some(Ok((c, _))) => try!(c.to_digit(16).ok_or(Error::BadCharacterInCharacterEscape(c))),
                    } as u64;
                }
                Err(Error::UnclosedEscapeSequence)
            }
            (c, _) => Err(Error::UnknownEscapeSequence(c)),
        }
    }
}

impl <S, E: error::Error, C: CharReader<S, E>> Iterator for Tokenizer<S, E, C> {
    type Item = Result<Token<S>, Error<E>>;

    fn next(&mut self) -> Option<Result<Token<S>, Error<E>>> {
        // Strip the Option<Result<>> cases first, so the rest is easier to read.
        let c = match self.reader.next() {
            None => return None,
            Some(Err(e)) => return Some(Err(From::from(e))),
            Some(Ok(x)) => x,
        };

        match c {
            // Check the control characters first
            (OPEN_CHAR, _) => Some(Ok(Token::Open)),
            (CLOSE_CHAR, _) => Some(Ok(Token::Close)),
            (ESCAPE_CHAR, _) => Some(self.parse_escape_sequence()),
            // Could be the start of a comment, so check that.
            (DIVIDER_CHAR, _) => match self.reader.next() {
                None => Some(Ok(Token::Divider)),
                Some(Ok((OPEN_CHAR, _))) => Some(self.parse_comment()),
                Some(x) => {
                    self.reader.reinsert(x);
                    Some(Ok(Token::Divider))
                },
            },
            // Otherwise it could either be whitespace or text
            (c, s) => {
                let mut is_whitespace = c.is_whitespace();

                let mut acc = self.reader.acc();
                C::push(&mut acc, (c, s));

                loop {
                    match self.reader.next() {
                        None => break,
                        Some(Err(e)) => return Some(Err(Error::IoError(e))),
                        Some(Ok((c, s))) => {
                            // If we found a control character, put it back and break.
                            if CONTROL_CHARS.contains(&c) {
                                self.reader.reinsert(Ok((c, s)));
                                break;
                            }
                            is_whitespace &= c.is_whitespace();
                            C::push(&mut acc, (c, s));
                        },
                    }
                }

                Some(Ok(if is_whitespace {
                    Token::Whitespace(C::accumulate(acc))
                } else {
                    Token::Text(C::accumulate(acc))
                }))
            },
        }
    }
}

/*
d888888b d88888b .d8888. d888888b
`~~88~~' 88'     88'  YP `~~88~~'
   88    88ooooo `8bo.      88
   88    88~~~~~   `Y8b.    88
   88    88.     db   8D    88
   YP    Y88888P `8888Y'    YP
*/

#[cfg(test)]
mod test {
    use std::error;
    use std::str::{Chars, CharIndices};
    use tokenizer::{Token, Error, Tokenizer, NoError, StringCharReader, StrIndexCharReader};

    fn test_tokenizer<S, E, I, J, F> (mut a: I, mut b: J, f: F)
    where S: PartialEq, E: error::Error,
    I: Iterator<Item = Result<Token<S>, Error<E>>>,
    J: Iterator<Item = Result<Token<S>, Error<E>>>,
    F: Fn(&Token<S>, &Token<S>) -> bool, {
        for i in a.by_ref().zip(b.by_ref()) {
            match i {
                (Err(a), Err(b)) => assert_eq!(a, b),
                (Err(_), Ok(_)) => assert!(false),
                (Ok(_), Err(_)) => assert!(false),
                (Ok(a), Ok(b)) => assert!(!f(&a, &b)),
            }
        }
        assert!(a.next().is_none());
        assert!(b.next().is_none());
    }

    fn new_tokenizer_1(s: &str) -> Tokenizer<String, NoError, StringCharReader<Chars>> {
        Tokenizer::new(StringCharReader(s.chars()))
    }

    fn new_tokenizer_2<'a>(s: &'a str) -> Tokenizer<&'a str, NoError, StrIndexCharReader<'a, str, CharIndices<'a>>> {
        Tokenizer::new(StrIndexCharReader::new(s, str::char_indices))
    }

    #[test]
    fn test_tokenizer_empty() {
        test_tokenizer(new_tokenizer_1(""), vec![].into_iter(), Token::eq);
        test_tokenizer(new_tokenizer_2(""), vec![].into_iter(), Token::eq);
    }

    #[test]
    fn test_tokenizer_simple() {
        //test_tokenizer(new_tokenizer_1("a"), vec![Ok(Token::Text("a".to_string()))].into_iter(), Token::eq);
        test_tokenizer(new_tokenizer_2("a"), vec![Ok(Token::Text("a"))].into_iter(), Token::eq);
    }
}
