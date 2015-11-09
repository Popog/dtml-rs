use std::mem::replace;
use std::char;
use std::u32;
use std::error;
use std::fmt;
use std::iter::Step;
use std::num::One;
use std::ops::{Range, Index};

use token::TokenWithComments;


const OPEN_CHAR:    char = '[';
const CLOSE_CHAR:   char = ']';
const DIVIDER_CHAR: char = '|';
const ESCAPE_CHAR:  char = '\\';
const COMMENT_CHAR: char = '#';
const CONTROL_CHARS: [char; 5] = [OPEN_CHAR, CLOSE_CHAR, DIVIDER_CHAR, ESCAPE_CHAR, COMMENT_CHAR];

const NULL_ESCAPE_CHAR:    char = '0';
const BYTE_ESCAPE_CHAR:    char = 'x';
const UNICODE_ESCAPE_CHAR: char = 'u';
const LINE_FEED_ESCAPE_CHAR: char = 'n';
const CARRIAGE_RETURN_ESCAPE_CHAR: char = 'r';
const HORIZONTAL_TAB_ESCAPE_CHAR: char = 't';

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


fn map_error<C, E: error::Error>(r: Option<Result<C, E>>) -> Result<C, Error<E>> {
    match r {
        None => return Err(Error::UnexpectedEOF),
        Some(Err(e)) => return Err(From::from(e)),
        Some(Ok(t)) => Ok(t),
    }
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

impl <S, E: error::Error, C: CharReader<S, E>> Iterator for TokenizerReader<S, E, C> {
    type Item = Result<(char, C::State), E>;

    fn next(&mut self) -> Option<Result<(char, C::State), E>> {
        replace(&mut self.peek, None).or_else(|| self.iter.next())
    }
}

impl <S, E: error::Error, C: CharReader<S, E>> TokenizerReader<S, E, C> {
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

impl <I: Iterator<Item = char>> StringCharReader<I> {
    pub fn new(i: I) -> Self { StringCharReader(i) }
}

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

impl <'a, Idx: Clone+Step+One, In: 'a + ?Sized + Index<Range<Idx>, Output=str>, It: Iterator<Item = (Idx, char)>> StrIndexCharReader<'a, In, It> {
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

impl <'a, Idx: Clone+Step+One, In: 'a + ?Sized + Index<Range<Idx>, Output=str>, It: Iterator<Item = (Idx, char)>> CharReader<&'a str, NoError> for StrIndexCharReader<'a, In, It> {
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
        let (start, end) = match a.idx {
            PartialRange::None => return "",
            PartialRange::Start(idx) => {
                let end = idx.step(&One::one()).unwrap_or_else(||idx.clone());
                (idx, end)
            },
            PartialRange::Full(rg) => (rg.start, rg.end.step(&One::one()).unwrap_or(rg.end)),
        };

        &a.i[Range{start: start, end: end}]
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

pub struct Tokenizer<S: Eq, E: error::Error, C: CharReader<S, E>> {
    reader: TokenizerReader<S, E, C>,
}

impl <S: Eq, E: error::Error, C: CharReader<S, E>> Tokenizer<S, E, C> {
    pub fn new(iter: C) -> Self {
        Tokenizer{
            reader: TokenizerReader{
                iter: iter,
                peek: None,
            },
        }
    }

    fn parse_block_comment(&mut self, mut acc: C::Accumulator) -> Result<TokenWithComments<S>, Error<E>> {
        enum State {
            Default,
            Closing,
            Opening,
        }

        let mut state = State::Default;
        let mut nesting_count = 1usize;

        loop {
            let (c, s) = try!(map_error(self.reader.next()));
            C::push(&mut acc, (c.clone(), s));

            state = match (state, c) {
                // Default
                (State::Default, CLOSE_CHAR)   => State::Closing,
                (State::Default, COMMENT_CHAR) => State::Opening,
                (State::Default, _)            => State::Default,

                // Closing
                (State::Closing, CLOSE_CHAR) => State::Closing,
                (State::Closing, COMMENT_CHAR) => {
                    nesting_count -= 1;
                    if nesting_count == 0 {
                        return Ok(TokenWithComments::Comment(true, C::accumulate(acc)));
                    }
                    State::Default
                },
                (State::Closing, _) => State::Default,

                // Opening
                (State::Opening, CLOSE_CHAR) => State::Closing,
                (State::Opening, COMMENT_CHAR) => State::Opening,
                (State::Opening, OPEN_CHAR) => {
                    nesting_count += 1;
                    State::Default
                },
                (State::Opening, _) => State::Default,
            }
        }
    }

    fn parse_comment(&mut self, s: C::State) -> Result<TokenWithComments<S>, Error<E>> {
        let mut acc = self.reader.acc();
        C::push(&mut acc, (COMMENT_CHAR, s));

        let (c, s) = try!(map_error(self.reader.next()));
        C::push(&mut acc, (c.clone(), s));

        // Check if this is a block comment
        if c == OPEN_CHAR {
            return self.parse_block_comment(acc);
        }

        loop {
            let (c, s) = try!(map_error(self.reader.next()));
            C::push(&mut acc, (c.clone(), s));

            if c == '\n' {
                return Ok(TokenWithComments::Comment(false, C::accumulate(acc)));
            }
        }
    }

    fn parse_escape_sequence(&mut self) -> Result<TokenWithComments<S>, Error<E>> {
        // Strip the Option<Result<>> cases first, so the rest is easier to read.
        let c = match self.reader.next() {
            None => return Err(Error::UnexpectedEOF),
            Some(Err(e)) => return Err(From::from(e)),
            Some(Ok(x)) => x,
        };

        match c {
            (OPEN_CHAR, _) => Ok(TokenWithComments::Escape(OPEN_CHAR)),
            (CLOSE_CHAR, _) => Ok(TokenWithComments::Escape(CLOSE_CHAR)),
            (DIVIDER_CHAR, _) => Ok(TokenWithComments::Escape(DIVIDER_CHAR)),
            (ESCAPE_CHAR, _) => Ok(TokenWithComments::Escape(ESCAPE_CHAR)),
            (COMMENT_CHAR, _) => Ok(TokenWithComments::Escape(COMMENT_CHAR)),
            (LINE_FEED_ESCAPE_CHAR, _) => Ok(TokenWithComments::Escape('\n')),
            (CARRIAGE_RETURN_ESCAPE_CHAR, _) => Ok(TokenWithComments::Escape('\r')),
            (HORIZONTAL_TAB_ESCAPE_CHAR, _) => Ok(TokenWithComments::Escape('\t')),
            (NULL_ESCAPE_CHAR, _) => Ok(TokenWithComments::Null),
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
                Ok(TokenWithComments::Escape(c))
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
                            return Ok(TokenWithComments::Escape(c));
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

impl <S: Eq, E: error::Error, C: CharReader<S, E>> Iterator for Tokenizer<S, E, C> {
    type Item = Result<TokenWithComments<S>, Error<E>>;

    fn next(&mut self) -> Option<Result<TokenWithComments<S>, Error<E>>> {
        // Strip the Option<Result<>> cases first, so the rest is easier to read.
        let c = match self.reader.next() {
            None => return None,
            Some(Err(e)) => return Some(Err(From::from(e))),
            Some(Ok(x)) => x,
        };

        match c {
            // Check the control characters first
            (OPEN_CHAR, _) => Some(Ok(TokenWithComments::Open)),
            (CLOSE_CHAR, _) => Some(Ok(TokenWithComments::Close)),
            (ESCAPE_CHAR, _) => Some(self.parse_escape_sequence()),
            (DIVIDER_CHAR, _) => Some(Ok(TokenWithComments::Divider)),
            (COMMENT_CHAR, s) => Some(self.parse_comment(s)),
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
                    TokenWithComments::Whitespace(C::accumulate(acc))
                } else {
                    TokenWithComments::Text(C::accumulate(acc))
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
    use std::fmt::Debug;
    use std::error;
    use std::str::{Chars, CharIndices};
    use tokenizer::{Error, Tokenizer, NoError, StringCharReader, StrIndexCharReader};
    use token::{TokenWithComments};

    fn test_tokenizer<S, E, I, J> (mut a: I, mut b: J)
    where S: Eq+Debug, E: error::Error,
    I: Iterator<Item = Result<TokenWithComments<S>, Error<E>>>,
    J: Iterator<Item = Result<TokenWithComments<S>, Error<E>>>, {
        for i in a.by_ref().zip(b.by_ref()) {
            match i {
                (Err(a), Err(b)) => assert_eq!(a, b),
                (Err(_), Ok(_)) => assert!(false),
                (Ok(_), Err(_)) => assert!(false),
                (Ok(a), Ok(b)) => assert_eq!(a, b),
            }
        }
        assert!(a.next().is_none());
        assert!(b.next().is_none());
    }

    fn new_tokenizer_1(s: &str) -> Tokenizer<String, NoError, StringCharReader<Chars>> {
        Tokenizer::new(StringCharReader::new(s.chars()))
    }

    fn new_tokenizer_2<'a>(s: &'a str) -> Tokenizer<&'a str, NoError, StrIndexCharReader<'a, str, CharIndices<'a>>> {
        Tokenizer::new(StrIndexCharReader::new(s, str::char_indices))
    }

    #[test]
    fn test_tokenizer_empty() {
        test_tokenizer(new_tokenizer_1(""), vec![].into_iter());
        test_tokenizer(new_tokenizer_2(""), vec![].into_iter());
    }

    #[test]
    fn test_tokenizer_simple() {
        test_tokenizer(new_tokenizer_1("a"), vec![Ok(TokenWithComments::Text("a".to_string()))].into_iter());
        test_tokenizer(new_tokenizer_1("a b c"), vec![Ok(TokenWithComments::Text("a b c".to_string()))].into_iter());
        test_tokenizer(new_tokenizer_1("[["), vec![Ok(TokenWithComments::Open), Ok(TokenWithComments::Open)].into_iter());
        test_tokenizer(new_tokenizer_1("#[]#"), vec![Ok(TokenWithComments::Comment(true, "#[]#".to_string()))].into_iter());
        test_tokenizer(new_tokenizer_1("##comment test\n"), vec![Ok(TokenWithComments::Comment(false, "##comment test\n".to_string()))].into_iter());
        test_tokenizer(new_tokenizer_1("[hello"), vec![Ok(TokenWithComments::Open), Ok(TokenWithComments::Text("hello".to_string()))].into_iter());
        test_tokenizer(new_tokenizer_1("[ ]"), vec![Ok(TokenWithComments::Open), Ok(TokenWithComments::Whitespace(" ".to_string())), Ok(TokenWithComments::Close)].into_iter());
        test_tokenizer(new_tokenizer_1("[a b c|1 2 3]"), vec![Ok(TokenWithComments::Open), Ok(TokenWithComments::Text("a b c".to_string())), Ok(TokenWithComments::Divider), Ok(TokenWithComments::Text("1 2 3".to_string())), Ok(TokenWithComments::Close)].into_iter());

        test_tokenizer(new_tokenizer_2("a"), vec![Ok(TokenWithComments::Text("a"))].into_iter());
        test_tokenizer(new_tokenizer_2("a b c"), vec![Ok(TokenWithComments::Text("a b c"))].into_iter());
        test_tokenizer(new_tokenizer_2("[["), vec![Ok(TokenWithComments::Open), Ok(TokenWithComments::Open)].into_iter());
        test_tokenizer(new_tokenizer_2("#[]#"), vec![Ok(TokenWithComments::Comment(true, "#[]#"))].into_iter());
        test_tokenizer(new_tokenizer_2("##comment test\n"), vec![Ok(TokenWithComments::Comment(false, "##comment test\n"))].into_iter());
        test_tokenizer(new_tokenizer_2("[hello"), vec![Ok(TokenWithComments::Open), Ok(TokenWithComments::Text("hello"))].into_iter());
        test_tokenizer(new_tokenizer_2("[ ]"), vec![Ok(TokenWithComments::Open), Ok(TokenWithComments::Whitespace(" ")), Ok(TokenWithComments::Close)].into_iter());
        test_tokenizer(new_tokenizer_2("[a b c|1 2 3]"), vec![Ok(TokenWithComments::Open), Ok(TokenWithComments::Text("a b c")), Ok(TokenWithComments::Divider), Ok(TokenWithComments::Text("1 2 3")), Ok(TokenWithComments::Close)].into_iter());
    }

    #[test]
    fn test_tokenizer_escape() {
        test_tokenizer(new_tokenizer_1(r"\["), vec![Ok(TokenWithComments::Escape('['))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\]"), vec![Ok(TokenWithComments::Escape(']'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\|"), vec![Ok(TokenWithComments::Escape('|'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\\"), vec![Ok(TokenWithComments::Escape('\\'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\#"), vec![Ok(TokenWithComments::Escape('#'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\"), vec![Err(Error::UnexpectedEOF)].into_iter());
        test_tokenizer(new_tokenizer_1(r"\n"), vec![Ok(TokenWithComments::Escape('\n'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\r"), vec![Ok(TokenWithComments::Escape('\r'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\t"), vec![Ok(TokenWithComments::Escape('\t'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\q"), vec![Err(Error::UnknownEscapeSequence('q'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\0"), vec![Ok(TokenWithComments::Null)].into_iter());
        test_tokenizer(new_tokenizer_1(r"\x64"), vec![Ok(TokenWithComments::Escape('\x64'))].into_iter());
        test_tokenizer(new_tokenizer_1(r"\u[64]"), vec![Ok(TokenWithComments::Escape('\u{64}'))].into_iter());

        test_tokenizer(new_tokenizer_2(r"\["), vec![Ok(TokenWithComments::Escape('['))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\]"), vec![Ok(TokenWithComments::Escape(']'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\|"), vec![Ok(TokenWithComments::Escape('|'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\\"), vec![Ok(TokenWithComments::Escape('\\'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\#"), vec![Ok(TokenWithComments::Escape('#'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\"), vec![Err(Error::UnexpectedEOF)].into_iter());
        test_tokenizer(new_tokenizer_2(r"\n"), vec![Ok(TokenWithComments::Escape('\n'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\r"), vec![Ok(TokenWithComments::Escape('\r'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\t"), vec![Ok(TokenWithComments::Escape('\t'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\q"), vec![Err(Error::UnknownEscapeSequence('q'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\0"), vec![Ok(TokenWithComments::Null)].into_iter());
        test_tokenizer(new_tokenizer_2(r"\x64"), vec![Ok(TokenWithComments::Escape('\x64'))].into_iter());
        test_tokenizer(new_tokenizer_2(r"\u[64]"), vec![Ok(TokenWithComments::Escape('\u{64}'))].into_iter());
    }

    #[test]
    fn test_tokenizer_complex() {
        test_tokenizer(new_tokenizer_1(
            r"[ [a|] #[this is a comment]#
b c |
 1 2 3 ]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Whitespace(" ".to_string())),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Text("a".to_string())),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Whitespace(" ".to_string())),
                Ok(TokenWithComments::Comment(true, "#[this is a comment]#".to_string())),
                Ok(TokenWithComments::Text("\nb c ".to_string())),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Text("\n 1 2 3 ".to_string())),
                Ok(TokenWithComments::Close),
            ].into_iter(),
        );
        test_tokenizer(
            new_tokenizer_1("[| [| [| [| [|!@$]]]]]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Whitespace(" ".to_string())),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Whitespace(" ".to_string())),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Whitespace(" ".to_string())),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Whitespace(" ".to_string())),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Text("!@$".to_string())),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Close),
            ].into_iter(),
        );
        test_tokenizer(
            new_tokenizer_1(r"[|right\[ stuff]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Text("right".to_string())),
                Ok(TokenWithComments::Escape('[')),
                Ok(TokenWithComments::Text(" stuff".to_string())),
                Ok(TokenWithComments::Close),
            ].into_iter(),
        );
        test_tokenizer(
            new_tokenizer_1(r"[left stuff|]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Text("left stuff".to_string())),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Close),
            ].into_iter(),
        );
        test_tokenizer(
            new_tokenizer_1(r"[FirstElement|\0]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Text("FirstElement".to_string())),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Null),
            ].into_iter(),
        );

        test_tokenizer(new_tokenizer_2(
            r"[ [a|] #[this is a comment]#
b c |
 1 2 3 ]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Whitespace(" ")),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Text("a")),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Whitespace(" ")),
                Ok(TokenWithComments::Comment(true, "#[this is a comment]#")),
                Ok(TokenWithComments::Text("\nb c ")),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Text("\n 1 2 3 ")),
                Ok(TokenWithComments::Close),
            ].into_iter(),
        );
        test_tokenizer(
            new_tokenizer_2("[| [| [| [| [|!@$]]]]]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Whitespace(" ")),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Whitespace(" ")),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Whitespace(" ")),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Whitespace(" ")),
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Text("!@$")),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Close),
                Ok(TokenWithComments::Close),
            ].into_iter(),
        );
        test_tokenizer(
            new_tokenizer_2(r"[|right\[ stuff]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Text("right")),
                Ok(TokenWithComments::Escape('[')),
                Ok(TokenWithComments::Text(" stuff")),
                Ok(TokenWithComments::Close),
            ].into_iter(),
        );
        test_tokenizer(
            new_tokenizer_2(r"[left stuff|]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Text("left stuff")),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Close),
            ].into_iter(),
        );
        test_tokenizer(
            new_tokenizer_2(r"[FirstElement|\0]"),
            vec![
                Ok(TokenWithComments::Open),
                Ok(TokenWithComments::Text("FirstElement")),
                Ok(TokenWithComments::Divider),
                Ok(TokenWithComments::Null),
            ].into_iter(),
        );
    }
}
