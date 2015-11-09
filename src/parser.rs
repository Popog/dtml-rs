use std::error;
use std::iter::{Iterator, once};
use token::{ContentType, Token, NonText, TokenWithComments, TextAccumulator, is_whitespace};
use tokenizer;
use tuple::{LazyTuple, parent_element, LazyTupleContainer};

type TokenWithCommentsResult<S, E> = Result<TokenWithComments<S>, tokenizer::Error<E>>;
type TokenResult<S, E> = Result<Token<S>, tokenizer::Error<E>>;


#[derive(Debug)]
pub enum Error<S: Eq, E: error::Error> {
    UnexpectedEOF,
    UnexpectedToken(Token<S>),
    TokenizerError(tokenizer::Error<E>),
}

fn map_error<T, S: Eq, E: error::Error>(r: Option<Result<T, tokenizer::Error<E>>>) -> Result<Option<T>, Error<S, E>> {
    match r {
        None => Ok(None),//Err(Error::UnexpectedEOF),
        Some(Err(e)) => Err(Error::TokenizerError(e)),
        Some(Ok(t)) => Ok(Some(t)),
    }
}

#[derive(PartialEq, Eq)]
enum Terminator {
    EOF,
    Close,
    Divider,
}

pub fn parse<S, E, I, C> (i: &mut I) -> Result<LazyTuple<S, C>, Error<S, E>>
where S: Eq+AsRef<str>,
E: error::Error,
I: Iterator<Item = TokenResult<S, E>>,
C: LazyTupleContainer<S>,
<C as LazyTupleContainer<S>>::Elements: Extend<LazyTuple<S, C>>, {
    let mut text = TextAccumulator::new();
    let t = try!(map_error(i.by_ref().filter_map(|i| text.filter(i)).next()));
    match (text.uncommitted_type(), t) {
        (_, None) => {
            text.commit();
            Ok(LazyTuple::Value(text))
        },

        (ContentType::Text, Some(NonText::Open)) => {
            // Tell parse_text_helper to keep parsing until EOF
            let (text, t) = try!(parse_text_helper(i, text, 1, false));
            match t {
                Terminator::EOF => Ok(LazyTuple::Value(text)),
                Terminator::Close => Err(Error::UnexpectedToken(Token::Close)),
                Terminator::Divider => Err(Error::UnexpectedToken(Token::Divider)),
            }
        },
        (_, Some(NonText::Open)) => {
            text.clear_uncommitted();
            match try!(parse_helper(i, text)) {
                LazyTuple::Value(text) => {
                    let (text, t) = try!(parse_text_helper(i, text, 0, true));
                    match t {
                        Terminator::EOF => Ok(LazyTuple::Value(text)),
                        Terminator::Close => Err(Error::UnexpectedToken(Token::Close)),
                        Terminator::Divider => Err(Error::UnexpectedToken(Token::Divider)),
                    }
                },
                tuple => {
                    // Loop til EOF, making sure everything is whitespace
                    let t = try!(map_error(i.skip_while(is_whitespace).next()));
                    if let Some(t) = t { Err(Error::UnexpectedToken(t)) }
                    else { Ok(tuple) }
                },
            }
        },

        (_, Some(NonText::Close)) => return Err(Error::UnexpectedToken(Token::Close)),

        (_, Some(NonText::Divider)) => return Err(Error::UnexpectedToken(Token::Divider)),

        (ContentType::Text, Some(NonText::Null)) => return Err(Error::UnexpectedToken(Token::Null)),
        (_, Some(NonText::Null)) => {
            // Loop til EOF, making sure everything is whitespace
            let t = try!(map_error(i.skip_while(is_whitespace).next()));
            if let Some(t) = t { Err(Error::UnexpectedToken(t)) }
            else { Ok(LazyTuple::Null) }
        },
    }

}

fn parse_helper<S, E, I, C> (i: &mut I, text: TextAccumulator<S>) -> Result<LazyTuple<S, C>, Error<S, E>>
where S: Eq+AsRef<str>,
E: error::Error,
I: Iterator<Item = TokenResult<S, E>>,
C: LazyTupleContainer<S>,
<C as LazyTupleContainer<S>>::Elements: Extend<LazyTuple<S, C>>, {

    let mut elements = match try!(parse_section_helper(i, text)) {
        ElementType::Empty => return Ok(LazyTuple::Parent(Default::default())),
        ElementType::Divided(t) => parent_element(t),
        ElementType::TerminatedText(t) => return Ok(t),
        ElementType::TerminatedWhitespace(t) => return Ok(t),
        ElementType::Terminated(t) => return Ok(LazyTuple::Parent(parent_element(t))),
    };

    loop {
        match try!(parse_section_helper(i, TextAccumulator::new())) {
            ElementType::Empty => return Ok(LazyTuple::Parent(elements)),
            ElementType::Divided(t) => elements.extend(once(t)),
            ElementType::TerminatedText(t) => {
                elements.extend(once(t));
                return Ok(LazyTuple::Parent(elements));
            },
            ElementType::TerminatedWhitespace(_) => return Ok(LazyTuple::Parent(elements)),
            ElementType::Terminated(t) => {
                elements.extend(once(t));
                return Ok(LazyTuple::Parent(elements));
            },
        }
    }
}

enum ElementType<T>{
    Empty,
    Divided(T),
    TerminatedWhitespace(T),
    TerminatedText(T),
    Terminated(T),
}

fn parse_section_helper<S, E, I, C> (i: &mut I, mut text: TextAccumulator<S>) -> Result<ElementType<LazyTuple<S, C>>, Error<S, E>>
where S: Eq+AsRef<str>,
E: error::Error,
I: Iterator<Item = TokenResult<S, E>>,
C: LazyTupleContainer<S>,
<C as LazyTupleContainer<S>>::Elements: Extend<LazyTuple<S, C>>, {
    let t = try!(map_error(i.by_ref().filter_map(|i| text.filter(i)).next()));
    match (text.uncommitted_type(), t) {
        (_, None) => Err(Error::UnexpectedEOF),

        (ContentType::Text, Some(NonText::Open)) => {
            text.commit();
            // Tell parse_text_helper to keep parsing until EOF
            let (text, t) = try!(parse_text_helper(i, text, 1, false));
            match t {
                Terminator::EOF => Err(Error::UnexpectedEOF),
                Terminator::Close => Ok(ElementType::Terminated(LazyTuple::Value(text))),
                Terminator::Divider => Err(Error::UnexpectedToken(Token::Divider)),
            }
        },
        (_, Some(NonText::Open)) => {
            text.clear_uncommitted();
            match try!(parse_helper(i, text)) {
                LazyTuple::Value(text) => {
                    let (text, t) = try!(parse_text_helper(i, text, 0, true));
                    match t {
                        Terminator::EOF => Err(Error::UnexpectedEOF),
                        Terminator::Close => Ok(ElementType::TerminatedText(LazyTuple::Value(text))),
                        Terminator::Divider => Ok(ElementType::Divided(LazyTuple::Value(text))),
                    }
                },
                tuple => {
                    match try!(map_error(i.skip_while(is_whitespace).next())) {
                        None => Err(Error::UnexpectedEOF),
                        Some(Token::Close) => Ok(ElementType::Terminated(tuple)),
                        Some(Token::Divider) => Ok(ElementType::Divided(tuple)),
                        Some(t) => Err(Error::UnexpectedToken(t)),
                    }
                },
            }
        },

        (ContentType::Empty, Some(NonText::Close)) => Ok(ElementType::Empty),
        (ContentType::Whitespace, Some(NonText::Close)) => {
            text.commit();
            Ok(ElementType::TerminatedWhitespace(LazyTuple::Value(text)))
        },
        (ContentType::Text, Some(NonText::Close)) => {
            text.commit();
            Ok(ElementType::TerminatedText(LazyTuple::Value(text)))
        },

        (_, Some(NonText::Divider)) => Ok(ElementType::Divided(LazyTuple::Value(text))),

        (ContentType::Text, Some(NonText::Null)) => Err(Error::UnexpectedToken(Token::Null)),
        (_, Some(NonText::Null)) => {
            text.clear_uncommitted();
            match try!(map_error(i.skip_while(is_whitespace).next())) {
                None => Err(Error::UnexpectedEOF),
                Some(Token::Close) => Ok(ElementType::Terminated(LazyTuple::Null)),
                Some(Token::Divider) => Ok(ElementType::Divided(LazyTuple::Null)),
                Some(t) => Err(Error::UnexpectedToken(t)),
            }
        },
    }
}

fn parse_text_helper<S, E, I> (i: &mut I, mut text: TextAccumulator<S>, mut depth: usize, mut post_close: bool) -> Result<(TextAccumulator<S>, Terminator), Error<S, E>>
where S: Eq+AsRef<str>,
E: error::Error,
I: Iterator<Item = TokenResult<S, E>>, {
    loop {
        match try!(map_error(i.by_ref().filter_map(|i| text.filter(i)).next())) {
            None => {
                if depth == 0 {
                    if post_close && text.uncommitted_type() == ContentType::Whitespace {
                        text.clear_uncommitted();
                    } else {
                        text.commit();
                    }
                    return Ok((text, Terminator::EOF));
                }
                return Err(Error::UnexpectedEOF)
            },
            Some(NonText::Divider) => {
                if depth == 0 {
                    if post_close && text.uncommitted_type() == ContentType::Whitespace {
                        text.clear_uncommitted();
                    } else {
                        text.commit();
                    }
                    return Ok((text, Terminator::Divider));
                }
                return Err(Error::UnexpectedToken(Token::Divider));
            }
            Some(NonText::Open) => {
                if text.uncommitted_type() == ContentType::Text {
                    text.commit();
                } else {
                    text.clear_uncommitted();
                }
                depth += 1;
                post_close = false;
            },
            Some(NonText::Null) => return Err(Error::UnexpectedToken(Token::Null)),
            Some(NonText::Close) => {
                match text.uncommitted_type() {
                    ContentType::Empty if !post_close => return Err(Error::UnexpectedToken(Token::Close)),
                    ContentType::Whitespace if post_close => text.clear_uncommitted(),
                    _ => text.commit(),
                }

                if depth == 0 {
                    return Ok((text, Terminator::Close));
                }
                depth -= 1;
                post_close = true;
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
    use tokenizer::{Tokenizer, StringCharReader, StrIndexCharReader};
    use token::{filter_comments};
    use tuple::{Tuple, VecTuple, TupleContainer};
    use parser::parse;

    fn test_parser<C: Eq+TupleContainer>(a: Tuple<C>, b: Tuple<C>) {
        assert!(PartialEq::eq(&a,&b), "`{}` != `{}`", a, b);
    }

    fn new_parser_1(s: &str) -> Tuple<VecTuple> {
        match parse::<_,_,_,VecTuple>(&mut Tokenizer::new(StringCharReader::new(s.chars())).filter_map(filter_comments)) {
            Ok(t) => t.eval(),
            Err(e) => {panic!("failed {:?}", e)},
        }
    }

    #[test]
    fn test_parser_empty() {
        test_parser(Tuple::Parent(vec![]), new_parser_1("[]"));
        test_parser(Tuple::Parent(vec![]), new_parser_1(" []"));
        test_parser(Tuple::Parent(vec![]), new_parser_1("[] "));
        test_parser(Tuple::Parent(vec![]), new_parser_1(" [] "));
    }

    #[test]
    fn test_parser_monad() {
        test_parser(Tuple::Value(r"".to_string()),      new_parser_1(r""));
        test_parser(Tuple::Value(r" ".to_string()),     new_parser_1(r" "));
        test_parser(Tuple::Value(r"test".to_string()),  new_parser_1(r"test"));
        test_parser(Tuple::Value(r" test".to_string()), new_parser_1(r" test"));
        test_parser(Tuple::Null,                        new_parser_1(r"\0"));
    }

    #[test]
    fn test_parser_tuple_1() {
        test_parser(
            Tuple::Parent(vec![Tuple::Value(r"".to_string()),]),
            new_parser_1(r"[|]"),
        );

        test_parser(new_parser_1(r"[|]"), new_parser_1(r" [|]"));

        test_parser(new_parser_1(r"[|]"), new_parser_1(r"[| ]"));
        test_parser(new_parser_1(r"[|]"), new_parser_1(r" [| ]"));

        test_parser(new_parser_1(r"[|]"), new_parser_1(r"[|] "));
        test_parser(new_parser_1(r"[|]"), new_parser_1(r" [|] "));
        test_parser(new_parser_1(r"[|]"), new_parser_1(r"[| ] "));
        test_parser(new_parser_1(r"[|]"), new_parser_1(r" [| ] "));
    }

    #[test]
    fn test_parser_tuple_2() {
        test_parser(
            Tuple::Parent(vec![Tuple::Value(r"test".to_string()),]),
            new_parser_1(r"[test|]"),
        );
        test_parser(new_parser_1(r"[test|]"), new_parser_1(r" [test|]"));

        test_parser(new_parser_1(r"[test|]"), new_parser_1(r"[test| ]"));
        test_parser(new_parser_1(r"[test|]"), new_parser_1(r" [test| ]"));

        test_parser(new_parser_1(r"[test|]"), new_parser_1(r"[test|] "));
        test_parser(new_parser_1(r"[test|]"), new_parser_1(r" [test|] "));
        test_parser(new_parser_1(r"[test|]"), new_parser_1(r"[test| ] "));
        test_parser(new_parser_1(r"[test|]"), new_parser_1(r" [test| ] "));
    }


    #[test]
    fn test_parser_tuple_3() {
        test_parser(
            Tuple::Parent(vec![
                Tuple::Value(r"hello".to_string()),
                Tuple::Value(r"world".to_string()),
            ]),
            new_parser_1(r"[hello|world]"),
        );
        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r" [hello|world]"));

        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r"[hello|world] "));
        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r" [hello|world] "));


        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r" [hello|world|]"));

        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r"[hello|world|] "));
        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r" [hello|world|] "));
        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r"[hello|world| ] "));
        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r" [hello|world| ] "));

        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r" [[hello]|world] "));
        test_parser(new_parser_1(r"[hello|world]"), new_parser_1(r" [[hello]|[world]] "));

    }

    #[test]
    fn test_parser_tuple_4() {
        test_parser(
            Tuple::Parent(vec![
                Tuple::Value(r"test".to_string()),
                Tuple::Null,
            ]),
            new_parser_1(r"[test|\0|]"),
        );
        test_parser(
            Tuple::Parent(vec![
                Tuple::Value(r"test".to_string()),
                Tuple::Null,
            ]),
            new_parser_1(r"[test|\0]"),
        );
        test_parser(
            Tuple::Parent(vec![
                Tuple::Parent(vec![]),
                Tuple::Value(r"test".to_string()),
                Tuple::Null,
            ]),
            new_parser_1(r"[[]|test|\0|]"),
        );
        test_parser(
            Tuple::Parent(vec![
                Tuple::Parent(vec![
                    Tuple::Parent(vec![]),
                    Tuple::Value(r"foo".to_string()),
                ]),
                Tuple::Value(r"test".to_string()),
                Tuple::Null,
            ]),
            new_parser_1(r"[[[]|foo|]|test|\0|]"),
        );
    }
}
