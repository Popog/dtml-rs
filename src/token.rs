#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum TokenWithComments<S: Eq> {
    Open,
    Close,
    Divider,
    Comment(bool, S),
    Escape(char),
    Null,
    Text(S),
    Whitespace(S),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum Token<S: Eq> {
    Open,
    Close,
    Divider,
    Null,
    Escape(char),
    Text(S),
    Whitespace(S),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum NonText {
    Open,
    Close,
    Divider,
    Null,
}

impl <S: Eq> Into<Token<S>> for NonText {
    fn into(self) -> Token<S> {
        match self {
            NonText::Open => Token::Open,
            NonText::Close => Token::Close,
            NonText::Divider => Token::Divider,
            NonText::Null => Token::Null,
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
enum Text<S: Eq>
{
    Escape(char),
    Text(S),
    Whitespace(S),
}

pub fn is_whitespace<S: Eq, E>(t: &Result<Token<S>, E>) -> bool {
    if let Ok(Token::Whitespace(_)) = *t { true } else { false }
}

#[derive(PartialEq, Eq)]
pub struct TextAccumulator<S: Eq>{
    v: Vec<Text<S>>,
    committed_items: usize,
    committed_text: bool,
    committed_length: usize,
    uncommitted_text: bool,
    uncommitted_length: usize,
}

#[derive(PartialEq, Eq)]
pub enum ContentType {
    Empty,
    Whitespace,
    Text,
}

impl <S: Eq> TextAccumulator<S> {
    pub fn new() -> Self {
        TextAccumulator{
            v: Vec::new(),
            committed_items: 0,
            committed_text: false,
            committed_length: 0,
            uncommitted_text: false,
            uncommitted_length: 0,
        }
    }

    pub fn commit(&mut self) {
        self.committed_items = self.v.len();
        self.committed_text |= self.uncommitted_text;
        self.committed_length += self.uncommitted_length;

        self.uncommitted_text = false;
        self.uncommitted_length = 0;
    }

    pub fn clear_uncommitted(&mut self) {
        self.v.truncate(self.committed_items);
        self.uncommitted_text = false;
        self.uncommitted_length = 0;
    }

    pub fn committed_type(&self) -> ContentType {
        if self.committed_length == 0 { ContentType::Empty }
        else if self.committed_text { ContentType::Text }
        else { ContentType::Whitespace }
    }

    pub fn uncommitted_type(&self) -> ContentType {
        if self.uncommitted_length == 0 { ContentType::Empty }
        else if self.uncommitted_text { ContentType::Text }
        else { ContentType::Whitespace }
    }
}

impl <S: Eq + AsRef<str>> TextAccumulator<S> {
    pub fn filter<E>(&mut self, args: Result<Token<S>, E>) -> Option<Result<NonText, E>> {
        match args {
            Err(e) => Some(Err(e)),
            Ok(Token::Whitespace(w)) => {
                self.uncommitted_length += w.as_ref().len();
                self.v.push(Text::Whitespace(w));
                None
            },
            Ok(Token::Text(t)) => {
                self.uncommitted_text = true;
                self.uncommitted_length += t.as_ref().len();
                self.v.push(Text::Text(t));
                None
            },
            Ok(Token::Escape(c)) => {
                self.uncommitted_text = true;
                self.uncommitted_length += c.len_utf8();
                self.v.push(Text::Escape(c));
                None
            },
            Ok(Token::Open) => Some(Ok(NonText::Open)),
            Ok(Token::Close) => Some(Ok(NonText::Close)),
            Ok(Token::Null) => Some(Ok(NonText::Null)),
            Ok(Token::Divider) => Some(Ok(NonText::Divider)),
        }
    }

    pub fn collect(self) -> String {
        let mut s = String::with_capacity(self.committed_length);
        for i in self.v.into_iter() {
            match i {
                Text::Text(t) => s.push_str(t.as_ref()),
                Text::Whitespace(w) => s.push_str(w.as_ref()),
                Text::Escape(c) => s.push(c),
            };
        }
        s
    }
}


pub fn filter_comments<S: Eq, E>(t: Result<TokenWithComments<S>, E>) -> Option<Result<Token<S>, E>> {
    match t {
        Err(e) => Some(Err(e)),
        Ok(TokenWithComments::Open) => Some(Ok(Token::Open)),
        Ok(TokenWithComments::Close) => Some(Ok(Token::Close)),
        Ok(TokenWithComments::Divider) => Some(Ok(Token::Divider)),
        Ok(TokenWithComments::Comment(_, _)) => None,
        Ok(TokenWithComments::Escape(c)) => Some(Ok(Token::Escape(c))),
        Ok(TokenWithComments::Null) => Some(Ok(Token::Null)),
        Ok(TokenWithComments::Text(t)) => Some(Ok(Token::Text(t))),
        Ok(TokenWithComments::Whitespace(w)) => Some(Ok(Token::Whitespace(w))),
    }
}
