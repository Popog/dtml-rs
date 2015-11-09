use std::fmt::{Display, Formatter, Result};
use std::iter::{FromIterator, IntoIterator, once};
use token::TextAccumulator;

pub trait TupleContainer {
    type Elements : Eq + Default;
    type Element: TupleContainer;

    fn is_empty(c: &Self::Elements) -> bool;

    // When you really need it you can use
    //&'a <C as TupleContainer>::Elements : IntoIterator<Item=&'a Tuple<<C as TupleContainer>::Element>>,
    fn iterate<'a, R, F: FnMut(&'a Tuple<Self::Element>)->Option<R>>(c: &'a Self::Elements, f: F) -> Option<R>
    where Self::Element: 'a;
}

pub trait LazyTupleContainer<S> {
    type Elements : Eq + Default;
}

#[derive(PartialEq, Eq)]
pub enum Tuple<C: TupleContainer> {
    Parent(C::Elements),
    Value(String),
    Null,
}

#[derive(PartialEq, Eq)]
pub enum LazyTuple<S: Eq, C: LazyTupleContainer<S>> {
    Parent(C::Elements),
    Value(TextAccumulator<S>),
    Null,
}

#[derive(PartialEq, Eq)]
pub struct VecTuple;

impl TupleContainer for VecTuple {
    type Elements = Vec<Tuple<VecTuple>>;
    type Element = VecTuple;

    fn is_empty(c: &Self::Elements) -> bool { c.is_empty() }

    fn iterate<'a, R, F: FnMut(&'a Tuple<Self::Element>)->Option<R>>(c: &'a Self::Elements, mut f: F) -> Option<R>
    where Self::Element: 'a {
        for element in c { if let Some(r) = f(element) { return Some(r); } }
        None
    }
}

impl <S: Eq> LazyTupleContainer<S> for VecTuple {
    type Elements = Vec<LazyTuple<S, VecTuple>>;
}

pub fn parent_element<Container, T>(t: T) -> Container
where Container : Default + Extend<T> {
    let mut p: Container = Default::default();
    p.extend(once(t));
    p
}

impl <S, C1> LazyTuple<S, C1>
where S: Eq+AsRef<str>,
C1: LazyTupleContainer<S>,
<C1 as LazyTupleContainer<S>>::Elements: IntoIterator<Item=LazyTuple<S, C1>>, {
    pub fn eval<C2>(self) -> Tuple<C2>
    where C2: TupleContainer,
    <C2 as TupleContainer>::Elements: FromIterator<Tuple<C2>>, {
        match self {
            LazyTuple::Parent(cs) => Tuple::Parent(cs.into_iter().map(|c| c.eval()).collect()),
            LazyTuple::Value(t) => Tuple::Value(t.collect()),
            LazyTuple::Null => Tuple::Null,
        }
    }
}

impl <C: TupleContainer> Display for Tuple<C> {
    fn fmt(&self, fmt: &mut Formatter) -> Result {
        match *self {
            Tuple::Parent(ref cs) => {
                try!(write!(fmt, "["));
                if let Some(r) = C::iterate(cs, |c| write!(fmt, "{}|", c).err()) {
                    return Err(r)
                }
                write!(fmt, "]")
            },
            Tuple::Value(ref s) => write!(fmt, "{}", s),
            Tuple::Null => write!(fmt, r"\0"),
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
    use tuple::{Tuple, VecTuple};

    fn vec_string_compare(t: Tuple<VecTuple>, s: &str) {
        assert_eq!(format!("{}", &t), s);
    }

    #[test]
    fn test_tuple_simple() {
        vec_string_compare(Tuple::Value(r"".to_string()),      r"");
        vec_string_compare(Tuple::Value(r" ".to_string()),     r" ");
        vec_string_compare(Tuple::Value(r"test".to_string()),  r"test");
        vec_string_compare(Tuple::Value(r" test".to_string()), r" test");
        vec_string_compare(Tuple::Null,                        r"\0");
        vec_string_compare(Tuple::Parent(vec![]),              "[]");
    }

    #[test]
    fn test_tuple_complex() {
        vec_string_compare(
            Tuple::Parent(vec![
                Tuple::Value(r"".to_string()),
            ]),
            r"[|]",
        );
        vec_string_compare(
            Tuple::Parent(vec![
                Tuple::Value(r"test".to_string()),
            ]),
            r"[test|]",
        );
        vec_string_compare(
            Tuple::Parent(vec![
                Tuple::Value(r"test".to_string()),
                Tuple::Null,
            ]),
            r"[test|\0|]",
        );
        vec_string_compare(
            Tuple::Parent(vec![
                Tuple::Parent(vec![]),
                Tuple::Value(r"test".to_string()),
                Tuple::Null,
            ]),
            r"[[]|test|\0|]",
        );
        vec_string_compare(
            Tuple::Parent(vec![
                Tuple::Parent(vec![
                    Tuple::Parent(vec![]),
                    Tuple::Value(r"foo".to_string()),
                ]),
                Tuple::Value(r"test".to_string()),
                Tuple::Null,
            ]),
            r"[[[]|foo|]|test|\0|]",
        );
    }

    //let mut code: Option<String> = None;

}
