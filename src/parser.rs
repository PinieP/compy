enum ParseError {
    Mismatch,
    NoInput,
}

trait Parser {
    type In;
    type Out;
    fn parse<'a, Iter>(&self, iter: Iter) -> (Option<Self::Out>, Iter)
    where
        Iter: Iterator<Item = Self::In> + 'a;

    fn join(&self, other: &impl Parser<In = Self::In, Out = Self::Out>) {}
}

struct Join<T, U>
where
    T: Parser,
    U: Parser<Out = T::Out>,
{
    parsers: (T, U),
}

impl<T, U> Parser for Join<T, U>
where
    T: Parser,
    U: Parser<Out = T::Out>,
{
    type In = T::In;
    type Out = T::Out;

    fn parse<'a, Iter>(&self, iter: Iter) -> (Option<Self::Out>, Iter)
    where
        Iter: Iterator<Item = Self::In> + 'a,
    {
        todo!()
    }
}

struct Just<T: Eq> {
    pred: T,
}

impl<T: Eq> Parser for Just<T> {
    type In = T;
    type Out = T;
    fn parse<'a, Iter>(&self, mut iter: Iter) -> (Option<Self::In>, Iter)
    where
        Iter: Iterator<Item = Self::In> + 'a,
    {
        (iter.next().filter(|token| *token == self.pred), iter)
    }
}
