use std::iter::Peekable;

#[derive(Clone, Debug)]
pub struct PeekTwo<I: Iterator> {
  peekable: Peekable<I>,
  peeked: Option<Option<I::Item>>,
}

impl<I: Iterator> PeekTwo<I> {
  pub fn new(iter: I) -> PeekTwo<I> {
    PeekTwo {
      peekable: iter.peekable(),
      peeked: None,
    }
  }

  pub fn peek(&mut self) -> Option<&I::Item> {
    self
      .peeked
      .get_or_insert_with(|| self.peekable.next())
      .as_ref()
  }

  pub fn peek_next(&mut self) -> Option<&I::Item> {
    self.peekable.peek()
  }
}

impl<I: Iterator> Iterator for PeekTwo<I> {
  type Item = I::Item;

  fn next(&mut self) -> Option<I::Item> {
    match self.peeked.take() {
      Some(v) => v,
      None => self.peekable.next(),
    }
  }

  fn count(self) -> usize {
    panic!("count not implemented for PeekTwo")
  }

  fn nth(self: &mut PeekTwo<I>, _: usize) -> Option<I::Item> {
    panic!("nth not implemented for PeekTwo")
  }

  fn last(self) -> Option<I::Item> {
    panic!("last not implemented for PeekTwo")
  }

  #[inline]
  fn size_hint(&self) -> (usize, Option<usize>) {
    panic!("size_hint not implemented for PeekTwo")
  }
}

#[test]
fn test_peek_two() {
  let chars = vec!['a', 'b', 'c', 'd', 'e'];
  let mut sut = PeekTwo::new(chars.iter());
  assert_eq!(sut.peek(), Some(&&'a'));
  assert_eq!(sut.peek_next(), Some(&&'b'));
  assert_eq!(sut.next(), Some(&'a'));
  assert_eq!(sut.next(), Some(&'b'));
  assert_eq!(sut.peek(), Some(&&'c'));
  assert_eq!(sut.peek_next(), Some(&&'d'));
}
