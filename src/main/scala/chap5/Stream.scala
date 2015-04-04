package chap5

import scala.{Stream => _}

trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => List()
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
    case Cons(h, _) if n == 1 => Stream.cons(h(), Stream.empty)
    case _ => Stream.empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Stream.cons(h(), t().takeWhile(p))
    case _ => Stream.empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  def forAll(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) && b)

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((a, b) => if (p(a)) Stream.cons(a, b) else Stream.empty)

  def headOptionViaFoldRight: Option[A] =
    foldRight(None[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] = this match {
    case Cons(h, t) => Stream.cons(f(h()), t().map(f))
    case _ => Stream.empty
  }

  def filter(f: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if f(h()) => Stream.cons(h(), t().filter(f))
    case _ => Stream.empty
  }

  def append[B >: A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => Stream.cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] = this match {
    case Cons(h, t) => f(h()) append t().flatMap(f)
    case _ => Stream.empty
  }

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))


  def constant[A](a: A): Stream[A] = {
    lazy val tail: Stream[A] = Cons(() => a, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  val fibs = {
    def go(a0: Int, a1: Int): Stream[Int] = {
      cons(a0, go(a0, a0 + a1))
    }
    go(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => empty
    case Some((a, s)) => cons(a, unfold(s)(f))
  }
}
