package chap2

object MyModule {

  def fib(n: Int): Int = {
    @annotation.tailrec
    def loop(n: Int, prev: Int, next: Int): Int = {
      if(n == 0) prev
      else loop(n - 1, next, prev + next)
    }
    loop(n, 0, 1)
  }

  def isSorted[A](as: List[A], ordered: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean = {
      if (n >= as.length - 1) true
      else if (!ordered(as(n), as(n + 1))) false
      else go(n + 1)
    }
    go(0)
  }

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
