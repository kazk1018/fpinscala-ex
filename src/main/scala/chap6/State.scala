package chap6


trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  def nonNegative(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    (if (n < 0) -(n + 1) else n, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegative(rng)
    (n.toDouble / (Int.MaxValue+1), rng2)
  }

  def intDouble(rng: RNG) = {
    val (n, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((n, d), rng3)
  }

  def douleInt(rng: RNG) = {
    val ((n, d), rng2) = intDouble(rng)
    ((d, n), rng2)
  }

  def double3(rng: RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(c: Int, r: RNG, l: List[Int]): (List[Int], RNG) = {
      if(c <= 0) (l, r)
      else {
        val (n, rng2) = r.nextInt
        go(c-1, rng2, n :: l)
      }
    }
    go(count, rng, List[Int]())
  }

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] = {
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }
  }

  def doubleViaMap(rng: RNG): Rand[Double] =
    map(nonNegative)(_ / (Int.MaxValue.toDouble + 1))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }
  }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List[A]()))((f, acc) => map2(f, acc)(_ :: _))

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def map_[A, B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  def map2_[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = {
    flatMap(ra)(a => map(rb)(b => f(a, b)))
  }
}

case class State[S, +A](run: S => (A, S)) {
  def flatMap[B](f: A => State[S, B]): State[S, B] = {
    State(s => {
      val (a, s1) = run(s)
      f(a).run(s1)
    })
  }

  def map[B](f: A => B): State[S, B] =
    flatMap(a => State.unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}
