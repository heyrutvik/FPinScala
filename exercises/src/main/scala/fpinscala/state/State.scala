package fpinscala.state

import scala.annotation.tailrec


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {
  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (i, next) = rng.nextInt
    if (i.abs <= 0) nonNegativeInt(next) else (i.abs, next)
  }

  // TODO: rewrite the solution, and for `f` of double1 as well
  def double(rng: RNG): (Double, RNG) = {
    nonNegativeInt(rng) match {
      case (p, n) => (p / (Int.MaxValue.toDouble + 1), n)
    }
  }

  def double1(rand: Rand[Double]): Rand[Double] = map(rand)(p => p / (Int.MaxValue.toDouble + 1))

  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (i1, next1) = rng.nextInt
    val (d2, next2) = double(next1)
    ((i1, d2), next2)
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = intDouble(rng) match {
    case (pair, next) => (pair.swap, next)
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, n1) = double(rng)
    val (d2, n2) = double(n1)
    val (d3, n3) = double(n2)
    ((d1, d2, d3), n3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @tailrec
    def go(count: Int, rng: RNG, acc: List[Int]): (List[Int], RNG) = {
      if (count == 0) (acc.reverse, rng)
      else {
        val (i, n) = rng.nextInt
        go(count-1, n, i :: acc)
      }
    }
    go(count, rng, Nil)
  }

  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, n1) = ra(rng)
    val (b, n2) = rb(n1)
    (f(a, b), n2)
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] = map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = rng => {
    @tailrec def go(fs: List[Rand[A]], rng: RNG, acc: List[A]): (List[A], RNG) = fs match {
      case Nil => (acc.reverse, rng)
      case a :: as => {
        val (i, n) = a(rng)
        go(as, n, i :: acc)
      }
    }
    go(fs, rng, Nil)
  }

  def ints1(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (i, n) = f(rng)
    g(i)(n)
  }

  // using flatMap
  def nonNegativeLessThan(n: Int): Rand[Int] = flatMap(nonNegativeInt) { a =>
    if (a + (n-1) - (a%n) >= 0) unit(a)
    else nonNegativeLessThan(n)
  }

  def mapF[A,B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def map2F[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(ra)(a => map(rb)(b => f(a, b)))
  //

}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State { s =>
    this.run(s) match { case (a, s1) => (f(a), s1) }
  }
  def flatMap[B](f: A => State[S, B]): State[S, B] = State[S, B] { s =>
    this.run(s) match { case (a, s1) => f(a).run(s1) }
  }
  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    this.flatMap(a => sb.map(b => f(a, b)))
}

object State {
  def unit[S, A](a: A): State[S, A] = State(s => (a, s))
  def sequence[S, A](ss: List[State[S, A]]): State[S, List[A]] = {
    def go(ss: List[State[S, A]], s: S, acc: List[A]): (List[A], S) = ss match {
      case Nil => (acc.reverse, s)
      case h :: t => h.run(s) match { case (a, s2) => go(t, s2, a :: acc) }
    }
    State(s => go(ss, s, Nil))
  }

  type Rand[A] = State[RNG, A]
}

object StateHelper {
  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object Machine {
  // solution 1 - using sequence and map
  def operate1(i: Input): State[Machine, (Int, Int)] = State { s =>
    (i, s) match {
      case (Coin, Machine(true, ca, co)) =>
        if (ca >= 0) ((co + 1, ca), s.copy(locked = false, coins = co + 1)) else ((co, ca), s)
      case (Coin, Machine(false, ca, co)) => ((co, ca), s)
      case (Turn, Machine(true, ca, co)) => ((co, ca), s)
      case (Turn, Machine(false, ca, co)) =>
        ((co, ca - 1), s.copy(locked = true, candies = ca - 1))
    }
  }
  def simulateMachine1(inputs: List[Input]): State[Machine, (Int, Int)] = State.sequence(inputs.map(i => operate1(i))).map(xs => xs.last)

  // solution 2 - using sequence and modify
  def operate2(i: Input)(m: Machine): Machine = (i, m) match {
    case (Coin, Machine(true, ca, co)) => if (ca >= 0) m.copy(locked = false, coins = co + 1) else m
    case (Coin, Machine(false, _, _)) => m
    case (Turn, Machine(true, _, _)) => m
    case (Turn, Machine(false, ca, _)) => m.copy(locked = true, candies = ca - 1)
  }
  def simulateMachine2(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- State.sequence(inputs.map(i => operate2(i) _).map(StateHelper.modify))
    m <- StateHelper.get[Machine]
  } yield (m.coins, m.candies)
}