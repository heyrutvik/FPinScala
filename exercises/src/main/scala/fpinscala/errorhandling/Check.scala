package fpinscala.errorhandling

import scala.{Option => _, Either => _, Left => _, Right => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Check[+E, +A] {
  def map[B](f: A => B): Check[E, B] = this match {
    case Fail(vs) => Fail(vs)
    case Pass(v) => Check.unit(f(v))
  }
  def apply[EE >: E, B](that: Check[EE, A => B]): Check[EE, B] = {
    (this, that) match {
        case (Pass(v), Pass(f)) => Pass(f(v))
        case (Fail(vs1), Fail(vs2)) => Fail(vs1 ::: vs2)
        case (Fail(v), _) => Fail(v)
        case (_, Fail(vs)) => Fail(vs)
    }
  }
  def map2[EE >: E, B, C](that: Check[EE, B])(f: (A, B) => C): Check[EE, C] = {
    this.apply(that.map(b => f(_, b)))
  }
  def orElse[EE >: E, AA >: A](that: => Check[EE, AA]): Check[EE, AA] = ???
}
case class Pass[+A](v: A) extends Check[Nothing, A]
case class Fail[+E](vs: List[E]) extends Check[E, Nothing]

object Check {
  def unit[A](a: => A): Check[Nothing, A] = Pass(a)
  def traverse[A, E](as: List[A])(f: A => Check[E, A]): Check[E, List[A]] =
    sequence(as.map(f))
  def sequence[A, E](as: List[Check[E, A]]): Check[E, List[A]] = ???
}