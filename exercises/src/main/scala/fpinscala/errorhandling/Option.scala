package fpinscala.errorhandling

import scala.annotation.tailrec
import scala.{Either => _, Option => _, Some => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this.flatMap(a => if (f(a)) this else None)

  /**
    * invoking `fold`,
    *   where B = Option[_] and default = None
    * is same as calling `flatMap` on it.
    *
    * Opt.fold(None)(f) === Opt.flatMap(f)
    */
  def fold[B](default: => B)(f: A => B): B = this.map(f).getOrElse(default)
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = mean(xs).flatMap(m => mean(xs.map(x => Math.pow(x - m, 2))))

  def map1[A, B](a: Option[A])(f: A => B): Option[B] = a.map(f)

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = a.flatMap(a => b.map(b => f(a, b)))

  def map3[A,B,C,D](a: Option[A], b: Option[B], c: Option[C])(f: (A, B, C) => D): Option[D] = a.flatMap(a => b.flatMap(b => c.map(c => f(a, b, c))))

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    @tailrec
    def go(a: List[Option[A]], acc: List[A]): List[A] = a match {
      case Nil => acc
      case x :: xs => go(xs, x.fold(acc)(x => x :: acc))
    }
    val g = go(a, Nil)
    if (a.length == g.length) Some(g.reverse) else None
  }

  def sequence1[A](a: List[Option[A]]): Option[List[A]] = {
    def go(a: List[Option[A]], acc: Option[List[A]]): Option[List[A]] = a match {
      case Nil => acc
      case x :: xs => x.flatMap(a => go(xs, acc).map(a :: _))
    }
    go(a, Some(Nil))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = sequence(a.map(f))

  def traverse1[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def go(a: List[A], acc: List[B]): List[B] = a match {
      case Nil => acc
      case x :: xs => f(x).fold(go(xs, acc))(fx => fx :: go(xs, acc))
    }
    val g = go(a, Nil)
    if (a.length == g.length) Some(g) else None
  }

  def traverse2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    def go(a: List[A], acc: Option[List[B]]): Option[List[B]] = a match {
      case Nil => acc
      case x :: xs => f(x).flatMap(fx => go(xs, acc).map(gs => fx :: gs))
    }
    go(a, Some(Nil))
  }
}