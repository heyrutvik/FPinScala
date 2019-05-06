package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def head[A](l: List[A]): A = l match {
    case Nil => throw new UnsupportedOperationException("head of empty list")
    case Cons(x, _) => x
  }

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("tail of empty list")
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("setHead on empty list")
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else drop(tail(l), n - 1)
  }

  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(x, xs) if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => throw new UnsupportedOperationException("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, z) => z + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  def sum3(ns: List[Int]) = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]) = foldLeft(ns, 1.0)(_ * _)

  def length1[A](l: List[A]): Int = foldLeft(l, 0)((z, _) => z + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil:List[A])((acc, x) => Cons(x, acc))

  def reverse1[A](l: List[A]): List[A] = {
    @annotation.tailrec
    def go[A](l: List[A], acc: List[A]): List[A] = l match {
      case Nil => acc
      case Cons(x, xs) => go(xs, Cons(x, acc))
    }
    go(l, Nil)
  }

  def append1[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_, _))

  def append2[A](a1: List[A], a2: List[A]): List[A] = reverse(foldLeft(a1, reverse(a2))((z, a) => Cons(a, z)))

  def concat[A](ls: List[List[A]]): List[A] = foldRight(ls, Nil:List[A])((a, z) => append1(a, z))

  def flip[A, B, C](f: (A, B) => C): (B, A) => C = (b: B, a: A) => f(a, b)

  def foldLeft1[A,B](l: List[A], z: B)(f: (B, A) => B): B = foldRight(reverse(l), z)(flip(f))

  def foldRight1[A,B](l: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(l), z)(flip(f))

  def map[A,B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(x, xs) => Cons(f(x), map(xs)(f))
  }

  def map1[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((a, z) => Cons(f(a), z))

  def filter[A](as: List[A])(p: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(x, xs) if p(x) => filter(xs)(p)
    case Cons(x, xs) => Cons(x, filter(xs)(p))
  }

  def filter1[A](as: List[A])(p: A => Boolean): List[A] = foldRight(as, Nil:List[A]) { (a, z) =>
    if (p(a)) z else Cons(a, z)
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = as match {
    case Nil => Nil
    case Cons(x, xs) => append(f(x), flatMap(xs)(f))
  }

  def flatMap1[A,B](as: List[A])(f: A => List[B]): List[B] = foldRight(as, Nil:List[B])((a, z) => append(f(a), z))

  def filter2[A](as: List[A])(p: A => Boolean): List[A] = flatMap(as)(a => if (p(a)) Nil else Cons(a, Nil))

  def zip[A, B](as: List[A], bs: List[B]): List[(A, B)] = (as, bs) match  {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(a, as1), Cons(b, bs1)) => Cons((a, b), zip(as1, bs1))
  }

  def e3_22(as: List[Int], bs: List[Int]): List[Int] = map(zip(as, bs)){case (a, b) => a + b}
}
