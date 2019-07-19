package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  //  def foldLeft[B](z: => B)(f: (=> B, A) => B): B = this match {
  //    case Cons(h, t) => t().foldLeft(f(z, h))(f)
  //    case _ => z
  //  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  /*
  expanding:
  Stream(1, 2, 3).exists(_ % 2 == 0)
  S(1,2,3).foldRight(false)((a, b) => a % 2 == 0 || b)) --> f: (A, => B) => B
  f(1, S(2,3).foldRight(false)(f))     FOR NOW, the 2nd arg won't be evaluated
  1 % 2 == 0 || S(2,3).foldRight(false)(f)
  false || S(2, 3).foldRight(false)(f) then we go to eval the latter one
  false || f(2, S(3).foldRight(false)(f))
  false || 2 % 2 == 0 || S(3).foldRight(false)(f)

   */

  def zip[B](sb: Stream[B]): Stream[(A, B)] =
    zipWith(sb)((_, _))


  def zipWith[B, C](sb: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, sb)) {
      case (Cons(h1, t1), Cons(h2, t2)) =>
        Some(
          f(h1(), h2()) -> (t1(),t2())
        )
      case _ => None
    }

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // ex 5.2
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    //    case Cons(h, t) if n > 0 => Cons(h, () => t().take(n-1))
    case _ => Empty
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  // ex 5.3
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    //    case Cons(h, t) if p(h) => cons(h(), t().takeWhile(p))
    case Cons(h, t) =>
      if (p(h())) cons(h(), t().takeWhile(p))
      else this
    case _ => this
  }

  // ex 5.5
  def takeWhile1(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else empty
    )


  // ex 5.4
  def forAll(p: A => Boolean): Boolean = foldRight(false)((a, b) => p(a) && b)

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, _) => Some(h())
  }

  // ex 5.6
  def headOption1: Option[A] =
    foldRight(None: Option[A])((a, _) => Some(a))

  // ex 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) =>
      if (p(a)) cons(a, b)
      else b)

  def append[B >: A](that: => Stream[B]): Stream[B] =
    foldRight(that)((a, b) =>
      cons(a, b)
    )


  // flat(this.map(f))
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)


  // ex5.1
  def toList: List[A] = this match {
    case Empty => List.empty
    case Cons(h, t) => h() :: t().toList
  }

  // ex5.13
  def map1[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case _ => None
    }

  def take1(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), m) if m > 0 => Some(h(), (t(), m - 1))
      case _ => None
    }


  def takeWhile2(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  def With[B, C](bs: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold((this, bs)) {
      case (Cons(l, ls), Cons(r, rs)) =>
        Some(
          f(l(), r()) -> (ls(), rs())
        )
      case _ => None
    }

  def All[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold((this, that)) {
      case (Cons(l, ls), Cons(r, rs)) =>
        Some(
          (Some(l()), Some(r())) -> (ls(), rs())
        )
      case (Cons(l, ls), Empty) =>
        Some(
          (Some(l()), None) -> (ls(), empty)
        )
      case (Empty, Cons(r, rs)) =>
        Some(
          (None, Some(r())) -> (empty, rs())
        )
      case _ => None
    }


  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  // 5.14
  def startsWith[B](s: Stream[B]): Boolean =
    this.All(s).takeWhile(_._2.nonEmpty) forAll {
      case (l, r) => l == r
    }

  // TODO why drop? not pattern match?
  // 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Empty => None
      case s => Some(s -> s.drop(1))
    } append empty

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: B)(f: (A, =>B) => B): Stream[B] =
    foldRight((z, Stream(z)))((a, p0) => {
      lazy val p1 = p0
      val b2 = f(a, p1._1)
      (b2, cons(b2, p1._2))
    })._2
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  // ex 5.8
  def constant[A](a: A): Stream[A] = cons(a, constant(a))

  // ex 5.9
  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  // ex 5.10
  def fib(a: Int, b: Int): Stream[Int] = Stream.cons(a, fib(b, a + b))

  def fibs: Stream[Int] = fib(0, 1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }

  // ex 5.12

  def ones1: Stream[Int] = unfold(1)(_ => Some((1, 1)))

  def constant1[A](a: A): Stream[A] = unfold(a)(_ => Some((a, a)))

  def from1(n: Int): Stream[Int] = unfold(n)(s => Some((s, s + 1)))

  def fibs1: Stream[Int] = unfold((0, 1)) {
    case (a, b) => Some(a -> (b, a + b))
  }


  def main(args: Array[String]): Unit = {
    println("take")
    Stream(1, 2, 3).take1(1).toList.foreach(println)
    println("drop")
    Stream(122, 211, 3, 31, 1).drop(2).toList.foreach(println)
    println("takeWhile")
    Stream(1, 8, 3).takeWhile(_ > 2).toList.foreach(println)

    println("constant")
    val threes = constant(3)
    threes.take(3).toList.foreach(println)

    println("from 3")
    val fromThree = from(3)
    println(fromThree.take(3).toList)

    println("fibs:")
    println(fibs.take(10).toList)

    println("ones1 3")
    println(ones1.take(3).toList)

    println("from1 3")
    println(from1(3).take(3).toList)
    println("fibs1")
    println(fibs1.take(10).toList)

    println("tails")
    println(Stream(1,2,3).tails.map(_.toList).toList)
  }
}