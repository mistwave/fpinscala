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
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val xxxx: Int = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def sum3(ns: List[Int]): Int = foldLeft(ns, 0)(_ + _)

  def product3(ns: List[Double]): Double = foldLeft(ns, 1.0)(_ * _)

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, xs) => xs
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => Cons(h, Nil)
    case Cons(_, xs) => Cons(h, xs)
  }

  def drop[A](l: List[A], n: Int): List[A] = (l, n) match {
    case (Nil, _) => Nil
    case (list, 0) => list
    case (Cons(_, xs), num) => drop(xs, num - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(x, xs) =>
      if (f(x)) Cons(x, dropWhile(xs, f))
      else dropWhile(xs, f)
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(x, Nil) => Nil
    case Cons(x, xs) => Cons(x, init(xs))
  }

  // ex3.9
  def length[A](l: List[A]): Int = foldRight(l, 0)((_, b) => b + 1)

  // ex3.11
  def length1[A](l: List[A]): Int = foldLeft(l, 0)((x, _) => x + 1)

  // ex3.10
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
  }

  // ex3.12
  /*
  foldLeft(1 :: 2 :: 3 :: Nil, Nil) ((b, a) => a :: b)
  foldLeft(2 :: 3 :: Nil, 1 :: Nil) ((b, a) => a :: b)
  foldLeft(3 :: Nil, 2 :: 1 :: Nil) ((b, a) => a :: b)
  foldLeft(Nil, 3 :: 2 :: 1 :: Nil) ((b, a) => a :: b)
  3 :: 2 :: 1 :: Nil
   */
  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((b, a) => Cons(a, b))


  // this implement is wrong
  /*
  foldRight(1 :: 2 :: 3 :: Nil, Nil) ::
  1 :: foldRight(2 :: 3 :: Nil, Nil) ::
  1 :: 2 :: foldRight(3 :: Nil, Nil) ::
  1 :: 2 :: 3 :: foldRight(Nil, Nil) ::
  1 :: 2 :: 3 :: (Nil)
   */
  def reverse1[A](l: List[A]): List[A] = foldRight(l, Nil: List[A])((a, b) => Cons(a, b))


  // ex3.13
  def foldRightByFoldLeft[A, B](as: List[A], b0: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), b0)((b, a) => f(a, b))

  /* deriving:
  foldRightByFoldLeft(1 :: 2 :: Nil, Nil) ((a, b) => a :: b)
  foldLeft(2 :: 1 :: Nil, Nil)((b :: a) => a :: b))
  1 :: 2
  == foldRight(1 :: 2 :: Nil, Nil) ::
   */
  def foldRightByFoldLeft1[A, B](as: List[A], b0: B)(f: (A, B) => B): B =
    foldLeft(
      foldLeft(as, Nil: List[A])((b, a) => Cons(a, b)), // reverse
      b0)((b, a) => f(a, b))


  // ex3.14
  def append1[A](ls: List[A], rs: List[A]): List[A] =
    foldRight(ls, rs)((a, b) => Cons(a, b))

  // ex3.15
  def flat[A](ass: List[List[A]]): List[A] =
    foldRight(ass, Nil: List[A])(append1)


  // ex3.16
  def addByOne(as: List[Int]): List[Int] = as match {
    case Nil => Nil
    case Cons(m, ms) => Cons(m + 1, addByOne(ms))
  }

  // ex3.17
  def eleToString(as: List[Double]): List[String] = as match {
    case Nil => Nil
    case Cons(m, ms) => Cons(m.toString, eleToString(ms))
  }

  // ex3.18
  def map[A, B](l: List[A])(f: A => B): List[B] = l match {
    case Nil => Nil
    case Cons(m, ms) => Cons(f(m), map(ms)(f))
  }

  def mapByFold[A, B](as: List[A])(f: A => B): List[B] =
    foldRight(as, Nil: List[B])((a, b) => Cons(f(a), b))

  // ex3.19
  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case Cons(m, ms) => if (f(m)) Cons(m, filter(ms)(f)) else filter(ms)(f)
  }

  def filterByFold[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, Nil: List[A])((a, b) => {
      if (f(a)) Cons(a, b)
      else b
    })

  // ex3.20
  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] =
    flat(map(as)(f))

  // ex3.21
  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as) { a =>
      if (f(a)) List(a)
      else Nil
    }

  // ex3.22
  def eleAdd(ls: List[Int], rs: List[Int]): List[Int] = (ls, rs) match  {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, eleAdd(xs, ys))
  }

  // ex3.23
  def zipWith[A, B, C](ls: List[A], rs: List[B])(f: (A, B) => C): List[C] = (ls, rs) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
  }

  // ex3.24
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = {
    def helper(pat: List[A], sub:List[A]): Boolean = (pat, sub) match {
      case (_, Nil) => true // Nil is Nil's sub sequence
      case (Nil, _) => false
      case (Cons(p, ps), Cons(s, ss)) => if (p == s) helper(ps, ss) else false
    }
    sup match {
      case Nil => sub == Nil
      case Cons(_, ss) => if (helper(sup, sub)) true else helper(ss, sub)
    }
  }
}

object LengthTest {
  def main(args: Array[String]): Unit = {
    assert(List.length(List()) == 0)
    assert(List.length(List(1)) == 1)
    assert(List.length1(List()) == 0)
    assert(List.length1(List(1)) == 1)
    assert(List.length1(List(11, 2)) == 2)
    println("length test passed")

    println(List.reverse(List(1, 2, 3)))
    println(List.reverse(List()))
    println(List.reverse1(List(1, 2, 3)))
    println(List.reverse1(List()))
    println("reverse test passed")

    println(List.append1(List(), List(1, 2, 3)))
    println(List.append1(List(5, 5, 5), List(1, 2, 3)))
    println(List.append1(List(5, 5, 5), List()))
    println("append1 test passed")

    println(List.flat(List(List(0), List(5, 5, 5), List(1, 2, 3))))
    println(List.flat(List(List(5, 5, 5), List(), List(1, 2, 3))))
    println(List.flat(List(List(1, 2, 3))))
    println(List.flat(List()))
    println("flat test passed")


    println(List.map(List(1,2,3))(_ + 1))
    println("map test passed")
    println(List.filter(List(1,2,3))(_ % 2 == 0))
    println("filter test passed")

  }

}
