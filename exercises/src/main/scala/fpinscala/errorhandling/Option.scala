package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  // B >: A denotes/indicates B is supertype of A
  def getOrElse[B>:A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this match {
    case None => None
    case Some(a) => f(a)
  }
  def flatMap1[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None
  // map(f): Option[Option[B]]

  def orElse[B>:A](ob: => Option[B]): Option[B] = this match {
    case None => ob
    case _ => this
  }

  def filter(f: A => Boolean): Option[A] = this match {
    case Some(a) if f(a) => Some(a)
    case _ => None
  }

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
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))


  def lift[A, B](f: A => B): Option[A] => Option[B] = _ map f

  val absO: Option[Double] => Option[Double] = lift(math.abs)

  def map2[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] = (oa, ob) match {
    case (Some(a), Some(b)) => Some(f(a, b))
    case _ => None
  }
  def map21[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    oa flatMap (a => ob map (b => f(a, b)))


  // for-yield syntax sugar
  def map22[A,B,C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- oa
      bb <- ob
    } yield f(aa, bb)

  def lift2[A, B, C](f: (A, B) => C): (Option[A], Option[B]) => Option[C] = (a, b) => map2(a, b)(f)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case oh :: tail =>
      oh flatMap
        (h => sequence(tail) map (t => h :: t)) // : A => Option[A]
  }

  def sequence1[M](a: List[Option[M]]): Option[List[M]] =
    a.foldRight[Option[List[M]]](Some(Nil)) ((a, b) => map2(a, b)(_ :: _))
  /*
  A -- Option[M]
  B -- Option[List[M]]
  (A, B) => B -- (Option[M], Option[List[M]]) => Option[List[M]]
  so (::) has been lifted
   */


  //TODO ??????
  def sequence2[M](a: List[Option[M]]): Option[List[M]] =
    traverse(a)(q => q)
  /*
  traverse(a)(id)

  a match {
    case Nil => Some(Nil)
    case h :: tail => map2(h, traverse(a)(id))(_ :: _)
    }



   */

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: tail => map2(f(h), traverse(tail)(f))(_ :: _)
  }

  def traverse1[M, N](a: List[M])(f: M => Option[N]): Option[List[N]] =
    a.foldRight[Option[List[N]]](Some(Nil)) ((a, b) => map2(f(a), b)(_ :: _))

  /*
  f: M => Option[N]
  A -- M
  B -- Option[List[N]]
  (A, B) => B -- (M, Option[List[N]]) -> Option[List[N]]
  (f(A), B) => B -- (Option[N], Option[List[N]]) -----map2----> Option[List[N]]
   */


}
