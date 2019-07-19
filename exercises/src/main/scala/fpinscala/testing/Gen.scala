package fpinscala.testing

import fpinscala.laziness.Stream
import fpinscala.state._
import fpinscala.parallelism._
import fpinscala.parallelism.Par.Par
import Gen._
import Prop._
import java.util.concurrent.{Executors,ExecutorService}

/*
The library developed in this chapter goes through several iterations. This file is just the
shell, which you can fill in and modify while working through the chapter.
*/


object Prop {
  type SuccessCount = Int
  type FailedCase = String
  type TestCases = Int
  type Result = Option[(FailedCase, SuccessCount)]
  def forAll[A](gen: Gen[A])(f: A => Boolean): Prop = Prop { (n, rng) =>
    randomStream(gen)(rng).zip(Stream.from(0)).take(n).map {
      case (a, i) =>
        try {
          if (f(a)) Passed else Falsified(a.toString, i)
        } catch {
          case e: Exception => Falsified(buildMsg(a, e), i)
        }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
    s"generated an exception: ${e.getMessage}\n" +
    s"stack trace: \n ${e.getStackTrace.mkString("\n")}"


}

case class Prop(run: (TestCases, RNG) => Result) {
  // 8.9
  def &&(p: Prop): Prop = ???
  def ||(p: Prop): Prop = ???
}


sealed trait Result {
  def isFalsified: Boolean
}

case object Passed extends Result {
  override def isFalsified: Boolean = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified: Boolean = true
}

object Gen {
  // 8.5
  def unit[A](a: => A): Gen[A] =
    Gen(State(RNG.unit(a)))

  def boolean: Gen[Boolean] =
    Gen(State(RNG.nonNegativeInt).map(_ % 2 == 0))

  def double: Gen[Double] =
    Gen(State(RNG.double1))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  // 8.4
  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State(RNG.nonNegativeInt).map(n => start + n % (stopExclusive - start)))

  // 8.7
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean.flatMap(b => if (b) g1 else g2)

  // 8.8
  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] =
    double.flatMap(d => {
      val threshold = g1._2 / (g1._2 + g2._2)
      if (d < threshold) g1._1
      else g2._1
    })


}

//trait Gen[A] {
//  def map[A,B](f: A => B): Gen[B] = ???
//  def flatMap[A,B](f: A => Gen[B]): Gen[B] = ???
//}

case class Gen[A](sample: State[RNG, A]) {
  // 8.6
  def flatMap[B](f: A => Gen[B]): Gen[B] =
    f(sample)

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

}

trait SGen[+A] {

}

