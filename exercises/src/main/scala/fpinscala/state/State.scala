package fpinscala.state


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

  // 6.1
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (next, rng2) = rng.nextInt
    if (next == Int.MinValue) (0, rng2)
    else (math.abs(next), rng2)
  }

  // 6.2
  def double(rng: RNG): (Double, RNG) = {
    val (next, rng2) = nonNegativeInt(rng)
    (1 + next.toDouble) / Int.MaxValue.toDouble -> rng2
  }

  // 6.3
  def intDouble(rng: RNG): ((Int,Double), RNG) = {
    val (nextInt, rng2) = rng.nextInt
    val (nextDouble, rng3) = double(rng2)
    (nextInt, nextDouble) -> rng3
  }

  def doubleInt(rng: RNG): ((Double,Int), RNG) = {
    val (rand, rng2) = intDouble(rng)
    (rand._2, rand._1) -> rng2
  }

  def double3(rng: RNG): ((Double,Double,Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    (d1, d2, d3) -> rng4
  }

  // 6.4
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def go(count: Int, r: RNG, xs: List[Int]): (List[Int], RNG) =
      if (count == 0)
        xs -> r
      else {
        val (num, r2) = r.nextInt
        go(count-1, r2, num :: xs)
      }
    go(count, rng, List.empty)
  }

  // 6.5
  val double1: Rand[Double] = map(r => r.nextInt) (i =>
    i.toDouble / (1+Int.MaxValue).toDouble
  )

  // 6.6
  def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] = rng => {
    val (a, rng2) = rng(ra)
    val (b, rng3) = rng2(rb)
    f(a, b) -> rng3
  }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)(_ -> _)

  // 6.7
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = r => {
    def go(fs: List[Rand[A]], r: RNG, xs: List[A]): (List[A], RNG) = fs match {
      case Nil => (xs, r)
      case h :: t => {
        val (x, r2) = h(r)
        go(t, r2, x :: xs)
      }
    }
    go(fs, r, Nil)
  }

  def sequenceFromAnswers[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldRight(unit(List.empty[A]))((f, acc) =>
      map2(f, acc)(_ :: _)
    )
  // It's interesting that we never actually need to talk about the `RNG` value
  // in `sequence`. This is a strong hint that we could make this function
  // polymorphic in that type.




  def ints1(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))



  // 6.8
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = rng => {
    val (a, rng2) = f(rng)
    g(a)(rng2)
  }

  // 6.9
  def _map[A, B](s: Rand[A])(f: A => B): Rand[B] = flatMap(s)(a => unit(f(a)))

  def _map2[A, B, C](sa: Rand[A], sb: Rand[B])(f: (A, B) => C): Rand[C] = flatMap(sa)(a => _map(sb)(b => f(a, b)))
}

case class State[S,+A](run: S => (A, S)) {
  // 6.10
  import State._
  def map[B](f: A => B): State[S, B] = flatMap(a => unit(f(a)))

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = flatMap(a => sb.map(b => f(a, b)))

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(s => {
    val (a, s2) = run(s)
    f(a).run(s2)
  })
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def unit[S,A](a: A): State[S, A] = State(s => (a, s))

  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldRight(unit[S, List[A]](List.empty[A]))((f, acc) =>
      f.map2(acc)(_ :: _)
    )

  def get[S]: State[S, S] = State(s => s -> s)

  def set[S](s: S): State[S, Unit] = State(_ => () -> s)

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
