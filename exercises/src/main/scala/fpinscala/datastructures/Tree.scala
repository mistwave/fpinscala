package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maximum(tree: Tree[Int]): Int = tree match {
    case Leaf(x) => x
    case Branch(l, r) => maximum(l) max maximum(r)
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }


  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B, B) => B): B = tree match {
    case Leaf(x) => f(x)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def size1[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)(1 + _ + _)

  /*
  size1(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))))
  fold(B(L1, B(L2, L3))) (_ => 1)-->f (1 + _ + _)-->g
  g(fold(L1)(f)(g), fold(B(L2,L3))(f)(g))
    fold(L1)(f)(g)
    f(1)
    1

    fold(B(L2, L3))(f)(g)
    g(fold(L2)(f)(g), fold(L3)(f)(g))
    g(1, 1)
    3

  g(1, 3)
  5
   */


  def maximun1(tree: Tree[Int]): Int =
    fold(tree)(b => b)(_ + _)

  def depth1[A](tree: Tree[A]): Int =
    fold(tree)(_ => 1)((l, r) => 1 + (l max r))

  def map1[A, B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(a => Leaf(f(a)): Tree[B])(Branch(_, _))

  /*
  map1(B(L1, B(L2, L3))) (_.toString)
  fold(B(L1, B(L2, L3))) (a => Leaf(a.toString))-->f (Branch(_, _))-->g
  g(fold(L1) f g, fold(B(L2,L3)) f g)
    fold(L1) f g
    f(1)
    Leaf("1")

    fold(B(L2, L3)) f g
    g(fold(L2) f g, fold(L3) f g)
    g(Leaf("2"), Leaf("3"))
    Branch(Leaf("2"), Leaf("3"))

  Branch(Leaf("1"), Branch(Leaf("2", "3")))
   */





}