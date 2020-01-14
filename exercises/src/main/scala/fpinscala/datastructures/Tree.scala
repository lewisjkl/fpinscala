package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {


    def size[A](t: Tree[A]): Int = t match {
        case _: Leaf[A] => 1
        case Branch(l, r) => 1 + size(l) + size(r)
    }

    def maxDepth[A](t: Tree[A]): Int = t match {
        case _: Leaf[A] => 0
        case Branch(l, r) => 1 + maxDepth(l) max maxDepth(r)
    }

    def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
      case Leaf(i) => Leaf(f(i))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }

    def fold[A, B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
      case Leaf(i) => f(i)
      case Branch(i, j) => g(fold(i)(f)(g), fold(j)(f)(g))
    }

    def sizeFromFold[A](t: Tree[A]): Int = fold(t)(_ => 1)((i, j) => 1 + i + j)

    def maxDepthFromFold[A](t: Tree[A]): Int = fold(t)(_ => 0)((i, j) => 1 + i max j)

    def mapFromFold[A, B](t: Tree[A])(f: A => B): Tree[B] = fold(t)(i => Leaf(f(i)): Tree[B])(Branch(_,_))

}

object TreeTests {

    private val testSize: Boolean = {
      Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 5 &&
      Tree.sizeFromFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 5
    }

    private val testMaxDepth: Boolean = {
      Tree.maxDepth(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 2 &&
      Tree.maxDepthFromFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) == 2
    }

    private val testMap: Boolean = {
      Tree.map(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_.toString) == Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3")) &&
      Tree.mapFromFold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_.toString) == Branch(Branch(Leaf("1"), Leaf("2")), Leaf("3"))
    }

    private val testFold: Boolean = {
      Tree.fold(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3)))(_.toString)(_ + _) == "123"
    }

    def main(args: Array[String]): Unit = {
      println(testSize)
      println(testMaxDepth)
      println(testMap)
      println(testFold)
    }

}