package fpinscala.datastructures

import scala.annotation.tailrec

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
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y // it chooses this one
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


  def tail[A](l: List[A]): List[A] = l match {
    case Cons(_, t) => t
    case Nil => Nil
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Cons(_, t) => Cons(h, t)
    case Nil => Cons(h, Nil)
  }

  @tailrec
  def drop[A](l: List[A], n: Int): List[A] = l match {
    case Nil => Nil
    case c: Cons[A] if n == 0 => c
    case Cons(_, t) => drop(t, n - 1)
  }

  @tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case other => other
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(h, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sumLeft(l: List[Int]): Int = foldLeft(l, 0)(_ + _)

  def productLeft(l: List[Double]): Double = foldLeft(l, 1.0)(_ * _)

  def lengthLeft[A](l: List[A]): Int = foldLeft(l, 0)((acc, _) => acc + 1)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, Nil: List[A])((acc, i) => Cons(i, acc))

  def foldRightFromFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => B): B = foldLeft(reverse(as), z)((i, j) => f(j, i))

  def append2[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)(Cons(_,_))

  def concat[A](l: List[List[A]]): List[A] = foldRight(l, Nil: List[A])(append)

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRightFromFoldLeft(l, Nil: List[B])((h,t) => Cons(f(h),t))

  def filter[A](l: List[A])(f: A => Boolean): List[A] = foldRightFromFoldLeft(l, Nil: List[A])((i, acc) => if (f(i)) Cons(i, acc) else acc)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] = concat(map(l)(f))

  def zipWith[A, B, C](a: List[A], b: List[B])(f: (A, B) => C): List[C] = (a, b) match {
    case (Nil, _) | (_, Nil) => Nil
    case (Cons(i, j), Cons(k, l)) => Cons(f(i, k), zipWith(j, l)(f))
  }
}

object ListTests {

  private val testMatch = List.x == 3

  private val testTail: Boolean = {
    val list = List(1, 2, 3, 4)
    List.tail(list) == List(2, 3, 4) &&
    List.tail(List(1)) == Nil &&
    List.tail(Nil) == Nil
  }

  private val testSetHead: Boolean = {
    List.setHead(List(1, 2, 3), 4) == List(4, 2, 3) &&
    List.setHead(Nil, 5) == List(5)
  }

  private val testDrop: Boolean = {
    List.drop(List(1, 2, 3, 4), 1) == List(2, 3, 4) &&
    List.drop(List(1), 10) == Nil &&
    List.drop(Nil, 10) == Nil &&
    List.drop(List(1, 2, 3), 0) == List(1, 2, 3)
  }

  private val testDropWhile: Boolean = {
    List.dropWhile[Int](List(1, 2, 3, 0, 0, 0), _ > 0) == List(0, 0, 0) &&
    List.dropWhile[Int](List(0, 1, 2, 3, 0, 0, 0), _ > 0) == List(0, 1, 2, 3, 0, 0, 0) &&
    List.dropWhile[Int](Nil, _ > 0) == Nil
  }

  private val testInit: Boolean = {
    List.init(List(1, 2, 3, 4)) == List(1, 2, 3) &&
    List.init(List(1)) == Nil &&
    List.init(Nil) == Nil
  }

  private val testLength: Boolean = {
    List.length(Nil) == 0 &&
    List.length(List(1)) == 1 &&
    List.length(List(1, 2, 3, 4, 5)) == 5 &&
    List.length(List(1, 2, 3, 4, 5)) == List.lengthLeft(List(1, 2, 3, 4, 5))
  }

  private val testFoldLeft: Boolean = {
    List.foldLeft[Int, Int](Nil, 0)(_ + _) == 0 &&
    List.foldLeft(List(1, 2, 3), 10)(_ * _) == 60 &&
    List.foldRightFromFoldLeft(List(1, 2, 3), 10)(_ * _) == 60
  }

  private val testReverse: Boolean = {
    List.reverse(List(1, 2, 3)) == List(3, 2, 1) &&
    List.reverse(Nil) == Nil &&
    List.reverse(List(1)) == List(1)
  }

  private val testAppend: Boolean = {
    List.append2(List(1, 2), List(3, 4)) == List(1, 2, 3, 4)
  }

  private val testConcat: Boolean = {
    List.concat(List(List(1, 2, 3), List(4, 5, 6))) == List(1, 2, 3, 4, 5, 6)
  }

  private val testMap: Boolean = {
    List.map(List(1, 2, 3))(_.toString) == List("1", "2", "3") &&
    List.flatMap(List(1, 2, 3))(i => List(i, i)) == List(1, 1, 2, 2, 3, 3)
  }

  private val testFilter: Boolean = {
    List.filter(List(1, 2, 3, 4))(_ % 2 == 0) == List(2, 4)
  }

  private val testZipWith: Boolean = {
    List.zipWith(List(1, 2, 3), List("A", "B", "C"))((a, b) => b + a.toString) == List("A1", "B2", "C3")
  }

  def main(args: Array[String]): Unit = {
    println(testMatch)
    println(testTail)
    println(testSetHead)
    println(testDrop)
    println(testDropWhile)
    println(testInit)
    println(testLength)
    println(testFoldLeft)
    println(testReverse)
    println(testAppend)
    println(testConcat)
    println(testMap)
    println(testFilter)
    println(testZipWith)
  }
}
