package fpinscala.errorhandling


import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`, since we are writing our own in this chapter

sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] = this match {
    case Some(a) => Some(f(a))
    case None => None
  }

  def getOrElse[B>:A](default: => B): B =  this match {
    case Some(b) => b
    case None => default
  }

  def flatMap[B](f: A => Option[B]): Option[B] = this.map(f).getOrElse(None)

  def orElse[B>:A](ob: => Option[B]): Option[B] = this.map(Some.apply).getOrElse(ob)

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
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap { m =>
      mean(xs.map(x => math.pow(x - m, 2)))
    }
  }

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap { aa =>
      b.map { bb =>
        f(aa, bb)
      }
    }
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    a.foldRight[Option[List[A]]](Some(Nil))((i, acc) => map2(i, acc)(_ :: _))

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((i, acc) => map2(f(i), acc)(_ :: _))

  def sequenceFromTraverse[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)
}

object OptionTest {

  private val testMap: Boolean = {
    Some(12).map(_.toString) == Some("12") &&
    None.map(_.toString) == None
  }

  private val testGetOrElse: Boolean = {
    Some(12).getOrElse(13) == 12 &&
    None.getOrElse(13) == 13
  }

  private val testFlatMap: Boolean = {
    Some(12).flatMap(i => Some(i)) == Some(12) &&
    None.flatMap(_ => None) == None &&
    Some(1).flatMap(_ => None) == None
  }

  private val testOrElse: Boolean = {
    Some(12).orElse(Some(13)) == Some(12) &&
    None.orElse(Some(13)) == Some(13) &&
    None.orElse(None) == None
  }

  private val testFilter: Boolean = {
    Some(20).filter(_ < 10) == None &&
    Some(20).filter(_ == 20) == Some(20) &&
    None.filter(_ => true) == None
  }

  private val testVariance: Boolean = {
    Option.variance(Seq(1.0, 2.0, 3.0)) == Some(0.6666666666666666)
  }

  private val testMap2: Boolean = {
    Option.map2(Some(1), Some(2))(_ + _) == Some(3) &&
    Option.map2(Some(1), None)(_ + _) == None
  }

  private val testSequence: Boolean = {
    Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)) &&
    Option.sequence(List(Some(1), None, Some(3))) == None &&
    Option.sequenceFromTraverse(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3))
  }

  private val testTraverse: Boolean = {
    Option.traverse(List(1, 2, 3))(Some.apply) == Some(List(1, 2, 3)) &&
    Option.traverse(List(1, 2, 3))(_ => None) == None
  }

  def main(args: Array[String]): Unit = {
    println(testMap)
    println(testGetOrElse)
    println(testFlatMap)
    println(testOrElse)
    println(testFilter)
    println(testVariance)
    println(testMap2)
    println(testSequence)
    println(testTraverse)
  }

}