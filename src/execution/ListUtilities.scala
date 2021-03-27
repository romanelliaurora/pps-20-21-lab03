package execution

import u03.Lists.List
import u03.Lists.List._
import u02.Optionals.Option._
import u02.Optionals.Option
import u02.SumTypes._
import scala.annotation.tailrec

object ListUtilities {

  @tailrec
  def drop(lst: List[Int], i: Int): List[Int] = lst match {
    case Cons(_, t) if i > 0 => drop(t, i - 1)
    case _ => lst
  }


  def flatMapL[A, B](lst: List[A])(f: A => List[B]): List[B] = lst match {
    case Cons(h, t) => append(f(h), flatMapL(t)(f))
    case Nil() => Nil()
  }


  def map2[A, B](l: List[A])(mapper: A => B): List[B] = flatMapL(l)(A => Cons(mapper(A), Nil()))

  def filter2[A](l1: List[A])(pred: A => Boolean): List[A] = flatMapL(l1)(A => if (pred(A)) Cons(A, Nil()) else Nil())

  def max(l: List[Int]): Option[Int] = {

    @tailrec
    def _max(l: List[Int], maximum: Int): Option[Int] = l match {
      case Cons(h, t) if h < maximum => _max(t, maximum)
      case Cons(h, t) if h > maximum => _max(t, h)
      case Nil() => if (maximum == Integer.MIN_VALUE)  None() else Some(maximum)
    }

    _max(l, Integer.MIN_VALUE)

  }
  def getCourses(l: List[Person]): List[String] = flatMapL[Person, String](l) {
    case Teacher(_, c) => Cons(c, Nil())
    case _ => Nil()
  }

/*
  def getCourses2[A](l: List[Person]):List[A] = l match {
    case Cons(Teacher(_,c), t)  => Cons ( c , getCourses2(t))
    case _ => Nil()
  }
*/
}