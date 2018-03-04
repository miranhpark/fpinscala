// Functional data structures are by definition immutable

// package fpinscala.datastructures

// List data type
sealed trait List[+A] // +A means A is covariant - if X is a subtype of Y, List[X] is a subtype of List[Y]
case object Nil extends List[Nothing] // Constructor representing empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Constructor representing nonempty lists

object List { // "List" companion object - contains list functions
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0
    case Cons(x, xs) => x + sum(xs) // The sum of a list starting with x is x plus the sum of the rest of the list
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
//    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax - accepts 0 or more arguments
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  // Remove the first element of a list (constant time)
  def tail[A](ds: List[A]): List[A] = ds match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  // Replace the first element of a List with a different value
  def setHead[A](ds: List[A], newHead: A): List[A] = ds match {
    case Nil => Nil
    case Cons(_, t) => Cons(newHead, t)
  }

  // Remove the first n elements from a list (time proportional to number of elements being dropped)
  def drop[A] (l: List[A], n: Int): List[A] = {
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }
  }

  // Remove elements from a the List prefix as long as they match a predicate
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) if f(h) => dropWhile(t, f)
      case _ => l
    }
  }

  // Returns a List consisting of all but the last element of a List
  // (not constant time like tail because have to go through all list elements?)
  def init[A](l: List[A]): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  // Normally when we call an anonymous function for f we have to specify the type of its argument
  // Scala can infer type of second argument if grouped into two argument lists
  // type info flows left to right across argument groups, multiple arguments maximizes type inference
  // dropWhile2 is curried
  // Usage:
  // val xs: List[Int] = List(1,2,3,4,5)
  // dropWhile2(xs)(x => x < 4)
  def dropWhile2[A](as: List[A])(f: A => Boolean): List[A] =
    as match {
      case Cons(h, t) if f(h) => dropWhile2(t)(f)
      case _ => as
    }

  // Our sum and product definitions are very similar in structure so let's generalize
  // by pulling subexpressions out into function arguments
  // Create a new function that takes as arugments the value to return in the case of the empty list
  // and the function to add an element to the result in the case of a nonempty list
  // foldRight is not specific to any one type of element
  // The value returned doesn't have to be of the same type as the elements of the list
  // foldRight must traverse all the way to the end of the list before it can begin collapsing
  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = { // (f) after lets type inference determine input types
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  // Compute the sum of a list (using foldRight)
  def sumRight(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  // Compute the product of a list (using foldRight)
  def productRight(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // more concise notation for (x,y) => x * y

  // product2 can't immediately halt recursion if it encounters a 0 as evaluation occurs after construction?
  // foldRight can take the list constructor itself

  // Compute the length of a list (using foldRight)
  def lengthRight[A](as: List[A]): Int = {
    foldRight(as, 0)((_, count) => count + 1)
  }

  def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
    }
  }

  // Compute the sum of a list (using foldLeft)
  def sumLeft(ns: List[Int]) =
    foldLeft(ns, 0)(_ + _)

  // Compute the product of a list (using foldLeft)
  def productLeft(ns: List[Int]) =
    foldLeft(ns, 1)(_ * _)

  // Compute the length of a list (using foldLeft)
  def lengthLeft[A](as: List[A]): Int = {
    foldLeft(as, 0)((count, _) => count + 1)
  }

  // Return the reverse of a list
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((l, a) => Cons(a, l))
  }
}