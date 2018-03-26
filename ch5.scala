// Strictness and laziness, streams

/** - Non-strictness is the property is a function -
  * a function may choose not to evaluate one or more of its arguments
  * - Strict functions always evaluate arguments (and are more typical
  * across programming languages)
  * - If the evaluation of an expression runs forever or throws an error
  * instead of returning a definite value, it doesn't "terminate"/it evaluates
  * to the "bottom" - a function f is strict if f(x) evaluates to the bottom
  * for all x that evaluate to bottom
   */

/** - Can write non-strict functions in Scala by accepting some arguments unevaluated
  * e.g. passing in a value onTrue: () => A or onTrue: => A
  * - The unevaluated expression is called a "thunk" and we can force it to evaluate later
  * - A non-strict function in Scala takes arguments "by name" rather than "by value"
  */

/** "smart" constructors are functions that construct a datatype ensuring some additional
  * invariant or different signature than the "real" constructor used for pattern matching
  * (convention is to lowercase the first letter)
  */

/** - Separation of concerns is important, i.e. separating the description of computations
  * from actually running them - e.g. with Stream we can build up a computation that
  * produces a sequence of elements without running the computation untill we need them
  * - We can then describe a "larger" expression than we need and evaluate a portion
  */

/** - The implementations we're doing are incremental - they don't fully generate answers -
  * logic is "interleaved" (some people call streams "first-class loops")
  * - Since streams aren't generated, transformations only require enough working memory
  * to store and transform the current element
  * - Because of the incrementality, functons here work on infinite streams (neato!!)
  * e.g. val ones: Stream[Int] = Stream.cons(1, ones)
  */

import Stream._
import scala.collection.immutable.{Stream => _}

sealed trait Stream[+A] {
  // Converts a Stream to a List (forces eval and lets you look at it in the REPL)
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList // have to force the thunk
  }

  // Returns the first n elements of a Stream
  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if (n == 1) => cons(h(), empty)
    case Cons(h, t) if (n > 1) => cons(h(), t().take(n - 1))
    case _ => empty
  }

  // Skips the first n elements of a Stream
  def drop(n: Int): Stream[A] = this match {
    case Empty => Empty
    case Cons(h, t) if (n > 0) => t().drop(n - 1)
    case _ => this
  }

  // Return all starting elements of a Stream that match a given predicate
  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => Empty
  }

  // Checks if an element matching a Boolean function exists in a Stream
  // (the || is non-strict in the second argument: if p(h()) returns true
  // we can terminate traversal early - saving any execution of the tail)
  def exists(p: A => Boolean): Boolean = this match {
    case Cons(h, t) => p(h()) || t().exists(p)
    case _ => false
  }

  // foldRight for Stream - similar to List, but f is non-strict in its
  // second parameter which means it can terminate traversal early (no recursion)
  def foldRight[B](z: => B)(f: (A, => B) => B): B =
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f))
      case _ => z
    }

  // exists implemented using foldRight
  // (not stack-safe if the stream is large and all elements return false)
  def existsViaFoldRight(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Checks that all elements in the stream match a given predicate,
  // terminating the traversal when encountering a non-matching value
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // takeWhile implemented using foldRight
  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty[A])

  // headOption using foldRight
  def headOption: Option[A] =
    foldRight(None: Option[A])((h, t) => Some(h))

  // map using foldRight
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  // filter using foldRight
  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  // append using foldRight (non-strict in its argument)
  // appending a Stream, not a value - kinda like concatTwoLists as before
  def append[B >: A](appendVal: => Stream[B]): Stream[B] =
    foldRight(appendVal)((a, b) => cons(a, b))

  // flatMap using foldRight
  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a).append(b))

  // map implemented using unfold
  def mapViaUnfold[B](f: A => B): Stream[B] =
    unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case _ => None
    }

  // take implemented using unfold
  def takeViaUnfold(n: Int): Stream[A] =
    unfold((this, n)) {
      case (Cons(h, t), 1) => Some((h(), (t(), 0)))
      case (Cons(h, t), n) if n > 1 => Some((h(), (t(), n - 1)))
      case _ => None
    }

  // takeWhile implemented using unfold
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if (p(h())) => Some((h(), t()))
      case _ => None
    }

  // zipWith implemented using unfold (see Ch. 3)
  def zipWithViaUnfold[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] =
    unfold((this, s2)) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((f(h1(), h2()), (t1(), t2())))
      case _ => None
    }

  // zipAll implemented using unfold
  // continues traversal as long as either stream has more elements
  def zipAllViaUnfold[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
    unfold(this, s2) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(((Some(h1()), Some(h2())), (t1(), t2())))
      case (Cons(h1, t1), Empty) => Some(((Some(h1()), None), (t1(), empty)))
      case (Empty, Cons(h2, t2)) => Some(((None, Some(h2())), (empty, t2())))
      case _ => None
    }

  // Checks if one Stream is a prefix of another
  // (like the function in Ch. 3 for List)
  def startsWith[A](s: Stream[A]): Boolean =
    this.zipAllViaUnfold(s).takeWhile(x => x._2 != None).forAll(x => x._1 == x._2)

  // Returns the Stream of suffixes of the given input sequence using unfold
  // e.g. Stream(1,2,3) returns Stream(Stream(1,2,3), Stream(2,3), Stream(3), Stream())
  // test with x.tails.map(_.toList).toList - doesn't seem to display the empty though
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case Cons(h, t) => Some((cons(h(), t()), t()))
      case _ => None
    }.append(empty)

  // Check whether a Stream contains another Stream as a subsequence
  // (like the List version in Ch. 3)f
  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)
    //this.tails.exists(_.startsWith(s))

  // scanRight - like foldRight that returns a stream of intermediate results
  // e.g. Stream(1,2,3).scanRight(0)(_ + _).toList returns List(6,5,3,0)
  // from the computation List(1+2+3+0, 2+3+0, 3+0, 0)
  def scanRight[B](z: B)(f: (A, => B) => B): Stream[B] =
  // inefficient solution: this.tails.map(_.foldRight(z)(f)).append(Stream(z))
  // Ended up just looking this one up - makes sense if you look carefully at the
  // type signatures, but was really difficult to visualize so I'm annotating heavily!

  // Our base case is our base value and a new "empty" Stream end with that value
  // (streamItem, accTuple) has type signature (A, (B, Stream[B]))
    foldRight((z, Stream(z)))((streamItem, accTuple) => {
      // Works without lazy val, but see note below about why (p0 = accTuple)
      // "p0 is passed by-name and used in by-name args in f and cons. So use lazy val to ensure only one evaluation..."
      lazy val lazyAccTuple = accTuple
      // Return next accumulation of function evaluation of next Stream item and last accumulated value
      // Use this value to prepend Stream
      (f(streamItem, lazyAccTuple._1), cons(f(streamItem, lazyAccTuple._1), lazyAccTuple._2))
    })._2 // retrieve just the Stream part of the tuple

}
case object Empty extends Stream[Nothing]
case class Cons[+A] (h: () => A, t: () => Stream[A]) extends Stream[A]
// A nonempty stream has a head and tail which are non-strict
// Due to technical limitations the above thunks must be explicitly forced

object Stream {
  // A smart constructor for creating a nonempty stream
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  // A smart constructor for creating an empty stream
  def empty[A]: Stream[A] = Empty

  // A variable-argument method for constructing a stream from multiple elements
  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // Returns an infinite stream of a given value
  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  // Generates an infinite stream of integers, starting from n
  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  // Generates infinite stream of Fibonacci numbers
  def fibs: Stream[Int] = {
    def go(fib1: Int, fib2: Int): Stream[Int] =
      cons(fib1, go(fib2, fib1 + fib2))
    go(0, 1)
  }

  // ** Potential extra credit? Footnotes say it's possible to write
  // a stack-safe forAll using an ordinary recursive loop

  // Takes an initial state and a function for producing both
  // the next state and the next value in the generated stream
  def unfold[A,S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((h, t)) => cons(h, unfold(t)(f))
    }
  }

  // fibs implemented using unfold
  def fibsViaUnfold: Stream[Int] =
    unfold((1, 1)){case (fib1, fib2) => Some(fib1, (fib2, fib1 + fib2))}

  // from implemented using unfold
  def fromViaUnfold(n: Int): Stream[Int] =
    unfold(n)(x => Some(x, x + 1))

  // constant implemented using unfold
  def constantViaUnfold[A](a: A): Stream[A] =
    unfold(a)(x => Some(x, x))

  // ones implemented using unfold
  def onesViaUnfold: Stream[Int] =
    unfold(1)(x => Some(x, x))
}

object StreamMain {
  def main(args: Array[String]): Unit = {
    val x = Stream(1,2,3).scanRight(0)(_ + _)
    println(x.toList)
  }
}