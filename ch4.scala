// Exception handling
import scala.{Option => _, Some => _, Either => _, _} // hide std library `Option`, `Some` and `Either`
// also use the :paste feature for the REPL or it will break!

// The Option data type has two cases:
// - Some if defined
// - None if not defined
// Good for handing invalid inputs!
// e.g. mean is a "partial function" - not defined for some inputs
// (makes assumptions about inputs that are not implied by input types)
// by using Option, we create a "total function" - takes each value of
// the input to exactly one value of the output type

// Can implement all of the following except map and getOrElse without pattern matching

sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] = this match {
    case None => None
    case Some(a) => Some(f(a))
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
//  this match {
//    case None => None
//    case Some(a) => f(a)
      this.map(f).getOrElse(None)
  }

  // Returns the result inside the Some case of Option,
  // if None then returns default given value
  // B >: A says B type parameter must be a supertype of A
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // Returns the first Option if it's defined, otherwise returns second option
  def orElse[B >: A](ob: => Option[B]): Option[B] = {
//    this match {
//    case None => ob
//    case Some(a) => Some(a)
    Some(this).getOrElse(ob)
  }

  def filter(f: A => Boolean): Option[A] = {
//  this match {
//    case Some(a) if (f(a)) => Some(a)
//    case _ => None
    this.flatMap(a => if (f(a)) Some(a) else None)
  }

}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  // Mean of a list of doubles
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  // Implementation of variance using flatMap
  // Variance = mean(math.pow(x - mean, 2)) for each element x in the sequence
  // get the mean of the sequence => pass into flatMap => pass into map =>
  // given the mean value, map the variance function onto each element =>
  // take the mean of the sequence of individual squared deviances via flatMap
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
}