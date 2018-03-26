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

  def flatMap[B](f: A => Option[B]): Option[B] =
  //  this match {
  //    case None => None
  //    case Some(a) => f(a)
  //  }
      this.map(f).getOrElse(None)


  // Returns the result inside the Some case of Option,
  // if None then returns default given value
  // B >: A says B type parameter must be a supertype of A
  def getOrElse[B >: A](default: => B): B = this match {
    case None => default
    case Some(a) => a
  }

  // Returns the first Option if it's defined, otherwise returns second option
  def orElse[B >: A](ob: => Option[B]): Option[B] =
  //  this match {
  //    case None => ob
  //    case Some(a) => Some(a)
  //  }
    Some(this).getOrElse(ob)


  def filter(f: A => Boolean): Option[A] =
  //  this match {
  //    case Some(a) if (f(a)) => Some(a)
  //    case _ => None
  //  }
    this.flatMap(a => if (f(a)) Some(a) else None)

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

  // General-purpose function to convert from an exception-based to Option-oriented API
  // Uses a non-strict or lazy argument, catching any exceptions and converting them to None
  def Try[A](a: => A): Option[A] =
    try Some(a)
    catch {case e: Exception => None} // discards information about the error

  // A generic function that combines two Option values using a binary function
  // - notice similarity in structure to above!
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(ai => b.map(bi => f(ai, bi)))


  // A function that combines a list of Options into one Option containing a
  // list of all the Some values in the original list - if the original list contains
  // None even once, the result of the function should be None
  def sequence[A](a: List[Option[A]]): Option[List[A]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  // A function that maps over a list using a function that might fail,
  // returning None if applying it to any element of the list returns None
  // - can do with map and sequence but this looks at the list twice
  // - can also implement sequence in terms of traverse
  def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => map2(f(h), traverse(t)(f))(_ :: _)
  }

  def sequence2[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(ai => ai)
}

// The Either data type has two cases like Option, but both cases carry a value
// Represents values that can be one of two things (disjoint union of two types)
// Can use it to indicate success or failure and give more information
// By convention the Right constructor is reserved for the success case
// (Left for failure - here the E is for error)

// Standard library Option and Either don't have sequence, traverse, map2
// Standard library Either doesn't define right-biased flatMap
// Check out the API!

sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => Right(f(a))
  }

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match {
    case Left(e) => Left(e)
    case Right(a) => f(a)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = this match {
    case Left(_) => b
    case Right(a) => Right(a)
  }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    this.flatMap(ai => b.map(bi => f(ai, bi)))
}

case class Left[+E](value: E) extends Either[E, Nothing]
case class Right[+A](value: A) extends Either[Nothing, A]

object Either {
  // Like Option, a function Try which factors out the common pattern of
  // converting thrown exceptions to values
  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e)}

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = as match {
    case Nil => Right(Nil)
    case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
  }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(esi => esi)
}

case class Person(name: Name, age: Age)
sealed class Name(val value: String)
sealed class Age(val value: Int)

object Person {
  def mkName(name: String): Either[String, Name] =
    if (name == "" || name == null) Left("Name is empty.")
    else Right(new Name(name))

  def mkAge(age: Int): Either[String, Age] =
    if (age < 0) Left("Age is out of range.")
    else Right(new Age(age))

  // As is, map2 can only report one error (name error) even if age is invalid as well
  // e.g. val x = Person.mkPerson("",-1) returns Left(Name is empty.)
  // It's possible to accumulate errors as an altered Either - see Cats' validation for example
  // Could also change the type signature of mkPerson to carry errors, but a bit clunky
  // - see Bartosz's YouTube lecture on Kleisli for a good example
  def mkPerson(name: String, age: Int): Either[String, Person] =
    mkName(name).map2(mkAge(age))(Person(_, _))
}








