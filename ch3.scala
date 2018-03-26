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
  // Create a new function that takes as arguments the value to return in the case of the empty list
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

  // foldRight is not tail-recursive and will cause StackOverflowError for large lists (not "stack-safe")
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
  def lengthLeft[A](as: List[A]): Int =
    foldLeft(as, 0)((count, _) => count + 1)


  // Return the reverse of a list
  def reverse[A](as: List[A]): List[A] =
    foldLeft(as, List[A]())((xs, x) => Cons(x, xs))


  // *** foldLeft in terms of foldRight
//  def foldLeftByFoldRight[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
//  }

  // *** foldRight in terms of foldLeft (nice because of tail-recursiveness)
//  def foldRightByFoldLeft[A,B](as: List[A], z: B)(f: (A, B) => A): A = {
//  }

  // Append a list using foldRight
  def appendRight[A](as: List[A], appendVal: A): List[A] =
    foldRight(as, List[A](appendVal))((x, xs) => Cons(x, xs))

  // Append a list using foldLeft
  def appendLeft[A](as: List[A], appendVal: A): List[A] =
    foldLeft(as, List[A](appendVal))((xs, x) => Cons(x, xs))

  // Concatenates a list of lists into a single list
  // Runtime should be linear in the total length of all lists
  def concatTwoLists[A](as1: List[A], as2: List[A]): List[A] =
    foldRight(as1, as2)((x, xs) => Cons(x, xs))

  def concatList[A](as: List[List[A]]): List[A] = {
    foldRight(as, List[A]())(concatTwoLists)
  }

  // Transform a list of integers by adding 1 to each element
  def addOne(ns: List[Int]): List[Int] = {
    foldRight(ns, List[Int]())((x, xs) => Cons(x + 1, xs))
  }

  // Turn each value in a List[Double] into a String
  // can use d.toString to convert d: Double to a String
  def doubleToString(ns: List[Double]): List[String] = {
    foldRight(ns, List[String]())((x, xs) => Cons(x.toString, xs))
  }

  // A function that generalizes modifying each list while maintaining the structure of that list
  def map[A,B](as: List[A])(f: A => B): List[B] =
    foldRight(as, List[B]())((x, xs) => Cons(f(x), xs))

  // A function that removes items from a list unless they satisfy a given predicate
  def filter[A](as: List[A])(f: A => Boolean): List[A] =
    foldRight(as, List[A]())((x, xs) => if (f(x)) Cons(x, xs) else xs)

  // A function that works like map but returns a list instead of a single result
  // and that list should be inserted into the final resulting list
  // e.g. flatMap(List(1,2,3))(i => List(i,i)) => List(1,1,2,2,3,3)
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] =
    foldRight(as, List[B]())((x, xs) => concatTwoLists(f(x),xs))

  // A function that performs a filter using flatMap
  def filterByFlatMap[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(x => if (f(x)) Cons(x, Nil) else Nil)

  // Takes two lists and performs element-wise addition
  // e.g. (List(1,2,3),List(4,5,6)) => List(5,7,9)
  def zipAdd(as1: List[Int], as2: List[Int]): List[Int] = {
    (as1, as2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons((x1 + x2), zipAdd(xs1, xs2))
    }
  }

  // Takes two lists and performs an operation element-wise
  def zipWith[A](as1: List[A], as2: List[A])(f: (A, A) => A): List[A] = {
    (as1, as2) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x1, xs1), Cons(x2, xs2)) => Cons(f(x1, x2), zipWith(xs1, xs2)(f))
    }
  }

  // List exists in the Scala standard library: Cons => :: (right-associative)
  // Case Cons(h, t) => case h :: t
  // Don't have to nest parentheses if writing a pattern like case h :: h2 :: t
  // to act on more than just first element of a list

  // Extra credit - standard library functions:

  // Returns a list consisting of the first n elements
  def take[A](as: List[A], n: Int): List[A] = {
     as match {
       case Cons(x, xs) if (n == 1) => Cons(x, Nil)
       case Cons(x, xs) if (n > 0) => Cons(x, take(xs, n - 1))
       case _ => Nil
     }
  }

  // Returns a list consisting of the longest
  // valid prefix whose elements all pass the predicate f
  def takeWhile[A](as: List[A])(f: A => Boolean): List[A] = {
    as match {
      case Cons(x, xs) if f(x) => Cons(x, takeWhile(xs)(f))
      case _ => Nil
    }
  }

  //  Returns true if and only if all elements pass the predicate f
  def forall[A](as: List[A])(f: A => Boolean): Boolean = {
    as match {
      case Nil => true
      case Cons(h, t) if f(h) => forall(t)(f)
      case _ => false
    }
  }

  // Returns true if any element of this passes the predicate f
  def exists[A](as: List[A])(f: A => Boolean): Boolean = {
    as match {
      case Nil => false
      case Cons(h, t) if !f(h) => exists(t)(f)
      case _ => true
    }
  }

  // Return list of partial results (rather than final accumulated value like foldLeft, foldRight)
  def scanLeft[A,B](as: List[A], z: B)(f: (A, B) => B): List[B] = {
//    as match {
//      case Nil => Cons(z, Nil)
//      case Cons(x, xs) => Cons(z, scanLeft(xs, f(x, z))(f))
//    }
    foldLeft(as, (z, List[B]()))(( acc: (B, List[B]), h: A ) => {
        val x = f(h, acc._1)
        (x, appendRight(acc._2, x))
      })._2
  }

  def scanRight[A,B](as: List[A], z: B)(f: (A, B) => B): List[B] = {
//      case Nil => Cons(z, Nil)
//      case Cons(x, xs) => Cons(f(x, z), scanRight(xs, z)(f))
    appendRight(reverse(scanLeft(reverse(as), z)(f)), z)
    // this is a total copout but I'm going to figure this out in the future
    }

  // Back to exercises from the book:
  // Check whether a List contains another List as a subsequence
  def startsWith[A](sup: List[A], sub: List[A]): Boolean = {
    (sup, sub) match {
      case (_, Nil) => true
      case (Cons(x1, xs1), Cons(x2, xs2)) if (x1 == x2) => startsWith(xs1, xs2)
      case _ => false
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup match {
      case Nil => false
      case _ if startsWith(sup, sub) => true
      case Cons(x, xs) => hasSubsequence(xs, sub)
    }
  }
}

// Tree data structure
sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  // Counts the number of nodes (leaves and branches) in a tree
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l, r) => size(l) + size(r) + 1
    }
  }

  // Returns the maximum element in a Tree[Int]
  // can use x.max(y) or x max y to compute the maximum of integers x and y
  def maximum(t: Tree[Int]): Int = {
    t match {
      case Leaf(x) => x
      case Branch(l, r) => maximum(l).max(maximum(r))
    }
  }

  // Returns the maximum path length from the root of a tree to any leaf
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(l, r) => depth(l).max(depth(r)) + 1
    }
  }

  // Modify each element in a tree with a given function
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(x) => Leaf(f(x))
      case Branch(l, r) => Branch(map(l)(f), map(r)(f))
    }
  }

  // Generalized function for operations on trees
  def fold[A,B](t: Tree[A])(f: A => B)(b: (B, B) => B): B = {
    t match {
      case Leaf(x) => f(x)
      case Branch(l, r) => b(fold(l)(f)(b), fold(r)(f)(b))
    }
  }

  // Counts the number of nodes (leaves and branches) in a tree using fold
  def foldSize(t: Tree[Int]): Int =
    fold(t)(x => 1)((l, r) => l + r + 1)

  // Returns the maximum element in a Tree[Int] using fold
  def foldMaximum(t: Tree[Int]): Int =
    fold(t)(x => x)((l, r) => l.max(r))

  // Returns the maximum path length from the root of a tree to any leaf using fold
  def foldDepth[A](t: Tree[A]): Int =
    fold(t)(x => 0)((l, r) => (l + 1).max(r + 1))

  // Modify each element in a tree with a given function using fold
  def foldMap[A,B](t: Tree[A])(f: A => B): Tree[B] =
    fold(t)(x => Leaf(f(x)): Tree[B])((l, r) => Branch(l, r))
  // I kept having the issue on this that there was a type mismatch where
  // the Branch(l, r) part was expecting a Leaf[B]! It's noted that this
  // is a consequence of subtype inference in Scala so just be aware =_=
}

