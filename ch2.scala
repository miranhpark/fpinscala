// comment
/* also comment */
/** doc comment */

// Compile: scalac simple.scala
// Run: scala simple

// :load simple.scala
// Can access MyModule.function

// import MyModule._
// Imports all functions

object MyModule { // Declares class and its only instance (singleton)
	def abs(n: Int): Int = // Takes integer and returns integer
		if (n < 0) -n // Returns negation if negative
	    else n
		
	def factorial(n: Int): Int = {
		def go(n: Int, acc:Int): Int = 
			if (n <= 0) acc
			else go(n - 1, n*acc)
		go(n,1)
	}
		
	def fib_r(n: Int): Int = {
		if (n <= 1) 0
		else if (n == 2) 1
		else fib_r(n - 1) + fib_r(n - 2)
	}
	
  def fib_tr(n: Int): Int = {
    def go(n: Int, fib1: Int, fib2: Int): Int =
    if (n <= 1) fib1
    else go(n - 1, fib2, fib1 + fib2)
    go(n, 0, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
  /* private method - MyModule members only;
  f is required to be a function from Int to Int */
      val msg = "The %s of %d is %d." // String + placeholders
      msg.format(name, n, f(n)) // Values for placeholders
  }

  def main(Args: Array[String]): Unit = { // Unit is like void
    println(formatResult("absolute value", -42, abs))
    println(formatResult("factorial", 7, factorial))
    println(formatResult("Fibonacci sequence number", 7, fib_r))
    println(formatResult("Fibonacci sequence number", 7, fib_tr))
	}

  // Return the first index in an array where a key occurs
	// second argument is "anonymous function" or "function literal" defined inline
	def findFirst[A](as: Array[A], p: A => Boolean): Int = {
		// Usage:
		// val firstArray = Array(1,2,3,4,5)
		// findFirst(firstArray, (key: Int) => key == 3)
    // Note: (key: Int) => key == 3 is a "function literal" or "anonymous function"
		@annotation.tailrec
		def loop(n: Int): Int =
			if (n >= as.length) -1
			else if (p(as(n))) n
			else loop(n + 1)

		loop(0)
	}

  // Checks whether an Array[A] is sorted according to a given comparison function
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    // Usage:
    // val sortedArray = Array(1,2,3,4,5)
    // isSorted(sortedArray, (A: Int, B: Int) => A < B)
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (!(ordered(as(n - 1), as(n)))) false
      else if (n >= as.length - 1) true
      else loop(n + 1)

    loop(1)
  }

  // Currying: convert a function of of two arguments into a function of one argument
  // that partially applies f
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    (a: A) => ((b: B) => f(a,b))
  }

  // Uncurrying: reverse the transformation of curry
  // A => (B => C) can be written as A => B => C (associativity)
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a: A)(b: B)
  }

  // Function composition: feeds the output of one function into
  // the input of another function
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a:A))
  }
}







