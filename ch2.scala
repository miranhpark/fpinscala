// comment
/* also comment */
/** doc comment */

object MyModule { // declares class and its only instance (singleton)
	def abs(n: Int): Int = // takes integer and returns integer
		if (n < 0) -n // returns negation if negative
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
			else go(n-1, fib2, fib1 + fib2)
	    go(n, 0, 1)
    }

    def formatResult(name: String, n: Int, f: Int => Int) = { 
		/* private method - MyModule members only; 
		f is required to be a function from Int to Int */
        val msg = "The %s of %d is %d." // string + placeholders
        msg.format(name, n, f(n)) // values for placeholders
    }

    def main(Args: Array[String]): Unit = { // Unit is like void 
        println(formatResult("absolute value", -42, abs))
        println(formatResult("factorial", 7, factorial))
		println(formatResult("Fibonacci sequence number", 7, fib_r))
		println(formatResult("Fibonacci sequence number", 7, fib_tr))
	}
}