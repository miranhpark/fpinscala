// Purely functional state
// Notation: if a comment starts with __, it's from the text - otherwise, I wrote it, yay!

/** - As an example of thinking about state: random number generation using the standard library
  * (using scala.util.Random) relies on side effects (mutation of some internal state)
  * to generate random numbers - this makes it difficult to test/reproduce and isn't
  * composable, modular, or easily parallelized - indeed to another instance of the "same"
  * generator, it must have its methods called with the same seed *and* the same number of times
  * - If we instead make state updates explicit (return the new state along with the generated
  * value) we can recover referential transparency - we're separating computation of the
  * next state from the communication the new state
  * - Can translate functions that mutate an object into a pure API by passing what would
  * compute the next state (p. 82 for an example)
  */

/** - For many of the examples the functions have the type RNG => (A, RNG) for some A - we call
  * these "state actions" or "state transitions" because they transform RNG states
  * - State actions can be combined using "combinators" - higher-order functions (nice because
  * it's tedious to pass the state manually)
  * - We can make an alias for this entire type:
  * e.g. type Rand[+A] = RNG => (A, RNG)
  * which is a state action = a *program* that depends on an RNG and uses it to generate an A
  * while also transitions the RNG to a new state (that can be used later)
  */
import RNG._
import State._

object StateMain {
  def main(args: Array[String]): Unit = {
    // Test away!
    val x = SimpleRNG(1)
//    println(x)
//    println(nonNegativeInt(x))
//    println(double(x))
//    println(intDouble(x))
//    println(doubleInt(x))
//    println(double3(x))
//    println(ints(3)(x))
//    println(map(nonNegativeInt)(i => i - i % 2)(x))
//    println(doubleViaMap(x))
//    println(map2(unit(1), unit(2))((a, b) => (a, b))(x))
//    println(map2(double, double)((a, b) => (a, b))(x))
//    println(sequence(List(doubleViaMap, doubleViaMap, double(_)))(x))
//    println(intsViaSequence(5)(x))
//    println(nonNegativeLessThan(10)(x))
//    println(flatMap(doubleViaMap)(x => unit(x))(x))
//    println(nonNegativeLessThanViaFlatMap(10)(x))
//    println(mapViaFlatMap(nonNegativeInt)(i => i - i % 2)(x))
//    println(map2ViaFlatMap(double, double)((a, b) => (a, b))(x))

    val gumballs = Machine(true, 14, 1)
    println(action(Turn).run(gumballs))
    println(simulateMachine(List(Coin, Turn)).run(gumballs))
  }
}

trait RNG {
  def nextInt: (Int, RNG)
}

object RNG {
  case class SimpleRNG(seed: Long) extends RNG {
    // __Uses a "linear congruential generator" to generate pseudo-random numbers
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // __Use current seed to generate new seed
      val nextRNG = SimpleRNG(newSeed) // __Next state (RNG from new seed)
      val n = (newSeed >>> 16).toInt // __Right binary shift w/ zero fill - n is the new random number
      (n, nextRNG) // __A tuple containing both the random number and next state
    }
  }

  // A function that uses RNG.nextInt to generate a random integer between 0 and Int.MaxValue (inclusive)
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (n, rng2) = rng.nextInt
    if (n == Int.MinValue) (0, rng2) // Not sure what the right value is to map here but seems sufficient?
    else if (n < 0) (Math.abs(n), rng2)
    else (n, rng2)
  }

  // A function to generate a Double between 0 and 1 (not inclusive of 1)
  def double(rng: RNG): (Double, RNG) = {
    val (n, rng2) = nonNegativeInt(rng)
    if (n == Int.MaxValue) (0, rng2) // Same comment as above
    else (n / Int.MaxValue.toDouble, rng2)
  }

  // A function that generates an (Int, Double) pair
  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, rng2) = rng.nextInt
    val (d, rng3) = double(rng2)
    ((i, d), rng3)
  }

  // A function that generates a (Double, Int) pair
  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((i, d), rng2) = intDouble(rng)
    ((d, i), rng2)
  }

  // A function that returns a (Double, Double, Double) 3-tuple
  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, rng2) = double(rng)
    val (d2, rng3) = double(rng2)
    val (d3, rng4) = double(rng3)
    ((d1, d2, d3), rng4)
  }

  // A function to generate a list of random integers
  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
//    val (n, rng2) = rng.nextInt
//    if (count == 0) (List(), rng)
//    else (List(n) ++ ints(count - 1)(rng2)._1, rng2) // works, but a little ugly
    @annotation.tailrec
      def go(ns: List[Int], rng: RNG, count: Int): (List[Int], RNG) = {
        val (n, rng2) = rng.nextInt
        if (count < 1) (ns, rng)
        else go(n :: ns, rng2, count - 1)
      }
    go(List(), rng, count)
  }

  // __Type alias for RNG => (A, RNG)
  type Rand[+A] = RNG => (A, RNG)

  // __Passes the RNG state without using it, returning a constant value
  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  // __Transforms the output of a state action without modifying the state
  // - looks like a Functor to me :)!
  def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  // double reimplemented using map
  def doubleViaMap: Rand[Double] =
    map(nonNegativeInt)(n => n / Int.MaxValue.toDouble) // This includes 1 but it's not hard to change

  // Takes two actions, ra and rb, and a function f for combining their results,
  // returning a new action that combines them - smells like an Applicative!
  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  // Combine a list of RNG transitions into a single transition
  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = fs match {
    case Nil => unit(Nil)
    case h :: t => map2(h, sequence(t))(_ :: _)
  }

  // ints reimplemented using sequence (List.fill(n)(x) makes a list with x repeated n times)
  def intsViaSequence(count: Int): Rand[List[Int]] =
    sequence(List.fill(count)(_.nextInt))

  // __Generates an integer between 0 (inclusive) and n (exclusive)
  def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
    val (i, rng2) = nonNegativeInt(rng)
    val mod = i % n
    if (i + (n - 1) - mod >= 0) (mod, rng2)
    else nonNegativeLessThan(n)(rng)
  }

  // It's flatMap ¯\_(ツ)_/¯
  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = { rng =>
    val (n, rng2) = f(rng)
    val (n2, rng3) = g(n)(rng2)
    (n2, rng3)
  }

  // nonNegativeLessThan implemented using flatMap
  def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => if (i + (n - 1) - i % n >= 0) unit(i % n) else nonNegativeLessThanViaFlatMap(n))

  // map reimplemented using flatMap
  def mapViaFlatMap[A,B](s: Rand[A])(f: A => B): Rand[B] =
    flatMap(s)(a => unit(f(a)))

  // map2 reimplemented using flatMap
  def map2ViaFlatMap[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    flatMap(ra)(a => (rng => (f(a, rb(rng)._1), rb(rng)._2)))
}

/** - State is a computation that carries some state along (or state action, state transition, statement)
  * e.g. we can use State to make  type alias for Rand:
  * type Rand[A] = State[RNG, A]
  * - We can also define the functions unit, map, map2, flatMap, and sequence for any generic state
  */

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    State(s => {
      val s2 = this.run(s)
      (f(s2._1), s2._2)
    })

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    State(s => {
      val (a, s2) = this.run(s)
      val (b, s3) = sb.run(s2)
      (f(a, b), s3)
    })

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    State({ s =>
      val (a, s2) = this.run(s)
      val (b, s3) = f(a).run(s2)
      (b, s3)
    })
}

/** - A lot of the assigning vals and running state actions looks a bit like imperative programming
  * (since it's a sequence of statements modifying the program state)
  * - To facilitate a kind of imperative programming with for-comprehensions or flatMaps
  * we really only need two primitive State combinators - reading (get) and writing (set) the state
  * (see State companion object below)
  */

/** A candy dispenser!
  * Two types of input: 1) insert a coin 2) turn the knob to dispense candy
  * Two types of state: 1) locked 2) unlocked
  * Tracks: 1) how many candies are left 2) how many coins it contains
  *
  * Rules:
  * 1) Insert coin: locked => unlocked if candy > 0
  * 2) Turning knob: unlocked => dispense candy, lock
  * 3) Turning knob: locked => nothing!
  * 4) Insert coin: unlocked => nothing!
  * 5) No candy? Ignores all inputs!
  *
  * Operates based on list of inputs and returns number of coins and candies left
  */

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {

  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  // __Modifies state in arbitrary ways
  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()

  //__Passes the incoming state and returns it as the value
  def get[S]: State[S, S] = State(s => (s, s))

  //__Sets the new state, ignoring the incoming state, replacing it with a new state
  // and returning () instead of a meaningful value
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  // Takes an input action and depending on the action, modifies the machine state
  def action(input: Input): State[Machine, Unit] = input match {
    case Coin => modify((s1: Machine) => coinAction(s1))
    case Turn => modify((s1: Machine) => turnAction(s1))
  }

  // A function that changes the machine state based on a coin
  def coinAction(s: Machine): Machine = {
    s match {
      case Machine(_, 0, _) => s // no candies? no action!
      case Machine(false, _, _) => s // unlocked? no action!
      case Machine(true, a, b) => Machine(false, a, b + 1) // locked? unlock and increment coin count
    }
  }

  // A function that changes the machine state based on a turn
  def turnAction(s: Machine): Machine = {
    s match {
      case Machine(_, 0, _) => s // no candies? no action!
      case Machine(true, a, b) => s // locked? no action!
      case Machine(false, a, b) => Machine(true, a - 1, b) // unlocked? dispense a candy and decrease candy count
    }
  }

  // Given a list of actions, simulates a candy dispenser according to our rules above
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val end = inputs.flatMap(i => List(action(i)))
      .foldRight(unit(()): State[Machine, Unit])(
        //        ((x: State[Machine, Unit], y: State[Machine, Unit]) => State((s: Machine) => y.run((x.run(s)._2)))) // ugly ver.
        //        (x, y) => x.flatMap(a => y)) // now with more flatMap!
        (x, y) => x.map2(y)((a, b) => b)) // now with more map2!

    end.flatMap(a => get).map(m => (m.candies, m.coins)) // now with more flatMap!
//    end.map2(get)((a, b) => (b.candies, b.coins)) // now with more map2!
    // can also do with sequence, traverse, etc.!
  }
}


