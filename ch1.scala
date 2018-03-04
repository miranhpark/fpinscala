// val x = "Hello, World"
// val r1 = x.reverse
// Referentially transparent - x is unchanged
// val x = new StringBuilder("Hello")
// val r1 = x.append(", World").toString
// No longer referentially transparent - x is changed

object HelloWorld{
	def main(args:Array[String]): Unit = {
		println("Hello, Twitch!")
	}
}
