

/**
  * 0. Background: Program Writing Paradigm
  * * Structured
  * * Object Oriented
  * * Functional
  * (Start with Option)
  */

val a = 10
val x: Option[Int] = None
val y: Option[Int] = Some(600)
val z: Option[Int] = Some(700)

val result = for {
    result1 <- x
    result2 <- y
} yield result1 + result2

for {
    r1 <- y
    r2 <- z
} yield r1 + r2

//is equivalent to :
val result1 = y.flatMap((yItem: Int) => z.map((zItem: Int) => yItem + zItem))


/**
  * 1. Generators with for comprehension, with filter.
  */
val xCoordinates = (0 to 20).toList
val yCoordinates = (0 to 10).toList
for {
    x <- xCoordinates if x % 2 == 0
    y <- yCoordinates if y % 2 == 0
} yield (x, y)

/**
  * How you do that in your way ?
  */

/**
  * 2. Method, Function and Chaining.
  */

/**
  * Method does not have instance, object.
  */
def sqr(a: Int): Int = a * a
def dbl(a: Int): Int = a * 2
dbl(sqr(4)) // >> Left to Right
sqr(dbl(4)) // >> Right to Left

/**
  * Functions has instance, object.
  */
val square: Int => Int = (a: Int) => a * a
def double: Int => Int = (a: Int) => a * 2
(double andThen square) (4) // >> Left to Right
(double compose square) (4) // >> Right to Left


/**
  * 3. Higher order function.
  */

def all(price: Int): Boolean = true
def even(price: Int): Boolean = price % 2 == 0
def odd(price: Int): Boolean = !even(price)
def accumulatedPrice(prices: List[Int], selector: Int => Boolean): Int =
    prices.filter(selector).sum

val prices = List(10, 15, 20, 25, 30, 31, 35, 40)
accumulatedPrice(prices, all)
accumulatedPrice(prices, even)
accumulatedPrice(prices, { x => x % 2 != 0 })

/**
  * 4. Programming with Safe computation.
  */
/*lazy val input = for {
    a <- Try(Some(Console.readInt())).getOrElse(None)
    b <- Try(Some(Console.readInt())).getOrElse(None)
} yield a + b */


/**
  * 5. Spelling Checking from text area with preserved positions.
  */
val dictionary = List("hello", "from", "the", "world")
val sourceText = "Hello   from the   Semantro world"

/**
  * 6. Data extraction and transformation for reporting.
  */
//Example 6

/**
  * 7. State demo.
  */
case class State[Id, Argument](run: Id => (Argument, Id))

def greetingsFrom(name: String): State[Int, String] = State {
    id => (s"Hello $name with $id to ", id + 1)
}

val pravin = greetingsFrom("Pravin")
pravin.run(100)

