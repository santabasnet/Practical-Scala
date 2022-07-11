package com.semantro.learn

import scala.util.Try

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-07-18.
  */
object ChainCompose {

    def main(args: Array[String]):Unit = {

        /**
          * Method does not have instance, object.
          */
        def sqr(a: Int): Int = a * a
        def dbl(a: Int): Int = a * 2
        println(dbl(sqr(4))) // >> Left to Right
        println(sqr(dbl(4))) // >> Right to Left

        /**
          * Functions has instance, object.
          */
        val square: Int => Int = (a: Int) => a * a
        def double: Int => Int = (a: Int) => a * 2
        println((double andThen square) (4)) // >> Left to Right
        println((double compose square) (4)) // >> Right to Left

        val join = (items: List[Int]) => items mkString ","
        println(join(List(100, 300, 45)))
        println(join.apply(List(100, 300, 45)))

        /**
          * Options to Chain.
          */

        /**
          * Either to Chain.
          */

        /**
          * Simple loop
          */
        for (a <- (0 to 10).toList) println(a)

        //For comprehension as generator: Order pair
        val x = List(1, 2, 3)
        val y = List(5, 6, 7, 8)
        val result = for {a <- x; b <- y} yield (a, b)
        println(result)


        //For comprehension with side effects.
        /*lazy val input = for {
            a <- Try(Some(Console.readInt())).getOrElse(None) if a > 100
            b <- Try(Some(Console.readInt())).getOrElse(None)
        } yield a + b

        println(input)*/


        def isPrime(x:Int) = (2 to x/2).forall(i => x % i != 0)
        Stream.from(2).filter(isPrime).take(100).foreach(println)


    }

}
