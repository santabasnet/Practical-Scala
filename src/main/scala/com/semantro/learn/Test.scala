package com.semantro.learn

import scala.util.Try

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-09-03.
  */
object Test extends App {
    // Read two numbers from console and print the sum.

    val sum = for {
        x <- Try(Console.readInt()).toOption
        y <- Try(Console.readInt()).toOption
    } yield x + y

    println(sum)

    //input/output, partial: total
    //{klajsdf, 890234, 82347, l;aksdf ...}

    def isPrime(x: Int): Boolean = (2 to x/2).forall(a => x%a!=0)

    //maps int to boolean
    //List to Int: sum, head, min, max, avg,
    //Json[15000] =>  Import to DB: Ruby on Rails
    //Json[15000] HashMap map(filter) to Json[5000] HashMap save

    // Computation is: transform from domain to range. Alanzo Church
    // Computation is:  sequence of finite steps. Alan Turing
    // They both are same.
    // Church - Turing Thesis
    println(Stream.from(2).filter(isPrime).take(100).map(x => x * x).sum)
    // sequence of execution
    // selection of execution
    // iteration of execution




}
