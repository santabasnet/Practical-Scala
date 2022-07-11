package com.semantro.learn

import scala.util.Try

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-07-20.
  */
object TotalComputation extends App {

    /**
      * Read two numbers from file and calculate the sum.
      */
    println("Give two numbers: ")
    lazy val inputSum = for {
        a <- Try(Some(Console.readInt())).getOrElse(None)//f1
        b <- Try(Some(Console.readInt())).getOrElse(None)//f2
    } yield a + b

    //inputSum = f1 @ f2//Scalaz

    println(inputSum)

}
