package com.semantro.learn

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-08-19.
  */
object Basic {

    /**
      * Factorial of a number.
      * tail recursion
       */
    def factorial(n: BigInt): BigInt = {
        if (n <= 1) 1
        else n * factorial(n - 1)
    }

    def efficientRecursion(n: BigInt): BigInt = {
        def fact(nInternal: BigInt, result: BigInt = 1): BigInt =
            if (nInternal <= 1) result else fact(nInternal-1, nInternal * result)
        fact(n)
    }

    /**
      * How Recursion works
      * 1. Requirements: base condition (termination condition)
      *     n <= 0.
      * 2. Self computation.
      *    n * factorial.
      *    Definition.
      *    n! means = n * (n-1)!
      *
      * Steps: Expansion.
      *     n = 5
      *     0. factorial(5), r = 1
      *     1. 5 * factorial(4), r = 5
      *     2. 5 * 4 * factorial(3), r = 20
      *     3. 5 * 4 * 3 * factorial(2), r = 60
      *     4. 5 * 4 * 3 * 2 * factorial(1), r = 120
      *     5. 5 * 4 * 3 * 2 * 1
      *     6. 20 * 3 * 2 * 1
      *     7. 60 * 2 * 1
      *     8. 120 * 1
      *     9. 120
      */
    /**
      * Sum of n numbers.
      * (n * n - 1) ??
      * @param n
      * @return
      */
    def sumOf(n:Int): Int = if(n <= 1) 1 else n + sumOf(n-1)

    /**
      *
      * @param args
      */

    def main(args: Array[String]): Unit = {
        //println("Result: " + factorial(100))
        //println("Result: " + efficientRecursion(1000))
        //println("Sum of 10: " + sumOf(10))

        /**
          * Generate order pair.
          * has 2 dimension.
          * x => List(4, 5, 6, 9)
          * y => List(8, 10, 2, 4)
          */
        val x = List(4, 5, 6, 9, 7)
        val y = List(8, 10, 2, 4, 14, 5, 10)
        val result = for {
            i <- x
            j <- y
        } yield (i, j)

        //result.foreach(print)

    }

    /**
      * Procedural Programming(Structured Programming)
      * 1. Step by step (Sequence of) execution.
      *     a = 5
      *     b = 10
      *     c = a+b
      *     print c
      * 2. Selection of execution.
      *     if (expr)
      *         execution
      *     else
      *         execution
      *
      *     switch(expr)
      *         exe1
      *         exe2
      *
      * 3. Iteration of execution
      *     for(;;)
      *         execution
      */

    /**
      * Functional programming has different idea of execution.
      * Lazy evaluation.
      *
      * function: domain => range.
      */

    val data = (0 to 99).toList
    /**
      * 1. split: fixed size, 10 elements.
      * 2. parallel collection.
      * 3. each -> sum {split computation}
      * -------- shuffle()
      * -------- compute(node)
      * 4. total -> reduce to final sum.
      * (Apache spark)
      */
    //val result = data.grouped(10).toList.par.map(items => items.sum).sum
    //println(result)


    val v1 = Vector(1, 1, 0, 0, 0, 1, 0)
    val v2 = Vector(1, 1, 0, 0, 1, 0, 0)
    //Max overlaps
    //1. count zeros
    //2. count_zeros(v1) => 4 * 60 = 240 mins
    //3. count_zeros(v2) => 4 * 60 = 240 mins

    //a. zip
    //b. group all availables(0s)
    //[(1,1),(1,1)],[(0,0),(0,0)],[(0,1),(1,0)],[(0,0)]
    //[(0,0),(0,0)], [(0,0)]
    //Sort by length
    //[(0,0),(0,0)], [(0,0)]
    //take first

    val xx = (0 to 10).toList
    // for comprehension, generate each element from xx and print them.
    //for (a <- xx) println(a)

    //xx.groupBy -> list to map
    //I want to convert/map a list to a hashmap
    //groupby/flatten, inverse function.

    //println(xx.groupBy(e => e%2==0).tail)
    //println(xx.groupBy(e => e%2==0).head)

    //iterate
    //group
    //slide
    //fold

    // Read two numbers from console and print the sum.

    // first 100 prime numbers.
    // isPrime? true/false

    //2, 3, 5, 7, 9 ... 100th
    //square
    //sum

    //while(prime_counter to 100) increase counter if isprime true.
    //while(number_counter...??? infinite loop)

    def isPrime(x: Int) = (2 to x/2).forall(a => x%a!=0)
    val results = Stream.from(2)
        .filter(isPrime).take(100).map(x => x * x).sum
    println(results)
    //Lazy evaluation


}
