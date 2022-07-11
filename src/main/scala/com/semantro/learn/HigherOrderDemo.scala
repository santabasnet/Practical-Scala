package com.semantro.learn

import shapeless.HList._
import shapeless._

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-07-18.
  */

object HigherOrderDemo {

    abstract class Writer {
        def write(message: String)
    }

    class StringWriter extends Writer {
        val target = new StringBuilder
        override def write(message: String): Unit = target.append(message)
        override def toString: String = target.toString()
    }

    trait UpperCaseFilter extends Writer {
        abstract override def write(message: String): Unit = super.write(message.toUpperCase)
    }

    trait ProfanityFilter extends Writer {
        abstract override def write(message: String): Unit = super.write(message.replaceAll("stupid", "s*****"))
    }

    def writeStuff(writer: Writer): Unit = {
        writer.write("This is stupid.")
        println(writer)
    }

    /**
      * Tuple unpacking for class instance generation.
      */
    case class Person(name: String, age: Int)
    object Person {
        def of(name: String, age: Int): Person = Person(name, age)
        def from(tuple: (String, Int)): Person = (of _).tupled(tuple)
    }

    def listToHList(x: List[_]): HList = if(x == Nil) HNil else x.head :: listToHList(x.tail)

    def test(f: Int => Int = _ + 1): Int => Int = f

    def main(args: Array[String]): Unit = {
        println("Hello World !")

        writeStuff(new StringWriter)
        writeStuff(new StringWriter with UpperCaseFilter)
        writeStuff(new StringWriter with UpperCaseFilter with ProfanityFilter)

        println(test()(2))
        val nameAndAge = ("Santa", 100)
        println(Person.from(nameAndAge))
        val a = List("Santa", 100)
        val hList = listToHList(a)
        println(hList)

    }

}
