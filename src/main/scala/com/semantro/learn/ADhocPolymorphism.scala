package com.semantro.learn

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-07-22.
  */
object ADhocPolymorphism extends App {

    /**
      * Provides extension for polymorphic operations on existing libraries, objects etc.
      * Ex1: Type class definition requires:
      * 1. Actual definition of type class
      * 2. Type class instance of type implicit.
      * 3. Interface using implicit parameters.
      */

    case class User(name: String, age: Int)
    object User {
        def demo = User("Demo User", 45)
    }
    case class Response(status: Boolean, code: Int)
    object ResponseStatus {
        def offline = Response(status = false, code = 400)
    }

    /**
      * Type class definition.
      */
    type XML = String
    trait XmlWriter[T] {
        def convertToXML(instance: T): XML
    }

    /**
      * Type class instance for xml writer in terms of function.
      */
    implicit val userXMLWriter: XmlWriter[User] = (user: User) => {
        s"""
           |<xml>
           |    <user> ${user.name} </xml>
           |    <age> ${user.age} </age>
           |</xml>
         """.stripMargin
    }

    /**
      * Type class instance for xml writer.
      */
    implicit val responseXMLWriter: XmlWriter[Response] = (response: Response) => {
        s"""
           |<xml>
           |    <code> ${response.code} </code>
           |    <status> ${response.status} </status>
           |</xml>
         """.stripMargin
    }

    /**
      * Interface with implicit parameters.
      */
    def xmlWriterInterface[T](aType: T)(implicit writer: XmlWriter[T]): XML = writer.convertToXML(aType)

    println(xmlWriterInterface(User.demo))
    println(xmlWriterInterface(ResponseStatus.offline))


    /**
      * Ex2: Very powerful concepts: every computable concepts are implementable with implicits.
      */

    type Result = String

    implicit def anyToString(a: Any): String = a.toString

    trait Addition[T] {
        def compute(a: T, b: T): Result
    }

    trait Multiplication[T] {
        def compute(a: T, b: T): Result
    }

    implicit val IntAddition: Addition[Int] = (a: Int, b: Int) => a + b
    implicit val StringAddition: Addition[String] = (a: String, b: String) => List(a, b).mkString("-")
    implicit val IntMultiplication: Multiplication[Int] = (a: Int, b: Int) => a * b
    implicit val StringMultiplication: Multiplication[String] = (first: String, second: String) => (for {
        ch1 <- first
        ch2 <- second
    } yield List[String](ch1, ch2)).flatten.mkString

    def summation[T](a: T, b: T)(implicit addition: Addition[T]): Result = addition.compute(a, b)

    def multiplication[T](a: T, b: T)(implicit multiplication: Multiplication[T]): Result = multiplication.compute(a, b)

    /**
      * Examples - Usage.
      */
    println(summation(100, 300))
    println(summation("Ram", "Sita"))
    println(multiplication(100, 300))
    println(multiplication("Ram", "Sita"))
    println("\n=====================")

    /**
      * Example 3.
      */
    //1. Type class definition.
    trait Ordering[T] {
        def compare(x: T, y: T): Int
    }

    //2. Type class instance in terms of object.
    implicit object IntOrdering extends Ordering[Int] {
        def compare(x: Int, y: Int): Int = x - y
    }

    implicit object StringOrdering extends Ordering[String] {
        def compare(x: String, y: String): Int = x.length - y.length
    }

    def min[T](a: T, b: T)(implicit o: Ordering[T]): T = if (o.compare(a, b) < 0) a else b

    def max[T](a: T, b: T)(implicit o: Ordering[T]): T = if (implicitly[Ordering[T]].compare(a, b) > 0) a else b

    def longer[T](a: T, b: T)(implicit o: Ordering[T]): T = if (o.compare(a, b) > 0) a else b

    println(min(400, 100))
    println(max(100, 800))
    println(longer("Santa", "Basnet"))
    println(longer("Basnet", "Santa"))
    println("\n========================")


    /**
      * Upper and lower type bounds.
      */
    trait DATE
    class Year extends DATE
    class Month extends Year
    class Day extends Month
    // We need to restrict parking to all subtypes of vehicle, above Tricycle
    class Salary[T <: Month](val value: T)
    class Commitment[T >: Month] (val value: T)

    val p2 = new Salary[Month](new Month)
    //val p3 = new Salary[Year](new Year)
    val p3 = new Commitment[Year](new Year)
    //val p4 = new Commitment[Day](new Day)


    /**
      * Expression Evaluation: from Martin Odersky (Integer Evaluation.)
      * 1. Type class definition/
      */
    type Output = Option[Int]
    trait Exp
    case class Quantity(value: Int) extends Exp
    case class Add[A <: Exp, B <: Exp](left: A, right: B) extends Exp // Upper type bound i.e. A should be the sub type of Exp. (type restrictions.)
    case class Mult[A <: Exp, B <: Exp](left: A, right: B) extends Exp // Upper type bound i.e. A should be the sub type of Exp. (type restrictions.)
    trait Computation[E] {
        def evaluate(e: E): Output
    }
    /**
      * 2. Implicit instance of type classes.
      */
    implicit def quantityOf = new Computation[Quantity] {
        def evaluate(quantity: Quantity) = Some(quantity.value)
    }
    implicit def addEvaluation[A <: Exp, B <: Exp](implicit c1: Computation[A], c2: Computation[B]) = new Computation[Add[A, B]] {
        def evaluate(exp: Add[A, B]) = for {
            a <- c1.evaluate(exp.left)
            b <- c2.evaluate(exp.right)
        } yield a + b

    }
    implicit def multiplyEvaluation[A <: Exp, B <: Exp](implicit c1: Computation[A], c2: Computation[B]) = new Computation[Mult[A, B]] {
        def evaluate(exp: Mult[A, B]) = for {
            a <- c1.evaluate(exp.left)
            b <- c2.evaluate(exp.right)
        } yield a * b
    }
    /**
      * 3. Define Interface function with implicit parameters.
      */
    def compute[A <: Exp](exp: A)(implicit c: Computation[A]) = {
        c.evaluate(exp)
    }
    trait Print[P] {
        def print(p: P): Unit
    }
    implicit def quantityPrint = new Print[Quantity] {
        def print(quantity: Quantity) = Console.print(quantity.value)
    }
    implicit def additionPrint[A <: Exp, B <: Exp](implicit p1: Print[A], p2: Print[B]) = new Print[Add[A, B]] {
        def print(a: Add[A, B]) = {
            Console.print("( ")
            p1.print(a.left)
            Console.print(" + ")
            p2.print(a.right)
            Console.print(" )")
        }
    }

    implicit def multiplyPrint[A <: Exp, B <: Exp](implicit p1: Print[A], p2: Print[B]) = new Print[Mult[A, B]] {
        def print(m: Mult[A, B]) = {
            Console.print("( ")
            p1.print(m.left)
            Console.print(" * ")
            p2.print(m.right)
            Console.print(" )")
        }
    }

    def printExpression[A <: Exp](exp: A)(implicit p: Print[A]) = {
        p.print(exp)
    }

    /**
      * Define another type for printing the results.
      */
    val exp1 = Mult(Add(Quantity(3), Quantity(4)), Quantity(7))
    println("Expression : " + exp1)
    printExpression(exp1)
    println(" is Evaluated to : " + compute(exp1) + ".")


}
