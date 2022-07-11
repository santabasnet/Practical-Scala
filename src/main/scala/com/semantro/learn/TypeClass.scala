package com.semantro.learn

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-07-20.
  */
object TypeClass {

    trait Exp
    case class Quantity(value: Int) extends Exp
    case class Add[A <: Exp, B <: Exp](left: A, right: B) extends Exp
    case class Mult[A <: Exp, B <: Exp](left: A, right: B) extends Exp

    /**
      * Class type definition.
      * @tparam E
      */
    trait Eval[E] {
        def eval(e: E): Int
    }

    /**
      * Implicit type class instance.
      */
    implicit def litEval = new Eval[Quantity] {
        def eval(l: Quantity) = l.value
    }

    implicit def addEval[A <: Exp, B <: Exp](implicit e1: Eval[A], e2: Eval[B]) = new Eval[Add[A, B]] {
        def eval(a: Add[A, B]) = e1.eval(a.left) + e2.eval(a.right)
    }

    implicit def mulEval[A <: Exp, B <: Exp](implicit e1: Eval[A], e2: Eval[B]) = new Eval[Mult[A, B]] {
        def eval(m : Mult[A, B]) = e1.eval(m.left) * e2.eval(m.right)
    }

    /**
      * Interface using implicit parameters.
      * @param exp
      * @param e
      * @tparam A
      * @return
      */
    def evaluate[A <: Exp](exp: A)(implicit e : Eval[A]) = {
        e.eval(exp)
    }

    trait Print[P] {
        def print(p: P): Unit
    }

    implicit def litPrint = new Print[Quantity] {
        def print(l: Quantity) = Console.print(l.value)
    }

    implicit def addPrint[A <: Exp, B <: Exp](implicit p1: Print[A], p2 : Print[B]) = new Print[Add[A, B]] {
        def print(a : Add[A, B]) = { Console.print("( "); p1.print(a.left); Console.print(" + "); p2.print(a.right); Console.print(" )");}
    }

    implicit def mulPrint[A <: Exp, B <: Exp](implicit p1: Print[A], p2: Print[B]) = new Print[Mult[A, B]] {
        def print(m : Mult[A, B]) = { Console.print("( "); p1.print(m.left); Console.print(" * "); p2.print(m.right); Console.print(" )");}
    }

    def printExpressions[A <: Exp](exp : A)(implicit p : Print[A]) = {
        p.print(exp)
    }

    def main(args: Array[String]) = {
        val exp1 = Mult(Add(Quantity(3), Quantity(4)), Quantity(7))
        println("Expression : " + exp1)
        printExpressions(exp1)
        println(", Evaluated to : " + evaluate(exp1))
    }
}
