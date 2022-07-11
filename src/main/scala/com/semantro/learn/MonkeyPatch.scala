package com.semantro.learn

/**
 * This class is a part of the package com.semantro.learn and the package
 * is a part of the project learn.
 *
 * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
 * https://www.integratedict.com.np
 *
 * Created by Santa on 2021-07-26.
 */
object MonkeyPatch {

    case class Distance(quantity: Double, unit: String = "m") {
        override def toString: String = List(quantity, unit).mkString(" ")

        def +(that: Distance): Distance = that.unit match {
            case "m" => Distance(this.quantity + that.quantity)
            case "cm" => Distance(this.quantity + that.quantity / 100.0d)
        }

        def -(that: Distance): Distance = that.unit match {
            case "m" => Distance(this.quantity - that.quantity)
            case "cm" => Distance(this.quantity - that.quantity / 100.0d)
        }
    }

    implicit class DoubleDistance(private val n: Double) {
        def m: Distance = Distance(n)
        def cm: Distance = Distance(n, "cm")
    }

    implicit class IntDistance(private val n: Int) {
        def m: Distance = n.toDouble m

        def cm: Distance = n.toDouble cm
    }

    def main(args: Array[String]): Unit = {
        val temM = 10 m
        val sevenCM = 7 cm

        println(temM)
        println(sevenCM)

        val sum = temM + sevenCM
        println(sum)

        val diff = temM - sevenCM
        println(diff)

    }
}
