package com.semantro.learn

import java.util.concurrent.TimeUnit

import scala.concurrent.duration.{Duration, FiniteDuration}

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-07-20.
  */
object ImplicitWorks extends App {

    /**
      * Type conversion styles.
      */
    //implicit val optionDoubleToInt:Option[Double] => Option[Int] = (discount: Option[Double]) => discount.map(_.toInt)
    implicit def optionDoubleToInt(value: Option[Double]):Option[Int] = value.map(_.toInt)
    val x: Option[Double] = Some(100.0d)
    val y: Option[Int] = x

    case class MKS(distance: Option[Double] = None, quantity: Option[Double] = None, duration: Option[Double] = None)
    case class CGS(distance: Option[Double] = None, quantity: Option[Double] = None, duration: Option[Double] = None)
    implicit def mKSToCGS(value: MKS): CGS = CGS(
        distance = value.distance.map(_ * 100),
        quantity = value.quantity.map(_ * 1000),
        duration = value.duration
    )

    val earthVelocityInMKS: MKS = MKS(distance = Some(722222.2d), duration = Some(1)) //722222.2d m/s
    val earthVelocityInCGS: CGS = earthVelocityInMKS

    println(earthVelocityInMKS)
    println(earthVelocityInCGS)

    /**
      * Implicits: Extension styles.
      */
    implicit class DurationInt(private val n: Int) {
        def seconds: FiniteDuration = Duration(n.toLong, TimeUnit.SECONDS)
        def days: FiniteDuration = Duration(n.toLong, TimeUnit.DAYS)
    }

    println(5 seconds) // new Duration(5).seconds
    println(10 days)

    val y1 = 10 days // new Duration(10).days

    /**
      * Implicit parameter styles.
      */
    implicit val timeout: FiniteDuration = 4 seconds
    def workFor(n: BigInt)(implicit timeout: Duration): Unit = {
        //compute within timeout and send response otherwise respond with None
    }
    workFor(BigInt("100000000000000000000000000000000000"))


    type Config = String
    implicit val config:Config = "Gremlin"
    def executeQuery(query: String)(implicit config: Config) = ???

}
