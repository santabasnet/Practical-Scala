package com.semantro.learn

import shapeless._
import shapeless.syntax.std.traversable._
import shapeless.ops.traversable._

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


object HListDemo extends App {

    trait Parse[Out] {
        def apply(value: String): Option[Out]
    }

    object Parse {

        implicit object convertToBoolean extends Parse[Boolean] {
            def apply(value: String): Option[Boolean] = Some(value.toLowerCase == "true")
        }

        implicit object convertToInt extends Parse[Int] {
            def apply(value: String): Option[Int] = Try(value.toInt).toOption
        }

        implicit object convertToDouble extends Parse[Double] {
            def apply(value: String): Option[Double] = Try(value.toDouble).toOption
        }

        implicit object convertToString extends Parse[String] {
            def apply(value: String) = Some(value)
        }

    }

    trait ParseAll[Out] {
        type In <: HList

        def apply(values: In): Option[Out]
    }

    object ParseAll {

        type Aux[I, O] = ParseAll[O] {type In = I}

        implicit object convertHNil extends ParseAll[HNil] {
            type In = HNil

            def apply(value: HNil) = Some(HNil)
        }

        implicit def convertHList[T, HO <: HList](implicit cv: Parse[T], cl: ParseAll[HO]): ParseAll[T :: HO] {type In = String :: cl.In} =
            new ParseAll[T :: HO] {
                type In = String :: cl.In
                def apply(value: In): Option[T :: HO] = value match {
                    case x :: xs => for {
                        t <- cv(x)
                        h0 <- cl(xs)
                    } yield t :: h0
                }
            }
    }

    trait Converter {
        type Output
        def convert[S <: HList, H <: HList](values: List[String])(implicit
                                                                  gen: Generic.Aux[Output, H], // Compute HList representation `H` of Output
                                                                  parse: ParseAll.Aux[S, H], // Generate parser of Hlist of String `S` to HList `H`
                                                                  ft: FromTraversable[S] // Generate converter of `List[String]` to HList of Strings `S`
        ): Option[Output] = values.toHList[S].flatMap(parse.apply).map(gen.from)
    }


    object ConverterISD extends Converter {
        type Output = (Int, Boolean, Double, String)
    }

    val x = ConverterISD.convert(List("1", "true", "2.34", "foo"))

    println(x)

}
