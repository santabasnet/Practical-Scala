package com.semantro.learn

import org.json4s.DefaultFormats
import org.json4s.jackson.JsonMethods

import scala.util.Try
import scala.xml.Elem

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-07-26.
  */
object FactoryMagic extends App {

    implicit val formats = DefaultFormats

    //Factory magic.
    //Product from xml and json with discount

    case class Product(data: Map[String, Any] = Map()) {
        def and(xml: Elem) = Product(this.data ++  Map(
            "price" -> (demoXML \\ "price").text,
            "quantity" -> (demoXML \\ "quantity").text,
            "unit" -> (demoXML \\ "unit").text,
            "currency" -> (demoXML \\ "currency").text
        ))
        def withDiscount(discount: Double) = Try {
            val d = this.data("quantity").toString.toDouble
            Product(this.data + ("quantity" -> ( d - (d * (discount/100.0)))))
        }getOrElse this
    }

    object Product {
        def from(json: String): Product = Try(Product(JsonMethods.parse(json).extract[Map[String, Any]])).getOrElse(Product.empty)
        def empty = Product()
    }

    val productWithDiscount = Product from demoJson and demoXML withDiscount 10.0
    println(productWithDiscount)

    def demoJson =
        """
          |"name": "Basmati Rice",
          |"category": "Kitchen"
        """.stripMargin

    def demoXML:Elem =
        <priceDefinition>
            <price>2500</price>
            <quantity>25</quantity>
            <unit>kg</unit>
            <currency>rs</currency>
        </priceDefinition>

}
