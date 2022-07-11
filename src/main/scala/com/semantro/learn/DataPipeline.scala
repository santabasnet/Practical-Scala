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
case class TransformError(error: String)
case class Person(firstName: String, middleName: String, lastName: String)
case class PhoneNumber(countryCode: Long, number: Long)

object PhoneNumber {
    private def toInt(number: String): Either[TransformError, Long] = Try(Some(number.toLong)).getOrElse(None) match {
        case Some(n) => Right(n)
        case None => Left(TransformError("Phone number has some errors in digits."))
    }

    def from(phoneString: String): Either[TransformError, PhoneNumber] = phoneString.split("-").filter(_.nonEmpty).toList match {
        case first :: second :: Nil => for {country <- toInt(first); number <- toInt(second)} yield PhoneNumber(country, number)
        case _ => Left(TransformError("Here, the " + phoneString + " is invalid phone number."))
    }
}

case class DomainUser(person: Person, phoneNumber: PhoneNumber)
object Source {
    def emit: Seq[RawUser] = RawData.generateRawUsers.map(RawUser.from)
}

case class RawUser(name: String, email: String, phone: String) {
    lazy val person: Either[TransformError, Person] = name.split(" ").map(_.trim).toList match {
        case firstName :: lastName :: Nil => Right(Person(firstName, "", lastName))
        case firstName :: middleName :: lastName :: Nil => Right(Person(firstName, middleName, lastName))
        case _ => Left(TransformError("The " + name + " has invalid name format."))
    }
}
object RawUser {
    def from(csv: String): RawUser = RawUser.from(csv.split(";").toList)
    def from(tokens: List[String]) = RawUser(tokens.head, tokens(1), tokens(2))
}

object Six {

    private def transform(rawUser: RawUser): Either[TransformError, DomainUser] = for {
        person <- rawUser.person
        phoneNumber <- PhoneNumber.from(rawUser.phone)
    } yield DomainUser(person, phoneNumber)

    def main(args: Array[String]): Unit = {
        val output = Source.emit.map(transform)
        output.foreach(println)

        println(output.head.isRight)
        println(output.last.isRight)
    }
}

object RawData {
    def generateRawUsers: Seq[String] = Seq[String](
        "Roth Drake;abc@gmail.com;977-9856233333",
        "Ram Sharan Karki;ram@gmail.com;977-9856233334",
        "Shakti Kumar;shakti@gmail.com;977-9856233335",
        "Rohit Mehara;rohit@gmail.com;977-9856233339",
        "Sita;rohit@gmail.com;9779856233339"
    )
}



