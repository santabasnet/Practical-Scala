package com.semantro.learn

/**
  * This class is a part of the package com.semantro.learn and the package
  * is a part of the project learn.
  *
  * Integrated ICT Pvt. Ltd. Jwagal, Lalitpur, Nepal.
  * https://www.integratedict.com.np
  *
  * Created by Santa on 2021-07-18.
  */
object StateTransformation extends App {

    /**
      * Type Definition.
      */
    case class State[Id, Argument](run: Id => (Argument, Id))

    def greetingOf(name: String): State[Int, String] = State {
        count => (s"Hello $name", count + 1)
    }

    val pravin = greetingOf("Pravin")
    println(pravin.run(100))

}
