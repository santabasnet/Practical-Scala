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


/**
  * Spelling checking of 80's
  */
object Spell80s extends App {

    private val BEGIN = 0
    private val EMPTY = ""
    private val COMMA = ','
    private val SPACE = ' '

    case class Word(text: String, from: Int, to: Int) {
        def withChar(ch: Char, from: Int, to: Int) = Word(this.text + ch, from, to)
        def nonEmpty: Boolean = text.nonEmpty
        override def toString = f"{$text: ($from, $to)}"
    }

    object Word {
        def empty = Word(Spell80s.EMPTY, 0, 0)
        def of(text: String, from: Int, to: Int) = Word(text: String, from: Int, to: Int)
    }

    val delimiter = List(COMMA, SPACE)
    val dictionary = List("hello", "from", "the", "world")
    val sourceText = "Hello   from the   Semantro world,   Nepal"

    val wrongWords = sourceText.foldLeft((0, List[Word](Word.empty)))((result, input) => {
        val lastWord = result._2.last
        if (delimiter.contains(input)) (result._1 + 1, result._2 :+ Word.of(EMPTY, result._1, result._1 + 1))
        else (result._1 + 1, result._2.dropRight(1) :+ lastWord.withChar(input, lastWord.from, result._1))
    })._2.filter(_.nonEmpty).filterNot(word => dictionary.contains(word.text.toLowerCase))

    println("Wrong Words: ")
    wrongWords.foreach(println)

}

