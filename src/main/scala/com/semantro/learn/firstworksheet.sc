val a = 10
val x:Option[Int] = None
val y:Option[Int] = Some(600)

val result =  for {
    result1 <- x
    result2 <- y
} yield result1 + result2

