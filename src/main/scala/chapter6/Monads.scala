package chapter6

import scala.util.Try

object Monads extends App {

  val opt1 = Some(1)
  val opt2 = Some(2)
  val opt3 = Some(3)

  val seq1: Seq[Int] = Seq(1)
  val seq2: Seq[Int] = Seq(2)
  val seq3: Seq[Int] = Seq(3)

  val try1 = Try(1)
  val try2 = Try(2)
  val try3 = Try(3)

  val sumOfOptions: Option[Int] = for {
    val1 <- opt1
    val2 <- opt2
    val3 <- opt3
  } yield  val1 + val2 + val3

  val seqSum: Seq[Int] = for {
    a <- seq1
    b <- seq2
    c <- seq3
  } yield a + b + c

  val sumTry: Try[Int] = for {
    val1 <- try1
    val2 <- try2
    val3 <- try3
  } yield  val1 + val2 + val3


  assert(sumOfOptions.contains(6))
  assert(seqSum == List(6))
  assert(sumTry == Try(6))


  val testParallelIteration = for {
    x <- Seq(1, 2, 3)
    y <- Seq(4, 5, 6)
  } yield x + y

  val testZip = Seq(1, 2, 3).zip(Seq(4, 5, 6))
  val forWithZip = for(x <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield { val (a, b) = x; a + b }
  val betterForWithZip = for((a,b) <- Seq(1, 2, 3).zip(Seq(4, 5, 6))) yield a + b

  println(testParallelIteration)
  println(testZip)
  assert(forWithZip == betterForWithZip)

  val forWithIntermediateResults = for {
    x <- Seq(1, 2, 3)
    square = x * x
    y <- Seq(2)
  } yield square * y

  assert(forWithIntermediateResults == Seq(2, 8, 18))

}
