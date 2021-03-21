package chapter6

import chapter6.MovieDb.{Director, Film, eastwood, mcTiernan, nolan, someGuy}

import scala.collection.immutable.Seq

object SequenceExercises extends App {
  def smallest(seq: Seq[Int]): Int = seq.foldLeft(Int.MaxValue)(math.min)
  def unique(seq: Seq[Int]): Seq[Int] = {
    def insert(seq: Seq[Int], elt: Int): Seq[Int] = if (seq.contains(elt)) seq else elt +: seq

    seq.foldLeft(Seq.empty[Int])(insert)
  }

  def reverseSeq(seq: Seq[Int]): Seq[Int] = seq.foldLeft(Seq.empty[Int])((seq, el) => el +: seq)
  def myMapFunc(seq: Seq[Int], f: Int => Int): Seq[Int] =
    seq.foldRight(Seq.empty[Int])((el, seq) => f(el) +: seq)


  val seq = List(1, 2, 3, 3, 5, 4, 5, 6)
  println(smallest(seq))
  println(unique(seq))
  println(reverseSeq(seq))
  println(myMapFunc(seq, _ * 2))

  // for comprehensions

  val forSeq = for {
    x <- Seq(1, 2, 3)
  } yield x * 2

  val forSeqTuple = for {
    x <- Seq((1, 2), (2, 4))
  } yield x._1 * 2

  assert(forSeq == Seq(2, 4, 6))
  assert(forSeqTuple == Seq(2, 4))

  val nestedSeq = Seq(Seq(1), Seq(1, 2), Seq(3, 2))
  val nestedSeqFor = for {
    subseq <- nestedSeq
    el <- subseq
  } yield el * 2

  assert(nestedSeq.flatMap(_.map(_ * 2)) == Seq(2, 2, 4, 6, 4))
  assert(nestedSeqFor == Seq(2, 2, 4, 6, 4))

  /*
  for {
    x <- a
    y <- b
    z <- c
  } yield e


  is like that:
  a.flatMap(x => b.flatMap(y => c.map(z => e)))
   */

  val exmpl = for (x <- forSeq) yield {
    val tmp = x * 2
    tmp * 10
  }

  assert(exmpl == Seq(40, 80, 120))

  val memento = Film("Memento", 2000, 8.5)
  val invictus = Film("Invictus", 2009, 7.4)
  val highPlainsDrifter = Film("High Plains Drifter", 1973, 7.7)
  val predator = Film("Predator", 1987, 7.9)
  val dieHard = Film("Die Hard", 1988, 8.3)
  val darkKnight = Film("Dark Knight", 2008, 9.0)

  val eastwood = Director("Clint", "Eastwood", 1930, Seq(highPlainsDrifter, invictus))
  val mcTiernan = Director("John", "McTiernan", 1951, Seq(predator, dieHard))
  val nolan = Director("Christopher", "Nolan", 1970, Seq(memento, darkKnight))

  val someGuy = Director("Just", "Some Guy", 1990,
    Seq())

  val directors = Seq(eastwood, mcTiernan, nolan, someGuy)

  val forNolanFilms = for {
    director <- directors
    film <- director.films if director == nolan
  } yield film.name

  val forAllFilms = for {
    director <- directors
    film <- director.films
  } yield film.name

  val forSortedFilmsByRating = (for {
    director <- directors
    film <- director.films
  } yield film) sortWith  { (a, b) =>
    a.imdbRating > b.imdbRating
  }

  println(forNolanFilms)
  println(forAllFilms)
  println(forSortedFilmsByRating)

  for {
    director <- directors
    film <- director.films
  } println(s"Tonight only! ${film.name} by ${director.firstName} ${director.lastName}!")
}
