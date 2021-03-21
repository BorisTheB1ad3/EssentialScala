package chapter6

import scala.collection.immutable._

object Sequences extends App {
  println(List().headOption)
  assert(List().headOption.isEmpty)

  // append
  assert(List(1, 2, 3) :+ 4 == List(1, 2, 3, 4))

  // prepend
  assert(0 +: List(1, 2, 3) == List(0, 1, 2, 3))

  // concat sequences
  assert(List(1) ++ List(2) == List(1, 2))

  // Nil - empty list
  assert(List() == Nil)

  val list = 1 :: 2 :: 3 :: Nil // Like linked list
  val list1 = 1 :: List(2, 3)
  val list2 = List(2, 3) :+ 4

  assert(list2.headOption.nonEmpty)

  val animals = List("cat", "dog", "penguin")
  println(("mouse" :: animals) :+  "tyrannosaurus")
}

object MovieDb extends App {

  case class Film(name: String,
                  yearOfRelease: Int,
                  imdbRating: Double)

  case class Director(firstName: String,
                      lastName: String,
                      yearOfBirth: Int,
                      films: Seq[Film])

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

  def validateNumberOfFilms(dirs: Seq[Director], numberOfFilms: Int): List[Director] = {
    dirs.filter(_.films.size > numberOfFilms).toList
  }

  def olderThan(dirs: Seq[Director], year: Int): List[Director] = {
    dirs.filter(_.yearOfBirth < year).toList
  }

  def sortByYearOfBorn(dirs: Seq[Director], ascending: Boolean = true): List[Director] = {
    if (ascending) dirs.sortBy(_.yearOfBirth)(Ordering.fromLessThan(_ < _)).toList
    else dirs.sortBy(_.yearOfBirth)(Ordering.fromLessThan(_ > _)).toList
  }

  assert(validateNumberOfFilms(directors, 1).size == 3)
  assert(olderThan(directors, 1951).size == 1)
  println(sortByYearOfBorn(directors))

  val nolanFilms = nolan.films.map(_.name)
  val directorsFilms = directors.flatMap(_.films)

  println(nolanFilms)
  println(directorsFilms)

  println(mcTiernan.films.map(_.yearOfRelease).min)

  // sorted
  println(directors.flatMap(_.films).sortWith((a, b) => a.imdbRating > b.imdbRating))

  // avg rating
  println(directors.flatMap(_.films.map(_.imdbRating)).sum / directors.flatMap(_.films.map(_.imdbRating)).length)

  val films = directors.flatMap(_.films)
  println(films.foldLeft(0.0)((sum, film) => sum + film.imdbRating) / films.length)

  // !
  directors.foreach { director =>
    director.films.foreach { film =>
      println(s"Tonight only! ${film.name} by ${director.firstName} ${director.lastName}!")
    }
  }

  val x = directors.flatMap(
    directors => directors.films
  ).sortWith((a,b) => a.yearOfRelease < b.yearOfRelease).headOption

  println(x)
}
