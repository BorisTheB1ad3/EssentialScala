package chapter3

//case class Cat(colour: String, food: String)

case class NewDirector(firstName: String, lastName: String, yearOfBirth: Int) {
  def name(): String = firstName + " " + lastName

  def older(d: NewDirector): NewDirector =
    if (d.yearOfBirth > this.yearOfBirth) this else d
}


case class NewFilm(name: String, yearOfRealese: Int, imdbRating: Double, director: NewDirector) {
  def highestRating(f: NewFilm): NewFilm =
    if (f.imdbRating > this.imdbRating) f else this
}

case class NewCounter(count: Int = 0) {
  def incr(value: Int): NewCounter = this.copy(count + value)
  def decrem(value: Int): NewCounter = this.copy(count - value)

  def incr = copy(count + 1)
  def decrem = copy(count - 1)
}

object Dad {
  def rate(f: NewFilm): Double = f match {
    case NewFilm(_, _, _, director) if director.name() == "Clint Eastwood" => 10.0
    case NewFilm(_, _, _, director) if director.name() == "John McTiernan" => 7.0
    case _ => 3.0
  }
}


object CaseClassesExercises extends App {
  val eastwood = NewDirector("Clint", "Eastwood", 1930)
  val mcTiernan = NewDirector("John", "McTiernan", 1951)
  val nolan = NewDirector("Christopher", "Nolan", 1970)

  val memento = NewFilm("Memento", 2000, 8.5, nolan)
  val darkKnight = NewFilm("Dark Knight", 2008, 9.0, nolan)
  val granTorino = NewFilm("Gran Torino", 2008, 8.2, eastwood)


  assert(eastwood.older(mcTiernan).name() == "Clint Eastwood")
  assert(darkKnight.highestRating(memento).imdbRating == 9.0)

  assert(NewCounter().incr(1).decrem(1).incr == NewCounter().incr)


  case class NewPerson(name: String, lname: String) {
    def fullName = name + " " + lname
  }

  object NewPerson {
    def apply(fullName: String): NewPerson = {
      val parts = fullName.split(" ")
      apply(parts(0), parts(1))
    }
  }

  val newPerson1 = NewPerson("Jack Black")
  val newPerson2 = NewPerson("Jack", "Black")

  assert(Dad.rate(darkKnight) == 3.0)
  assert(Dad.rate(granTorino) == 10.0)
}
