package chapter3

class Cat(name: String, colour: String, val food: String)

object Director {
  def apply(firstName: String, lastName: String, yearOfBirth: Int): Director =
    new Director(firstName, lastName, yearOfBirth)

  def older(d1: Director, d2: Director): Director =
    if (d1.yearOfBirth > d2.yearOfBirth) d2 else d1
}

class Director(firstName: String, lastName: String, val yearOfBirth: Int) {
  def name(): String = firstName + " " + lastName
}

object Film {
  def apply(name: String, yearOfRealese: Int, imdbRating: Double, director: Director): Film =
    new Film(name, yearOfRealese, imdbRating, director)

  def highestRating(f1: Film, f2: Film): Film =
    if (f1.imdbRating > f2.imdbRating) f1 else f2

  def oldestDirectorAtTheTime(f1: Film, f2: Film): Director =
    Director.older(f1.director, f2.director)
}

class Film(val name: String, yearOfRealese: Int, val imdbRating: Double, val director: Director) {
  def directorsAge(): Int = director.yearOfBirth
  def isDirectedBy(director: Director) =
    this.director.equals(director)
  def copy(name: String = "Green elephant",
           yearOfRealese: Int = 2000,
           imdbRating: Double = 9.0,
           director: Director = new Director("test", "testovich", 2000)): Film =
    new Film(name, yearOfRealese, imdbRating, director)
}

object ChipShop {
  def willServe(cat: Cat): Boolean = cat.food == "Chips"
}

object ObjectsAndClasses extends App {

  object Somebody {
    def greetSomebody(p :Person): Unit = println(s"Hi, ${p.fullName()}")
  }

  class Person(val name: String = "Jack", val secondName: String) {
    def fullName(): String = name + " " + secondName
  }

  val newJackBlack = new Person("Jack", "Black")
  assert(newJackBlack.fullName() == "Jack Black")
  assert(newJackBlack.name == "Jack")

  Somebody.greetSomebody(newJackBlack)

  val firstCat = new Cat("Oswald", "Black", "Milk")
  val anotherCat = new Cat("Henderson", "Ginger", "Chips")
  val oneMoreCat = new Cat("Quentin", "Tabby and white", "Curry")

  assert(ChipShop.willServe(anotherCat))
  assert(!ChipShop.willServe(firstCat))
}

object FilmExercise extends App {
  /* List of Directors */
  val eastwood = new Director("Clint", "Eastwood", 1930)
  val mcTiernan = new Director("John", "McTiernan", 1951)
  val nolan = new Director("Christopher", "Nolan", 1970)
  val someBody = new Director("Just", "Some Body", 1990)

  /* List of films */
  val memento = new Film("Memento", 2000, 8.5, nolan)
  val darkKnight = new Film("Dark Knight", 2008, 9.0, nolan)
  val inception = new Film("Inception", 2010, 8.8, nolan)
  val highPlainsDrifter = new Film("High Plains Drifter", 1973, 7.7,
    eastwood)
  val outlawJoseyWales = new Film("The Outlaw Josey Wales", 1976, 7.9,
    eastwood)
  val unforgiven = new Film("Unforgiven", 1992, 8.3, eastwood)
  val granTorino = new Film("Gran Torino", 2008, 8.2, eastwood)
  val invictus = new Film("Invictus", 2009, 7.4, eastwood)
  val predator = new Film("Predator", 1987, 7.9, mcTiernan)
  val dieHard = new Film("Die Hard", 1988, 8.3, mcTiernan)
  val huntForRedOctober = new Film("The Hunt for Red October", 1990,
    7.6, mcTiernan)
  val thomasCrownAffair = new Film("The Thomas Crown Affair", 1999, 6.8, mcTiernan)

  assert(eastwood.yearOfBirth == 1930)
  assert(dieHard.director.name == "John McTiernan")
  assert(!invictus.isDirectedBy(nolan))

  val newFilmOne = highPlainsDrifter.copy(name = "L'homme des hautes plains")

  println(newFilmOne.name, newFilmOne.director.name())

  assert(Director.older(nolan, eastwood).name() == "Clint Eastwood")
  assert(Film.highestRating(dieHard, invictus).name == "Die Hard")
  assert(Film.oldestDirectorAtTheTime(dieHard, invictus).name() == "Clint Eastwood")
}

object Counter extends App {

  class Counter(val value: Int) {

    def inc(x: Int = 1): Counter = new Counter(value + x)
    def dec(x: Int = 1): Counter = new Counter(value - x)

    def inc: Counter = inc()
    def dec: Counter = dec()

    def count: Int = value

    def adjust(adder: Adder): Counter = new Counter(adder(count))
  }

  class Adder(amount: Int) {
    def apply(in: Int): Int = in + amount
  }

  val newAdder = new Adder(10)

  val newCounter: Int = new Counter(10).inc.dec.dec(7).inc(10).adjust(newAdder).count
  println(newCounter)

}

class Timestamp(val seconds: Long)

/* Companion object of Timestamp class */
object Timestamp {
  def apply(hours: Int, minutes: Int, seconds: Int): Timestamp =
    new Timestamp(hours * 60 * 60 + minutes * 60 + seconds)
}

object TimestampTest extends App {
  println(Timestamp(1, 1, 1).seconds)
}

class Person(val fname: String, val lname: String)

object Person {
  def apply(fullName: String): Person = {
    val fullNameAsArray = fullName.split("\\s")
    new Person(fullNameAsArray(0), fullNameAsArray(1))
  }
}

object PersonCompanionTest extends App {
  val p1 = Person("Jack Black")
  println(p1.fname, p1.lname)
}

case class NewPerson(name: String, age: Int) {
  def printSmth(): String = name + " " + age.toString
}

object NewPersonTest extends App {
  println(NewPerson("test", 1).toString) // class
  println(NewPerson("test", 1).printSmth()) // like object

  assert(NewPerson("test", 1).equals(NewPerson("test", 1)))

  println(NewPerson("test", 1).copy("test2").toString)
}

case object Citizen {
  def firstName = "Jack"
}

object CitizenTest extends App {
  println(Citizen.toString)
}

sealed trait Message
case class StartSpeakingMessage(textToSpeak: String) extends Message
case object StopSpeakingMessage extends Message
case object PauseSpeakingMessage extends Message
case object ResumeSpeakingMessage extends Message

object CaseObjectTest extends App {

  def receive(m: Message): Unit = m match {
    case StartSpeakingMessage(txt) => println(txt)
    case StopSpeakingMessage => println("stop speaking")
    case PauseSpeakingMessage => println("pause conversation")
    case ResumeSpeakingMessage => println("resume conversation")
  }

  receive(StopSpeakingMessage)
}