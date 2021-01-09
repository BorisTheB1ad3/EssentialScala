package chapter3

case class Man(firstName: String, lastName: String)

object PatternMatching extends App {
  def inspect(person: Man): String = person match {
    case Man("Luke", "Skywalker") => "Stop, rebel scum!"
    case Man(first, last) => s"Move along, $first"
  }

  val lukeSkywalker = Man("Luke", "Skywalker")

  println(inspect(lukeSkywalker))
}

case class Kitten(name: String, colour: String, food: String)

object NewChipShop {
  def willServe(c: Kitten): Boolean = c match {
    case Kitten(_, _, "chips") => true
    case Kitten(_, _, _) => false
  }
}


object TestObject extends App {
  val catNumberOne = Kitten("Jersey", "yellow", "chips")
  val catNumberTwo = Kitten("Jacky", "brown", "cheese")

  println(NewChipShop.willServe(catNumberOne))
  println(NewChipShop.willServe(catNumberTwo))
}
