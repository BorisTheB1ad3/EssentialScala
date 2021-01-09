package chapter4

object StructuralRecursion extends App {

  /* sum type pattern with polymorphism */
  sealed trait A {
    def foo: String
  }

  final case class B() extends A {
    def foo: String = "it's B"
  }

  final case class C() extends A {
    def foo: String = "it's C"
  }

  val anA: A = B()
  println(anA.foo) // B

  val anA1: A = C()
  println(anA1.foo) // C

  sealed trait Feline {
    def dinner: Food = this match {
      case Lion() => Antelope
      case Tiger() => TigerFood
      case Panther() => Licorice
      case Cat(favouriteFood) => CatFood(favouriteFood)
    }
  }

  final case class Lion() extends Feline
  final case class Tiger() extends Feline
  final case class Panther() extends Feline
  final case class Cat(favouriteFood: String) extends Feline

  sealed trait Food
  case object Antelope extends Food
  case object TigerFood extends Food
  case object Licorice extends Food
  final case class CatFood(food: String) extends Food


}
