package chapter7

import scala.math.Ordering

object TypeClasses extends App {
  /* Type classes are a powerful feature of Scala that allow us to extend exis􏰇ting
   libraries with new func􏰇tionality, without using inheritance
   and without having access to the original library source code. */

  /* Type classes are implemented with implicit */


  /* Example of type class instances */
  val minOrdering = Ordering.fromLessThan[Int](_ < _)
  val maxOrdering = Ordering.fromLessThan[Int](_ > _)

  List(4, 2, 3).sorted(minOrdering)
  List(4, 2, 3).sorted(maxOrdering)

  implicit val ordering: Ordering[Int] = Ordering.fromLessThan[Int](_ < _)
  assert(List(4, 2, 3).sorted == List(2, 3, 4))

  val absOrdering = Ordering.fromLessThan[Int](math.abs(_) < math.abs(_))
  assert(List(-4, -1, 0, 2, 3).sorted(absOrdering) == List(0, -1, 2, 3, -4))
  assert(List(-4, -3, -2, -1).sorted(absOrdering) == List(-1, -2, -3, -4))
}

object ImplicitOrdering extends App {
  implicit val absOrdering: Ordering[Int] = Ordering.fromLessThan[Int](math.abs(_) < math.abs(_))

  assert(List(-4, -1, 0, 2, 3).sorted == List(0, -1, 2, 3, -4))
  assert(List(-4, -3, -2, -1).sorted == List(-1, -2, -3, -4))
}

final case class Rational(numerator: Int, denominator: Int)

object Rational {
  implicit val rationalOrdering: Ordering[Rational] =
    Ordering.fromLessThan[Rational]((x,y) => {
      (x.numerator.toDouble / x.denominator.toDouble) < (y.numerator.toDouble / y.denominator.toDouble)
    })
}

object RationalSorted extends App {
  assert(List(Rational(1, 2), Rational(3, 4), Rational(1, 3)).sorted == List(Rational(1, 3), Rational(1, 2), Rational(3, 4)))
}

object OrderingByTotalPrice {
  implicit val orderingByTotalPrice: Ordering[Order] =
    Ordering.fromLessThan[Order]((x, y) => x.totalPrice > y.totalPrice)
}

object OrderByNumberOfUnits {
  implicit val orderingByNumOfUnits: Ordering[Order] =
    Ordering.fromLessThan[Order]((x, y) => x.units > y.units)
}

final case class Order(units: Int, unitPrice: Double) {
  val totalPrice: Double = units * unitPrice
}

object SortOrdersByTotalPrice extends App {
  import OrderingByTotalPrice._

  assert(List(Order(10, 3), Order(2, 10)).sorted == List(Order(10, 3), Order(2, 10)))
}

object SortByNumOfUnits extends App {
  import OrderByNumberOfUnits._

  assert(List(Order(10, 3), Order(25, 10)).sorted == List(Order(25, 10), Order(10, 3)))
}


/*
  There are four components of the type class pa􏰈ttern:
    1) the actual type class itself
    2) the type class instances
    3) interfaces using implicit parameters
    4) interfaces using enrichment and implicit parameters
*/
object CreatingTypeClasses extends App {

  case class Person(name: String, email: String)

  trait Equal[A] {
    def equal(a: A, b: A): Boolean
  }

  object ComparePersonByName extends Equal[Person] {
    def equal(a: Person, b: Person): Boolean = a.name == b.name
  }

  assert(ComparePersonByName.equal(Person("John", "111@email.ru"), Person("John", "222@email.ru")))

  object Eq {
    def apply[A](a: A, b: A)(implicit eq: Equal[A]): Boolean =
      eq.equal(a, b)
  }

  object CompareByNameImplicit {
    implicit object CompareByName extends Equal[Person] {
      def equal(a: Person, b: Person): Boolean = a.name == b.name
    }
  }

  def byName = {
    import CompareByNameImplicit._
    Eq(Person("John", "111@email.ru"), Person("John", "222@email.ru"))
  }

  assert(byName)
}


object TypeClassInterfacePattern extends App {

  case class Person(name: String, email: String)

  trait Equal[A] {
    def equal(a: A, b: A): Boolean
    def concat(a: A, b: A): String
  }

  object Equal {
    def apply[A](implicit eqInstace: Equal[A]): Equal[A] = eqInstace
  }

  object CompareByNameImplicit {
    implicit object CompareByName extends Equal[Person] {
      def equal(a: Person, b: Person): Boolean = a.name == b.name
      def concat(a: Person, b: Person): String = a.name + " " + b.name
    }
  }

  import CompareByNameImplicit._

  assert(Equal[Person].equal(Person("John", "111@email.ru"), Person("John", "222@email.ru")))
  println(Equal[Person].concat(Person("John", "111@email.ru"), Person("John", "222@email.ru")))

}

