package chapter4

import chapter4.ModelingDataWithTraits.Calculation

object ModelingDataWithTraits extends App {
  // AND
  /* product type patter

  trait A {
    def b: B
    def c: C
  }

  case class A(b: B, c: C)

  A has a B and C
   */

  // OR
  /* sum type pattern

  sealed trait A
  final case class B() extends A
  final case class C() extends A


  A is a B or C
   */

  /* ADT - algebraic data type (any data that uses the above 2 patterns */

  sealed trait TrafficLight {
    def next: TrafficLight = this match {
      case Red => Green
      case Green => Yellow
      case Yellow => Red
    }
  }

  final case object Red extends TrafficLight
  final case object Green extends TrafficLight
  final case object Yellow extends TrafficLight


  sealed trait Calculation
  final case class Success(result: Int) extends Calculation
  final case class Fail(result: String) extends Calculation

  object Calculator {
    def +(calc: Calculation, x: Int): Calculation = calc match {
      case Success(res) => Success(res + x)
      case Fail(res) => Fail(res)
    }

    def -(calc: Calculation, x: Int): Calculation = calc match {
      case Success(res) => Success(res - x)
      case Fail(res) => Fail(res)
    }

    def /(calc: Calculation, x: Int): Calculation = calc match {
      case Success(res) if x == 0 => Fail("Division by zero")
      case Success(res) if x != 0 => Success(res / x)
      case Fail(res) => Fail(res)
    }
  }

  assert(Calculator.+(Success(1), 2) == Success(3))
  assert(Calculator.-(Success(1), 1) == Success(0))
  assert(Calculator.+(Fail("Badness"), 1) == Fail("Badness"))

  assert(Calculator./(Success(4), 2) == Success(2))
  assert(Calculator./(Success(4), 0) == Fail("Division by zero"))
  assert(Calculator./(Fail("Badness"), 0) == Fail("Badness"))

  sealed trait Source
  final case object WellWater extends Source
  final case object SpringWater extends Source
  final case object TapWater extends Source

  final case class BottledWater(size: Int, source: Source, carbonated: Boolean)

  println(Red.next.toString)
}
