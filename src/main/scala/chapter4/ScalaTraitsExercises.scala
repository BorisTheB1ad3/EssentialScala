package chapter4


trait Feline {
  def colour: String
  def sound: String
}

trait BigCat extends Feline {
  override val sound = "roar"
}

case class DomesticCat(colour: String, favouriteFood: String) extends Feline {
  override val sound: String = "meow"
}

case class Tiger(colour: String) extends BigCat
case class Lion(colour: String, maneSize: Int) extends BigCat
case class Panther(colour: String) extends BigCat

/* Shaping with traits */

sealed trait Shape {
  def sides: Int
  def perimeter: Double
  def area: Double
}

sealed trait Rectangular extends Shape {
  def width: Double
  def height: Double
  val sides = 4
  override def perimeter = 2 * width + 2 * height
  override def area = width*height
}

final case class Circle(radius: Double) extends Shape {
  val sides: Int = 1
  val perimeter: Double = 2 * Math.PI * radius
  val area: Double = Math.PI * radius * radius
}

final case class Rectangle(width: Double, height: Double) extends Rectangular

final case class Square(size: Double) extends Rectangular {
  val width = size
  val height = size
}

object Draw {
  def apply(s: Shape): String = s match {
    case Circle(radius) => s"A circle of radius $radius cm"
    case Rectangle(width, height) => s"A rectangle of width ${width}m and height ${height}cm"
    case Square(size) => s"A square of size of $size"
  }
}

sealed trait DivisionResult

final case class Finite(value: Int) extends DivisionResult
case object Infinite extends DivisionResult

object Divide {
  def apply(num: Int, den: Int): DivisionResult =
    if (den == 0) Infinite else Finite(num / den)

  def show(num: Int, den: Int): String = apply(num, den) match {
    case Finite(value) => s"This is finite $value"
    case Infinite => "This is infinite"
  }
}

object ScalaTraitsExercises extends App {
  val c = Circle(2)
  println(c.area)
  println(c.perimeter)

  val s = Square(7)
  println(s.area)
  println(s.perimeter)

  val r = Rectangle(10, 4)
  println(r.perimeter)
  println(r.area)

  println(Draw(Circle(10)))
  println(Divide.show(10, 0))
  println(Divide.show(10, 4))
}
