package chapter2

trait TypeChecker[A] {
  def typeCheck(value: A): String
}

object TypeChecker {
  def apply[A](value: A)(implicit w: TypeChecker[A]): String = w.typeCheck(value)

  implicit class TypeCheckSyntax[A](value: A) {
    def typeCheck(implicit w: TypeChecker[A]): String = w.typeCheck(value)
  }

  implicit val stringTypeCheck: TypeChecker[String] =
    new TypeChecker[String] {
      def typeCheck(value: String): String = value match {
        case _: String => "This is String"
      }
  }

  implicit val intTypeCheck: TypeChecker[Int] =
    new TypeChecker[Int] {
      def typeCheck(value: Int): String = value match {
        case _: Int => "This is Int"
      }
    }

  def typeCheck[A](value: A): String = value match {
    case _: String => "This is String"
    case _: Int => "This is Int"
    case _: Unit => "This is Unit"
  }
}

object Test {
  def name: String = "This is name"
  val nameAsField: String = "Jack"
}

object Test7 {
  val simpleField = {
    println("Evaluating simpleField")
    42
  }
  def noParameterMethod = {
    println("Evaluating noParameterMethod")
    42
  }
}

object Calc {
  def square(value: Double): Double = value * value
  def cube(value: Double): Double = value * square(value)

  def square(value: Int): Int = value * value
  def cube(value: Int): Int = value * square(value)
}


object ExpressionsTypesValues extends App {
  assert(1 + 2 == 3)
  assert("3".toInt == 3)

  try {
    "foo".toInt
  } catch {
    case e: Exception => println(s"You can't cast 'foo' to Int. $e")
  }

  assert(("foo" take 1) == "f")
  assert(1.+(2).+(3) == 6)

  import TypeChecker._
  assert("Hello world".typeCheck == "This is String")
  assert(32.typeCheck == "This is Int")

  println(Test.name)
  println(Test.nameAsField)

  assert(Calc.square(10) == 100)
  assert(Calc.cube(10) == 1000)

  object Person {
    val fName: String = "Jack"
    val lName: String = "Black"
  }

  object Alien {
    def greet(p: Person.type): String = s"Hi, ${p.fName}"
  }

  println(Alien.greet(Person))
}

object ConditionalsAndBlocks {
  val cond1: String = if (1 > 2) "alien" else "predator"
  val cond2: Any = if (1 > 2) "alien" else 2010
  val cond3: Any = if(false) "hello"

}


